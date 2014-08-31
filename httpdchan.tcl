# httpdchan.tcl - a transforming channels for Tcl to generate headers.

Debug define httpdchan

namespace eval httpdchan {
    variable max_header [expr {20 * 1024}]	;# no http header may be more than this long

    # Headers - split request/response line into header -Headers fields
    # turn rest of header into dict
    proc Headers {header {eol \n}} {
	set header [regsub -all "$eol\[\t \]+" $header " "]	;# strip continuation lines
	set header [split $header $eol]	;# break up into a list of lines
	
	# assemble headers into a dict
	set headers {}
	
	foreach element [lassign $header firstline] {
	    set value [string trim [join [lassign [split $element :] key] :]]
	    set key [string tolower [string trim $key]]
	    if {[dict exists $headers $key]} {
		# turn multiply occurring keys into a list of values
		if {![dict exists $headers -Headers multiple $key]} {
		    dict set headers $key [list [dict get $headers $key]]	;# first duplicate
		}
		
		dict lappend headers $key $value
		dict set headers -Headers multiple $key [llength [dict get $headers $key]]
	    } else {
		dict set headers $key $value
	    }
	}

	dict set headers -Headers full $firstline	;# record the first line as request/status line
	set full $firstline
	if {[string match HTTP/* $full]} {
	    # Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
	    dict set headers -Headers reason [join [lassign [split [dict get $headers -Headers full]] version status]]
	    dict set headers -Headers status $status
	} else {
	    # Request-Line = Method SP Request-URI SP HTTP-Version CRLF
	    foreach name {method uri version} value [split $full] {
		set $name $value
		dict set headers -Headers $name $value
	    }
	}
	dict set headers -Headers version [lindex [split $version /] end]

	return $headers
    }

    # finalize - 
    proc finalize {handle} {
	upvar #0 [namespace current]::$handle H
	Debug.httpdchan {finalize $handle [array get H]}
	unset H
    }

    # initialize - 
    proc initialize {handle mode} {
	upvar #0 [namespace current]::$handle H
	set H(rw) $mode
	set H(state) 0	;# header mode
	set H(offset) 1	;# one after the byte to start searching
	Debug.httpdchan {initialize $handle [array get H]}
	return [list drain read write finalize initialize]
    }

    # drain - 
    # This optional subcommand is called whenever data in the transformation input (i.e. read) buffer has to be forced
    # upward, i.e. towards the user or script. The result returned by the method is taken as the binary data to push upward
    # to the level above this transformation (the reader or a higher-level transformation).
    proc drain {handle} {
	upvar #0 [namespace current]::$handle H

	if {$H(state) == 0} {
	    error "Can't drain in HEADER state"
	}

	if {![info exists H(buffer)]} {
	    Debug.httpdchan {drain! $handle [array get H] - nothing to drain}
	    return result ""
	}

	set result $H(buffer); unset H(buffer)
	Debug.httpdchan {draining [string length $result]}
	return $result
    }

    # read - called whenever the base channel, or a transformation below this transformation, pushes data upward.
    #
    # $buffer contains the binary data which has been given to us from below.
    # It is the responsibility of this subcommand to actually transform the data.
    # The result is taken as the binary data to push further upward to the transformation above this transformation.
    # 
    # Note that the result is allowed to be empty, or even less than the data we received; the transformation is not required
    # to transform everything given to it right now. It is allowed to store incoming data in internal buffers and to defer
    # the actual transformation until it has more data.
    proc read {handle buffer} {
	upvar #0 [namespace current]::$handle H
	switch -- $H(state) {
	    0 {
		Debug.httpdchan {read state $H(state): adding [string length $buffer]}
		append H(buffer) $buffer
		set end [string first \n\n [string range $H(buffer) $H(offset)-1 end]]
		if {$end == -1} {
		    set H(offset) [string length $H(buffer)]
		    variable max_header
		    if {$H(offset) > $max_header} {
			error "$handle - header longer than $max_header"
		    }
		    return ""	;# haven't seen end of headers yet, keep reading
		}

		Debug.httpdchan {EOH $end}
		
		# got a complete header set - transform the buffered data
		set end [expr {$H(offset) + $end}]; unset H(offset)
		set header [string range $H(buffer) 0 $end-2]
		set H(buffer) [string range $H(buffer) $end+2 end]
		incr H(state)
		if {$H(buffer) eq ""} {
		    incr H(state)
		}
		return [string map {\n \x80} $header]\n	;# return the headers as a single line
	    }

	    1 {
		# we're in the pass-through state with some trailing buffer to clear
		Debug.httpdchan {read state $H(state): pass thru [string length $buffer]}
		set buffer $H(buffer)$buffer; unset H(buffer)
		Debug.httpdchan {read state $H(state): cleared buffer: [string length $buffer] '[string range $buffer 0 60]...'}
		incr H(state)
		return $buffer
	    }

	    2 {
		Debug.httpdchan {read state $H(state): pass thru [string length $buffer] '[string range $buffer 0 60]...'}
		return $buffer
	    }

	    default {
		error "Bad state $H(state)"
	    }
	}	    
    }

    # write - called whenever a transformation above this transformation writes data downward.
    # buffer contains the binary data which has been written to us.
    # It is the responsibility of this subcommand to actually transform the data.
    # The result returned by the subcommand is taken as the binary data to write to the transformation below this transformation.
    # Note that the result is allowed to be empty, or less than the data we got;
    # the transformation is not required to transform everything which was written to it right now. It is allowed to store
    # this data in internal buffers and to defer the actual transformation until it has more data.
    proc write {handle buffer} {
	upvar #0 [namespace current]::$handle H
	Debug.httpdchan {write $handle [string length $buffer]}
	return $buffer
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && $argv0 eq [info script]} {
    #set socket [socket google.com 80]
    #chan configure $socket -blocking 0
    set socket stdin

    set s1 [chan push $socket httpdchan]
    set header [gets $s1]
    puts stderr HEADER:[httpdchan Header $header \x80]
    set data [read $s1]
    puts stderr "READ: [string length $data] '[string range $data 0 30]...[string range $data end-30 end]'" 
    chan pop $s1
}
