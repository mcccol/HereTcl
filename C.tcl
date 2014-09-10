# Hclient - H's http client
Debug define client
#Debug on httpd
#Debug on httpdlow

# CxSend - process a single reply
proc CxSend {socket method url args} {
    if {[llength $args]%2} {
	set content [lindex $args end]
	set args [lrange $args 0 end-1]
    }
    set h $args	;# h is args as a dict
    corovar Host; dict set headers host $Host

    ### start sending header down the socket
    TxLine $socket [string toupper $method] $url HTTP/1.1
    
    # set up entity transmission header elements
    if {[info exists content]} {
	# we have all the content, may as well gzip it if required
	switch -- [dict get? $h content-encoding] {
	    gzip {
		# gzip it right now - prepend a minimal gzip file header:
		# signature, deflate compression, no flags, mtime,
		# xfl=0, os=3
		set gzheader [list crc 0 time [clock seconds] type [expr {[string match text/* [dict get? $reply content-type]]?"text":"binary"}]]
		set gzheader [dict merge $gzheader [dict get? $h -gzheader]]
		set content [::zlib gzip $content -header $gzheader]
	    }
	    identity - "" {
		# do nothing for identity
	    }
	    default {
		error "Specified [dict get? $headers content-encoding] content-encoding, we only know gzip"
	    }
	}

	dict set headers content-length [string length $content] ;# set correct content-length
    }

    # convert headers to a list
    foreach {n v} $args {
	if {![dict exists $headers $n]} {
	    lappend headers $n $v
	}
    }

    TxHeaders $socket $headers 	;# emit $reply's headers

    if {[info exists content]} {
	# content exists - we have the full content ready to go - just send it
	# send literal content entire
	chan puts -nonewline $socket $content	;# send the content in its entirety
    }

    # associate callback with reception
    corovar sent
    corovar callbacks
    if {[dict exists $h -callback]} {
	dict set callbacks [incr send] [dict get $h -callback]
    } else {
	corovar callback; dict set callbacks [incr send] $callback
    }
}

proc CxGOT {r} {
    set entity [dict get? $r -entity]
    catch {dict unset r -entity}
    puts stderr "GOT: ($r) with entity '[string range $entity 0 10]...[string range $entity end-10 end]'"
}

variable cx_defaults {
    timeout 30000
}

proc Cx {args} {
    # all of these variables become corovars
    variable cx_defaults
    set callback CxGOT
    set args [dict merge $cx_defaults $args]
    dict with args {}	;# install rx state vars

    set connected 0
    set sent 0		;# how many packets have we sent?
    set pending {}	;# requests queued before connection
    set callbacks {}	;# request callbacks pending responses

    try {
	while {[chan pending output $socket] != -1} {
	    set rest [lassign [::yieldm $sent] op]			;# wait for READ event
	    Debug.client {CX [info coroutine] got op $op}

	    switch -- $op {
		connected {
		    # we have established an async connection
		    after cancel $timer
		    chan configure $socket -blocking 0 -encoding binary -translation binary -buffering none
		    chan event $socket writable {}				;# turn off the writable event now
		    chan event $socket readable $rx	;# trigger us whenever input becomes available
		    incr connected
		    foreach el $pending {
			set rest [lassign $el method url]
			CxSend $socket $method $url {*}$args		;# send anything which is pending connection
		    }
		}

		conn_fail {
		    # we have failed to get a connection
		    if {!$connected} {
			Debug.client {[info coroutine] Failed connection}
		    } else {
			Debug.error {[info coroutine] Spurious connection timeout - we are already connected.}
		    }
		}

		closing {
		    Debug.client {[info coroutine] Rx closing $rest}
		}
		
		continue {
		    # Rx indicating it needs a 100-Continue sent
		    # special case 100-Continue - straight out the socket
		    Debug.httpdtx {[info coroutine] Cx sending 100-Continue}
		    TxLine $socket "HTTP/1.1 100 Continue"
		    TxLine $socket ""
		}

		send {
		    # applicaton has asked to send a request - send it out or queue it up
		    if {$connected} {
			CxSend $socket {*}$rest
		    } else {
			lappend pending $rest
		    }
		}
		
		pending {
		    # Rx has received the start of a command
		    Debug.client {[info coroutine] Cx Pending $rest}
		}
		
		reply {
		    set rest [lassign $rest r]
		    Debug.client {[info coroutine] reply ($r) / ($callbacks)}
		    {*}[dict get $callbacks [dict get $r -transaction]] $r
		    dict unset callbacks [dict get $r -transaction]
		}
		
		timeout {
		    # interpacket timeout
		}
		
		idle {
		    # idle timeout
		}
		
		default {
		    error "[info coroutine] yielded $op $rest"
		}
	    }
	}
    } on error {e eo} {
	Debug.error {[info coroutine] Tx $socket ERROR '$e' ($eo)}
    } finally {
	catch {chan close $socket write}
    }
}

proc CxCheck {r} {
    Debug.httpdlow {CxCheck $r}
    # rfc2616 14.10:
    # A system receiving an HTTP/1.0 (or lower-version) message that
    # includes a Connection header MUST, for each connection-token
    # in this field, remove and ignore any header field(s) from the
    # message with the same name as the connection-token.
    #### I have no idea what this is for
    set version [dict get $r -Header version]
    if {$version < 1.1 && [dict exists $r connection]} {
	foreach token [split [dict get $r connection] ","] {
	    catch {dict unset r [string tolower [string trim $token]]}
	}
	dict unset r connection
    }

    dict set r -Header state CxCheck
    Debug.httpdlow {CxCheck done: $r}
    return $r
}

# CxProcess - default handling of packet reception for clients
proc CxProcess {R} {
    set R [Header $R 1]		;# fetch request/status line
    set R [RxHeaders $R]	;# fetch all remaining headers
    set R [CxCheck $R]		;# parse $headers as a complete request header
    set R [RxEntity $R]		;# fetch any entity
    return $R			;# return completed request
}



# speak - connect outward to a host, port
proc speak {args} {
    if {[llength $args]%2} {
	set host [lindex $args end]
	set args [lrange $args 0 end-1]	;# port tagged on the end of args
    } elseif {[dict exists $args host]} {
	set host [dict get $args host]	;# passed in host
    }
    if {![info exists host]} {
	error "Must specify host or host:port to speak"
    }
    set Host $host
    # see if port's been specified as host:port
    lassign [split $host :] host port
    if {$port eq ""} {
	if {[dict exists $args port]} {
	    set port [dict get $args port]
	} else {
	    set port 80
	}
    } else {
	append Host :$port
    }
    lappend args Host $Host

    # get extra args for [socket]
    set extra {}
    if {[dict exists $args -myaddr]} {
	lappend extra [list -myaddr [dict get $args -myaddr]]
    }
    if {[dict exists $args -myport]} {
	lappend extra [list -myport [dict get $args -myport]]
    }

    try {
	set socket [socket -async {*}$extra $host $port]
    } trap NONE {e eo} {
	lassign [split $e :] e1 e2
	if {[string trim $e2] eq "Name or service not known"} {
	    error "Unknown host '$host'"
	} else {
	    #puts "MOOP $e ($eo)"
	    return -options $eo $e
	}
    } on error {e eo} {
	puts stderr "speak connection error $e ($eo)"
	return -options $eo $e
    }

    set namespace [namespace current]
    set Cx ${namespace}::C::$socket 
    set Rx ${namespace}::R::$socket
    if {[dict exists $args timeout]} {
	set timeout [dict get $args timeout]
    } else {
	variable cx_defaults; set timeout [dict get $cx_defaults timeout]
    }

    set timer [after $timeout [list $Cx conn_fail]]

    set args [dict merge $args [list port $port host $host socket $socket]]
    ::coroutine $Cx $namespace Cx {*}$args rx $Rx timer $timer	;# create Rx coro around H::Rx command
    ::coroutine $Rx $namespace Rx rxprocess CxProcess dispatch [list $Cx reply] {*}$args tx $Cx	;# create coro for client

    chan event $socket writable [list $Cx connected]	;# await connection

    return $Cx
}

namespace eval C {
    namespace import [namespace parent]::*
}
