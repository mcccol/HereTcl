# Hrx.tcl - light Httpd 1.1 rx component

# Tmpfile - create a temporary file with appropriate encodings
proc Tmpfile {R} {
    # create a temp file to contain entity
    corovar entitypath	;# path in which to create entities
    set entity [file tempfile $entitypath]

    chan configure $entity -translation binary	;# store it as we get it

    # prepare output file for receiving chunks
    corovar te
    if {[info exists te] && "gzip" in [dict keys $te]} {
	::zlib push inflate $entity	;# inflate it on the fly
	chan configure $entity -translation binary -encoding binary
    }

    return $entity
}

# Readable - set socket's readable event
proc Readable {socket args} {
    if {[llength $args]} {
	lappend args "" [lindex [info level -1] 0]
    }
    return [chan event $socket readable $args]
}

# CharEncoding - determine the charset of any content
proc CharEncoding {r} {
    # decode the content-type ... FIXME - I bet there's more decoding to be done
    set charset [join [lassign [split [dict get $r content-type] \;] ctype] \;]

    if {[string match "*charset=*" $charset]} {
	# there's a charset in there somewhere
	set charset [string trim [lindex [split [string tolower $charset] =] 1]]
    } elseif {$ctype eq "text"} {
	# no charset defined by the request, use default
	corovar def_charset
	return $def_charset
    } else {
	return binary	;# best guess - no charset
    }

    # client specified both ctype and charset - do we know that charset?
    set charset [string tolower $charset]
    if {[string match "iso-*" $charset]} {
	set charset [string map {iso- iso} $charset]
    }
    if {$charset ni [encoding names]} {
	# send NotAcceptable
	corovar tx; tailcall $tx reply [dict merge $r {-code 406}]
    }

    return $charset	;# return the encoding we've selected
}

# HeaderCheck - given a request dict $r, perform semantic checks and adjustments
proc HeaderCheck {r} {
    Debug.httpdlow {HeaderCheck $r}
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

    set headers [split [dict get $r -Header full]]
    set uri [join [lrange $headers 1 end-1]]
    dict set r -Header uri $uri

    # fill -Url
    if {[dict exists $r host]} {
	# client sent Host: field
	if {[string match http*:* $uri]} {
	    # absolute Host: field
	    # rfc 5.2 1 - a host header field must be ignored
	    # if request-line specified an absolute URL host/port
	    dict set r -Url [dict merge [dict get $r -Url] [parse_url $uri]]
	} else {
	    # no absolute URL was specified on the request-line
	    # use the Host field to determine the host
	    lassign [split [dict get $r host] :] h p
	    dict set r -Url host $h
	    if {$p eq ""} {
		corovar port
		dict set r -Url port $port
	    } else {
		dict set r -Url port $p
	    }
	    dict set r -Url [dict merge [dict get $r -Url] [path $uri]]
	}
    } elseif {$version > 1.0} {
	Debug.httpdlow {Host field required: $r}
	Bad $r "HTTP 1.1 required to send Host"
    } else {
	# HTTP 1.0 isn't required to send a Host field
	# but we still need host info as provided by Listener
	dict set r -Url [dict merge [dict get $r -Url] [path $uri]]
	dict set r -Url host [host [dict get $r -Url]]
    }

    if {0} {
	#### could be done where it's used, if it's used
	# remove 'netscape extension' length= from if-modified-since
	if {[dict exists $r if-modified-since]} {
	    dict set r if-modified-since [lindex [split [dict get $r if-modified-since] {;}] 0]
	}
    }

    dict set r -Header state HeaderCheck
    Debug.httpdlow {HeaderCheck done: $r}
    return $r
}

# ParseRQ - unpack request/status line into its fields
proc ParseRQ {R line} {
    dict set R -Header full $line	;# record the first line as request/status line
    if {[string match HTTP/* $line]} {
	# Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
	dict set R -Header reason [join [lassign [split [dict get $R -Header full]] version status]]
	dict set R -Header status $status
    } else {
	# Request-Line = Method SP Request-URI SP HTTP-Version CRLF
	foreach name {method uri version} value [split $line] {
	    set $name $value
	    dict set R -Header $name $value
	}
    }

    dict set R -Header version [lindex [split $version /] end]
}

# Parse - split request/response line into header -Header fields
# turn rest of header into dict
# Parse - given a set of header lines, parse them and populate the request dict
proc Parse {R} {
    set lines [regsub -all {\n[\t ]+} [dict get $R -Full] " "]	;# merge continuation lines
    dict unset R -Full
    Debug.httpdlow {Parse: $lines}

    set clientheaders {}

    # assemble headers into the R
    foreach element [lassign [split $lines \n] firstline] {
	set value [string trim [join [lassign [split $element :] key] :]]
	set key [string tolower [string trim $key]]
	if {$key eq ""} continue
	if {[dict exists $R $key]} {
	    # turn multiply occurring keys into a list of values
	    if {![dict exists $R -Header multiple $key]} {
		dict set R $key [list [dict get $R $key]]	;# first duplicate
	    }

	    dict lappend R $key $value
	    dict set R -Header multiple $key [llength [dict get $R $key]]
	} else {
	    dict set R $key $value
	    lappend clientheaders $key	 ;# keep list of headers passed in by client
	}
    }

    set R [ParseRQ $R $firstline]	;# fill in Header
    dict set R -Header clientheaders $clientheaders
    dict set R -Header state Parsed
    return $R
}

# RxWait - wait for some input to process
proc RxWait {where} {
    corovar timer
    if {[info exists timer]} {
	catch {::after cancel $timer}; unset timer
    }

    corovar timeout
    if {[dict exists $timeout $where]} {
	set time [dict get $timeout $where]
    } elseif {[dict exists $timeout ""]} {
	set time [dict get $timeout ""]
    } else {
	set time -1
    }
    if {$time > 0} {
	corovar timer
	set timer [::after [expr {$time*1000}] [info coroutine] TIMEOUT $where]
	#puts stderr "Timer $timer [after info $timer] - [info level -1]"
    }

    corovar socket; Readable $socket [info coroutine]
    set rest [lassign [::yieldm $where] exception from]			;# wait for READ event

    if {[info exists timer]} {
	catch {::after cancel $timer}; unset timer
    }

    if {$exception ne ""} {
	Debug.httpd {Exception '$exception' from '$from' in $where}
	return -code error -errorcode [list $exception $from] "$exception from $from"
    } else {
	return 0
    }
}

# Header - read header of request
proc Header {r {one 0}} {
    corovar maxheaders	;# maximum number of headers
    corovar maxline	;# maximum header line length
    set socket [dict get $r -socket]
    chan configure $socket -blocking 0

    if {$one} {
	set state Request
    } else {
	set state Headers
    }

    while {![catch {chan eof $socket} eof] && !$eof} {
	RxWait $state
	set len [gets $socket line]
	if {$len == -1} {
	    # we have no line - can we even get a line?
	    if {$maxline && [chan pending input $socket] > $maxline} {
		Debug.httpd {[info coroutine] MAXLINE [chan pending input $socket] > $maxline}
		Bad $r "Line too long (over $maxline)"
	    } else {
	    }
	    continue
	} elseif {$len == 1} {
	    # we just got a \r - empty line
	    if {$one} {
		# skip multiple redundant empty lines at start of header
		if {[incr count] > 4} {
		    Bad $r "Too Much Blank"
		}
	    } else {
		# this terminates headers
		Debug.httpdlow {[info coroutine] got [llength [split [dict get $r -Full] \n]] lines of header}
		dict set R -Header state $state
		return $r
	    }
	} else {
	    Debug.httpdlow {[info coroutine] read $len bytes '$line' - in:[chan pending input $socket] out:[chan pending output $socket]}
	    dict append r -Full [string range $line 0 end-1] \n	;# append all lines in header
	    if {$one} {
		dict set R -Header state $state
		return $r	;# we only want the first line
	    }
	}
    }

    Debug.httpd {[info coroutine] got EOF after headers:'$r' $eof}
    Bad $r "No end of Headers"
}

# ChunkSize - return the next chunk size
proc ChunkSize {socket} {
    RxWait ChunkSize
    Readble $socket			;# turn off readable event

    set chunk_extra [join [lassign [split [string range [gets $socket] 0 end-1] \;] cs] \;]
    set chunksize 0x$cs	;# how many bytes to read?

    return $chunksize
}

# RxChunked - perform chunked entity reception
proc RxChunked {r} {
    corovar socket		;# connection to client
    corovar todisk		;# the size at which we elect to leave entities on disk
    corovar maxentity	;# maximum sized entity we will accept

    # get size of next chunk
    set chunksize [ChunkSize $socket]	;# how many bytes to read?
    Debug.entity {RxChunked $chunksize}
    if {$chunksize <= 0} {
	# no more bytes to read
	corovar entity_to_read; set entity_to_read 0	;# the entity has been read
	return 0
    }

    # we can only know the length of a Chunked entity after it's been received,
    # so we always store it in a temporary file
    set entity [Tmpfile $r]	;# get entity fd
    chan configure $entity -encoding binary

    set total 0			;# total size of entity read so far
    while {$chunksize > 0} {
	if {$maxentity > 0 && ($total+$chunksize) > $maxentity} {
	    # 413 "Request Entity Too Large"
	    Bad $r "Request Entity Too Large ($maxentity)"
	}

	# prepare the socket for copy
	Readable $socket [info coroutine]
	while {![chan eof $socket] && ![error $socket]} {
	    RxWait RxChunked

	    set buf [chan read $socket $chunksize]
	    chan puts -nonewline $entity $buf
	    set bytes [string length $buf]
	    incr chunksize -$bytes
	    incr total $bytes
	}

	if {[catch {chan eof $socket} eof] || $eof} {
	    Bad $r "EOF in entity"
	}

	set chunksize [ChunkSize $socket]	;# how big is next chunk?
    }

    dict set r content-length $total

    Readable $socket [info coroutine]	;# restart reader loop

    Debug.entity {got chunked entity in $entity}

    # at this point we have a complete entity in $entity file, it's already been ungzipped
    # we need to process it somehow.

    corovar te
    if {[info exists te] && "gzip" in [dict keys $te]} {
	chan pop $entity	;# remove the gzip compression
    }
    set encoding [CharEncoding $r]
    if {$encoding ne "binary"} {
	chan configure $entity -encoding $encoding
    }

    if {$todisk == 0 || [chan tell size $epath] <= $todisk} {
	# we don't want to have things on disk, or it's small enough to have in memory
	# ??? How is entity encoded? - got to read it with encoding
	dict set r -reply -entity [chan read $entity]	;# grab the entity in its entirety
	chan close $entity				;# close the entity fd
    } else {
	# leave some hints for Query file processing
	chan seek $entity 0			;# rewind entity to start
	dict set r -reply -entity_fd $entity	;# this entity is an open fd
    }

    # read+parse more header fields - apparently this is possible with Chunked ... who knew?
    tailcall dict merge $r [HeaderCheck [Parse [Header $r]]]
}

# RxEntityEOF - keep reading the entity until EOF
proc RxEntityEOF {r} {
    corovar socket	;# socket for pipeline
    corovar todisk	;# size at which we leave entities on disk
    corovar maxentity	;# maximum sized entity we will accept

    set encoding [CharEncoding $r]	;# determine charset of content

    # read entity into memory
    Readable $socket [info coroutine]
    Debug.entity {RxEntityEOF}

    set entity ""
    while {![chan eof $socket]} {
	RxWait RxEntityEOF
	append entity [chan read $socket]	;# read in as much as is available
    }

    Debug.entity {RxEntityEOF finished reading [string length $entity] [chan eof $socket]}

    if {$encoding ne "binary"} {
	dict set r -reply -entity [encoding convertfrom $encoding $entity]
    } else {
	dict set r -reply -entity $entity
    }

    # postprocess/decode the entity
    corovar te
    if {[info exists te]
	&& [dict exists $r -reply -entity]
	&& "gzip" in $te
    } {
	dict set r -reply -entity [::zlib inflate [dict get $r -reply -entity]]
    }

    dict set r -reply content-length 

    return $r
}

# RxSizedEntity - given an entity size, read it in.
proc RxSizedEntity {r} {
    corovar socket	;# socket for pipeline
    corovar todisk	;# size at which we leave entities on disk
    corovar maxentity	;# maximum sized entity we will accept

    # simple 'entity follows header' with explicit length
    set left [dict get $r content-length]

    Debug.entity {RxSizedEntity of length $left}

    # enforce server limits on Entity length
    if {$maxentity > 0 && $left > $maxentity} {
	# 413 "Request Entity Too Large"
	Bad $r "Request Entity Too Large" 413
    }

    set encoding [CharEncoding $r]	;# determine charset of content

    # decide whether to read to RAM or disk
    if {$todisk > 0 && $left > $todisk} {
	# this entity is too large to be handled in memory, write it to disk
	
	# create a temp file to contain entity
	set entity [Tmpfile $r]
	chan configure $entity -encoding binary
	Debug.entity {RxSizedEntity of length $left > $todisk ==> write to $entity}

	Readable $socket [info coroutine]

	# start the copy
	while {$left && ![chan eof $socket]} {
	    set buf [read $socket $left]
	    chan puts -nonewline $entity $buf
	    incr left [string length $buf]
	}

	if {[catch {chan eof $socket} eof] || $eof} {
	    Bad $r "EOF in entity"
	}

	# at this point we have a complete entity in the open $entity file, it's already been ungzipped
	# we need to process it somehow
	chan seek $entity 0
	if {$encoding ne "binary"} {
	    chan configure $entity -encoding $encoding	;# set encoding (if any)
	}

	dict set r -reply -entity_fd $entity
    } elseif {$left > 0} {
	# read entity into memory
	Readable $socket [info coroutine]
	Debug.entity {RxSizedEntity of length $left < $todisk ==> write to memory}

	set entity ""
	while {[string length $entity] < $left && ![chan eof $socket]} {
	    RxWait RxSizedEntity
	    append entity [chan read $socket [expr {$left - [string length $entity]}]]	;# read in as much as is available
	}

	Debug.entity {RxSizedEntity finished reading [string length $entity] [chan eof $socket]}

	if {$encoding ne "binary"} {
	    dict set r -reply -entity [encoding convertfrom $encoding $entity]
	} else {
	    dict set r -reply -entity $entity
	}

	if {[string length $entity] < $left} {
	    Bad $r "EOF in entity"
	}

	# postprocess/decode the entity
	corovar te
	if {[info exists te]
	    && [dict exists $r -reply -entity]
	    && "gzip" in $te
	} {
	    dict set r -reply -entity [::zlib inflate [dict get $r -reply -entity]]
	}
    } else {
	dict set r -reply -entity ""
	# the entity, length 0, is therefore already read
	# 14.13: Any Content-Length greater than or equal to zero is a valid value.
    }

    return $r
}

# RxEntity - return a request dict containing any Entity
proc RxEntity {R} {
    
    # Read Entity (if any)
    # TODO: 4.4.2 If a message is received with both
    # a Transfer-Encoding header field
    # and a Content-Length header field,
    # the latter MUST be ignored.
    if {[dict exists $R transfer-encoding]} {
	# chunked 3.6.1, identity 3.6.2, gzip 3.5,
	# compress 3.5, deflate 3.5
	set tels {}; set te_params {}

	variable te_encodings	;# te_encodings we support
	foreach tel [split [dict get $R transfer-encoding] ,] {
	    set param [lassign [split $tel ";"] tel]
	    set tel [string tolower [string trim $tel]]
	    if {$tel ni $te_encodings} {
		# can't handle a transfer encoded entity
		# queue up error response (no caching)
		Bad $R "$tel transfer encoding" 501
		return {}
		# see 3.6 - 14.41 for transfer-encoding
	    } else {
		dict set te $tel [split $param ";"]
	    }
	}

	if {[dict exists $te chunked]} {
	    dict set R -Header state Chunking
	    set R [RxChunked $R]
	} else {
	    Bad $R "Length Required" 411
	}
    } elseif {[dict exists $R content-length]} {
	dict set R -Header state Sized
	set R [RxSizedEntity $R]
    } elseif {[dict get $R -Header version] > 1.0} {
	# this is a content-length driven entity transfer 411 Length Required
	Bad $R "Length Required" 411
    } else {
	dict set R -Header state RxEntityEOF
	set R [RxEntityEOF $R]
    }
    state_log {R rx entity [dict get $R -socket] [dict get $R -transaction]}

    dict set R -Header state Entity
    return $R
}

# RxDead - called when Rx coroutine disappears
proc RxDead {coro s tx args} {
    #puts stderr "RxDEAD $coro '$s' [catch {set r [chan pending input $s]}]/$r [catch {set t [chan pending output $s]}]/$t [llength [chan names]]"
}


# RxHeaders - collect the headers
proc RxHeaders {R} {
    set R [Header $R]	;# collect all remaining headers

    # indicate to tx that a request with this transaction id
    # has been received and is (as yet) unsatisfied
    [dict get $R -tx] pending [dict get $R -transaction]

    set R [Parse $R]

    state_log {R rx headers [dict get $R -socket] [dict get $R -transaction]}
    return $R
}

variable rx_defaults [defaults {
    port 80		;# default listening port
    maxline 4096	;# maximum line length we'll accept
    maxline 4096	;# maximum header line length
    maxheaders 200	;# maximum number of headers we'll accept
    maxurilen 0	;# maximum length of URI
    maxfield 0
    maxentity 0
    todisk 0
    def_charset [encoding system]
    entitypath ""	;# path on which Tmpfile creates entity files
    opts {}
    timeout {"" 20 Header 20 ChunkSize 20 Chunked 20 RxSizedEntity 20}
    ctype text/html
    process process	;# default processing is to call process
    rxprocess RxProcess	;# default rx processing is to call RxProcess
}]

# RxProcess - default handling of packet reception
# this can be overridden, and the application can take over processing
proc RxProcess {R} {
    set R [Header $R 1]		;# fetch request/status line
    if {![string match HTTP/* [lindex [split [dict get $R -Full] " "] end]]} {
	Bad $r "This isn't even HTTP"
    }
    set R [RxHeaders $R]	;# fetch all remaining headers
    set R [HeaderCheck $R]	;# parse $headers as a complete request header
    set R [RxEntity $R]		;# fetch any entity
    return $R			;# return completed request
}

# Rx - coroutine to process pipeline reception
proc Rx {args} {
    # all of these variables become corovars
    variable rx_defaults

    set args [dict merge $rx_defaults $args]
    dict with args {}	;# install rx state vars
    if {[info exists onconnect]} {
	{*}$onconnect [info coroutine] [info locals]
    }

    Debug.listener {start Rx [info coroutine] $args}

    # This can be used as a debugging aid to track coro state
    #trace add command [info coroutine] delete [namespace code [list RxDead [info coroutine] $socket $tx]] ;# track coro state

    set passthru 0
    set headers {}
    set transaction 0	;# unique count of packets received by this receiver
    set R {}
    try {
	# put receiver into header/CRLF mode and start listening for readable events
	chan configure $socket -blocking 1
	Readable $socket [info coroutine]	;# start listening on $socket with this coro

	while {![catch {chan eof $socket} eof]
	       && !$eof
	       && [chan pending input $socket] != -1 && [chan pending output $socket] != -1
	       && (![dict exists $R connection] || [string tolower [dict get $R connection]] ne "close")
	   } {
	    state_log {R rx request $socket $transaction}
	    
	    # receive and process packet
	    set R [list -socket $socket -transaction [incr transaction] -tx $tx -reply {} -Header {state Initial}]
	    set R [{*}$rxprocess $R]	;# receive the request
	    {*}$process $R		;# Process the request+entity in a bespoke command

	    state_log {R rx processed $socket $transaction}
	}
    } trap HTTP {e eo} {
	state_log {R rx http $socket $transaction}
	Debug.httpd {Httpd $e}
    } trap TIMEOUT {e eo} {
	if {[dict get $eo -errorcode] eq ""} {
	    state_log {R rx inactive $socket $transaction}
	    Debug.httpd {Inactive $e}
	} else {
	    state_log {R rx timeout $socket $transaction}
	    Debug.httpd {Httpd $e}
	}
    } trap PASSTHRU {e eo} {
	# the process has handed off our socket to another process
	# we have nothing to do but wait
	set passthru 1
	puts stderr "[info coroutine] PASSTHRU"
    } on error {e eo} {
	state_log {R rx error $socket $transaction}
	Debug.error {Rx $socket ERROR '$e' ($eo)}
    } on return {e eo} {
	# this happens if something tailcalls out of the coro (?)
	#Debug.error {Rx $socket RETURN '$e' ($eo)}
    } on continue {e eo} {
	Debug.error {Rx $socket CONTINUE '$e' ($eo)}
    } on break {e eo} {
	Debug.error {Rx $socket BREAK '$e' ($eo)}
    } on ok {e eo} {
	# this happens on normal return
	
	if {[catch {chan eof $socket} eof] || $eof} {
	    set reason "EOF on socket"
	} elseif {[chan pending input $socket] == -1 || [chan pending output $socket] == -1} {
	    set reason "No Pending i/o [chan pending input $socket] == -1 || [chan pending output $socket] == -1"
	} elseif {[dict exists $R connection] && [string tolower [dict get $R connection]] eq "close"} {
	    set reason "Client requested close"
	} {
	    set reason unknown
	}
	
	state_log {R rx closed $socket $transaction $reason}
	Debug.httpd {Normal termination: $reason}
    } finally {
	if {[info exists timer]} {
	    catch {::after cancel $timer}; unset timer
	}
	
	Debug.listener {Rx [info coroutine] DONE $socket [chan eof $socket] || [chan pending input $socket] == -1 || [chan pending output $socket] == -1 || headers [llength $headers]}
	
	if {!$passthru} {
	    catch {Readable $socket}	;# turn off the chan event readable
	    catch {chan close $socket read}	;# close the socket read side
	    catch {$tx closing}		;# inform Tx coro that we're closing
	} else {
	    catch {$tx passthru}		;# inform Tx coro that we're closing
	}

	state_log {"" rx closed $socket $transaction}
    }
    
    if {[info exists ondisconnect]} {
	{*}$ondisconnect [info coroutine] [info locals]
    }
}

# load required H components
load Hurl.tcl Herr.tcl
