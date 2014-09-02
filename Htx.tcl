# Htx.tcl - light Httpd 1.1
variable ce_encodings {gzip}		;# support these char encodings
variable do_not_encode {image binary}	;# support these char encodings
variable buffering_size	1048576		;# how many bytes to read/send from files
variable chunksize	1024			;# how bug a chunk to send per chunk

variable tx_defaults [defaults {
    server_id "H [package present H]"
}]

# log - common log format
# used to generate one form of log from a response dict
proc log {r} {
    lappend line [dict get? $r -ipaddr]	;# remote IP
    lappend line -	;# RFC 1413 identity of the client.  'sif
    if {[dict exists $r -user]} {
	# is there a user identity?
	lappend line [dict get? $r -user]
    } else {
	lappend line -
    }

    # receipt time of connection
    if {[info exists $r -received_seconds]} {
	lappend line \[[clock format [dict get $r -received_seconds] -format "%d/%b/%Y:%T %Z"]\]
    }

    # first line of request
    lappend line \"[dict get? $r -Header full]\"

    # status we returned to it
    if {[dict exists $r -code]} {
	lappend line [dict get? $r -code]
    } else {
	lappend line 200
    }

    # content byte length
    lappend line [dict get? $r content-length]

    # referer, useragent, cookie, if any
    if {[dict exists $r referer]} {
	lappend line \"[dict get $r referer]\"
    } else {
	lappend line \"\"
    }
    if {[dict exists $r user-agent]} {
	lappend line \"[dict get $r user-agent]\"
    } else {
	lappend line \"\"
    }

    if {[dict exists $r -user]} {
	lappend line \"[dict get $r -user]\"
    } elseif {[dict exists $r cookie]} {
	lappend line \"[dict get $r cookie]\"
    }

    if {[dict exists $r -received] && [dict exists $r -sent]} {
	set diff [expr {[dict get $r -sent] - [dict get $r -received]}]
    }

    return [string map {\n \\n \r \\r} [join $line]]
}

# Errors - map from HTTP result code to human readable form
variable Errors
array set Errors {
    1 "Informational - Request received, continuing process"
    100 Continue
    101 "Switching Protocols"

    2 "Success - received, understood, and accepted"
    200 OK 201 Created 202 Accepted 203 "Non-Authoritative Information"
    204 "No Content" 205 "Reset Content" 206 "Partial Content"

    3 "Redirection - Further action needed"
    300 "Multiple Choices" 301 "Moved Permanently" 302 "Found" 303 "See Other"
    304 "Not Modified" 305 "Use Proxy" 307 "Temporary Redirect"

    4 "Client Error - request bad or cannot be fulfilled"
    400 "Bad Request" 401 "Unauthorized" 402 "Payment Required" 403 "Forbidden"
    404 "Not Found" 405 "Method Not Allowed" 406 "Not Acceptable" 407 "Proxy Authentication Required"
    408 "Request Time-out" 409 "Conflict" 410 "Gone" 411 "Length Required" 412 "Precondition Failed"
    413 "Request Entity Too Large" 414 "Request-URI Too Large" 415 "Unsupported Media Type"
    416 "Requested range not satisfiable" 417 "Expectation Failed"

    5 "Server Error - Server failed to fulfill an apparently valid request"
    500 "Internal Server Error" 501 "Not Implemented" 502 "Bad Gateway"
    503 "Service Unavailable" 504 "Gateway Time-out" 505 "HTTP Version not supported"
}

# set of request-only headers
variable rq_headers {
    accept accept-charset accept-encoding accept-language authorization
    expect from host if-match if-modified-since if-none-match if-range
    if-unmodified-since max-forwards proxy-authorization referer te
    user-agent keep-alive cookie via range
    origin sec-websocket-key1 sec-websocket-key2
    access-control-request-method dnt
}

# TxChunk - send a buffer in chunk mode
proc TxChunk {buf} {
    variable chunksize
    while {[string length $buf]} {
	set chunk [string range $buf 0 $chunksize-1]
	set buf [string range $buf $chunksize end]
	TxLine $socket [format %X [string length $chunk]] $chunk
	Debug.httpdtxlow {[info coroutine] TxChunk sent [format %X [string length $chunk]] [binary encode hex $chunk] '$chunk'}
    }
}

# find etag in if-none-match field
proc any-match {r etag} {
    if {![dict exists $r if-none-match]} {
	return 0
    }

    set im [split [dict get $r if-none-match] ,]
    set result [expr {$etag in $im}]
    Debug.cache {any-match: $result - $etag in $im}
    return $result
}

# find etag in if-match field
proc if-match {r etag} {
    if {![dict exists $r if-match]} {
	return 1
    }

    set im [split [dict get $r if-match] ,]
    set result [expr {$im eq "*" || $etag in $im}]
    Debug.cache {if-match: $result - $etag in $im}
    return $result
}

# find etag in if-range field
proc if-range {r etag} {
    if {![dict exists $r if-range]} {
	return 1
    }

    set im [split [dict get $r if-range] ,]
    set result [expr {$im eq "*" || $etag in $im}]
    Debug.cache {if-match: $result - $etag in $im}
    return $result
}

# TxConditional - make GET/HEAD conditional
# this will transform a request if there's a conditional which
# applies to it.
proc TxConditional {} {
    corovar reply
    set etag \"[string trim [dict get $reply etag] \"]\"

    # Check if-none-match
    if {[any-match $reply $etag]} {
	# rfc2616 14.26 If-None-Match
	# If any of the entity tags match the entity tag of the entity
	# that would have been returned in the response to a similar
	# GET request (without the If-None-Match header) on that
	# resource, or if "*" is given and any current entity exists
	# for that resource, then the server MUST NOT perform the
	# requested method, unless required to do so because the
	# resource's modification date fails to match that
	# supplied in an If-Modified-Since header field in the request.
	if {[string toupper [dict get? $reply -Header method]] in {"GET" "HEAD"}} {
	    # if the request method was GET or HEAD, the server
	    # SHOULD respond with a 304 (Not Modified) response, including
	    # the cache-related header fields (particularly ETag) of one
	    # of the entities that matched.

	    # the response MUST NOT include other entity-headers
	    # than Date, Expires, Cache-Control, Vary, Etag, Content-Location
	    set reply [dict filter $reply script {n v} {
		expr {[string match -* $n]
		      || [string tolower $n] in {date expires cache-control vary etag content-location}
		  }
	    }]
	    
	    dict set reply -code 304
	} else {
	    # For all other request methods, the server MUST respond with
	    # a status of 412 (Precondition Failed).
	    dict set reply -code 412
	}
    } elseif {![if-match $reply $etag]} {
	# return status of 412 (Precondition Failed).
	dict set reply -code 412
    } elseif {![if-range $reply $etag]} {
	catch {dict unset reply range}
	# 14.27 If-Range
	# If the entity tag given in the If-Range header matches the current
	# entity tag for the entity, then the server SHOULD provide the
	# specified sub-range of the entity using a 206 (Partial content)
	# response. If the entity tag does not match, then the server SHOULD
	# return the entire entity using a 200 (OK) response.
    }

    return [dict get $reply -code]
}

# TxCharset - ensure correctly encoded content in response
proc TxCharset {} {
    corovar reply
    if {[dict exists $reply -chconverted]
	|| ![dict exists $reply content-type]
    } {
	return	;# don't re-encode by charset
    }

    # handle charset for text/* types
    lassign [split [dict get $reply content-type] {;}] ct
    if {[string match text/* $ct]
	|| [string match */*xml $ct]
	|| [string match application/*javascript $ct]
    } {
	if {[dict exists $reply -charset]} {
	    set charset [dict get $reply -charset]
	} else {
	    set charset [encoding system]	;# default charset (utf-8)
	    dict set reply -charset $charset
	}

	# ensure content is converted to correct charset,
	# flag conversion in response, to avoid double conversion
	dict set reply -chconverted $charset
	dict set reply content-type "$ct; charset=$charset"
	dict set reply -content [encoding convertto $charset [dict get $reply -content]]
    }
}

proc TxRange {} {
    corovar reply
    # handle range for 200
    # NOTE: not currently supporting range request
    set ranges [dict get $reply range]	;# client requested a range of content
    # FIXME: multiple Range - this only does one
    # FIXME: what about transfer-encoded (ie: gzipped) content?
    # FIXME: what about a range over a file?
    Debug.httpdtx {ranges: $ranges}
    set ranges [lindex [lassign [split $ranges =] unit] 0]
    set ranges [split $ranges ,]
    set ranges [lindex $ranges 0]	;# only handle one range
    foreach rr $ranges {
	lassign [split $rr -] from to
	lassign [split $to] to
	set size [dict get $reply content-length]
	if {$from eq ""} {
	    set from [expr {$size-$to+1}]
	    set to $size
	} elseif {$to > $size || $to eq ""} {
	    set to [expr {$size-1}]
	}
	lappend range $from $to	;# remember range to send
    }
    
    # send appropriate content range and length fields
    set code 206	;# partial content
    dict set reply -code 206
    dict set reply -range $range	;# record this for TxEntity
    dict set reply content-range "bytes $from-$to/$size"
    dict set reply content-length [expr {$to-$from+1}]
    
    Debug.httpdt {range: [dict get $reply content-range] of length [dict get $reply content-length]}
}

# TxLine - send a line to the pipeline socket
proc TxLine {socket args} {
    #set ll [open output.txt a]
    #fconfigure $ll -encoding binary -translation binary
    foreach line $args {
	chan puts -nonewline $socket ${line}\x0d\x0a
	#chan puts -nonewline $ll ${line}\x0d\x0a
    }
    #close $ll
    Debug.httpdtxlow {[info coroutine] TxLine: '$args' ([fconfigure $socket])}
    # FIXME: refrain from sending too-long lines
}

# TxHeaders - emit the reply's headers
proc TxHeaders {socket reply} {
    variable rq_headers

    # send all HTTP fields which have relevance in response
    #set ll [open output.txt a]
    #fconfigure $ll -encoding binary -translation binary
    dict for {n v} $reply {
	if {[string index $n 0] eq "-"} continue
	set nl [string tolower $n]
	if {[string match x-* $nl] || $nl ni $rq_headers} {
	    Debug.httpdtxlow {[info coroutine] Tx Header $n: $v}
	    chan puts -nonewline $socket "$n: $v\x0d\x0a"
	    #chan puts -nonewline $ll "$n: $v\x0d\x0a"
	}
    }

    # special case for cookies
    foreach c [dict get? $reply -set-cookies] {
	chan puts -nonewline $socket "set-cookie: $c\x0d\x0a"
    }

    chan puts -nonewline $socket "\x0d\x0a"	;# send end-of-header
    #chan puts -nonewline $ll "\x0d\x0a"
    #close $ll
}

variable read_chunks [expr {10 * 1024 * 1024}]
proc TxReadFile {fd tx {keep 0}} {
    variable read_chunks
    if {[chan eof $fd]} {
	if {!$keep} {
	    close $fd
	}
	tailcall $tx send 1	;# send the data to the client
    }

    set data [chan read $fd $read_chunks]
    if {$data eq ""} return
    tailcall $tx send 0 $data	;# send the data to the client
}

# TxFile - utility function to send a file or stream
proc TxFile {fd args} {
    set reply [lindex $args end]
    set args [lrange $args 0 end-1]
    set reply [dict merge $reply $args]

    set tx [dict get $reply -tx]
    set ns [dict get $reply -ns]
    set socket [dict get $reply -socket]

    # the app has returned an open file instead of literal content
    set position [chan tell $fd]	;# initial fd position
    if {$position == -1} {
	# this file is not seekable, it's a pure stream
	Debug.httpdtx {Content is a pure stream - chunking}
	catch {dict unset reply content-length}		;# this is a pipe, not a seekable file
	dict set reply transfer-encoding chunked
    } elseif {[dict exists $reply content-encoding]} {
	# we're going to gzip this thing, so we won't know its length
	Debug.httpdtx {Content is a seekable file, want GZIP - chunking}
	catch {dict unset reply content-length}		;# we don't know the gzipped length
	dict set reply transfer-encoding chunked
    } elseif {![dict exists $reply content-length]} {
	# not gzipping, is seekable - can/should know its length - set it
	Debug.httpdtx {Content is a seekable file, do not want GZIP - unknown content-length}
	chan seek $fd 0 end			;# move to the end of the file
	dict set reply content-length [expr {[chan tell $fd] - $position}]
	chan seek $fd $position start	;# move back to the old file position
	catch {dict unset reply transfer-encoding}
    } else {
	# fd is in position, we have been given the length to send
	Debug.httpdtx {Content is a seekable file, do not want GZIP - known content-length}
    }

    ${ns}::TxHeaders $socket $reply	;# emit $reply's headers

    # set up a readable event stream over $fd
    chan configure $fd -blocking 0
    chan event $fd readable [list ${ns}::TxReadFile $fd $tx [expr {[dict exists $reply -file_keep]?[dict get $reply -file_keep]:0}]]

    return $reply
}

# TxFileName - utility function to send a file or stream
proc TxFileName {name args} {
    set fd [open $name r]
    chan configure $fd -encoding binary -translation binary
    return [H TxFile $fd {*}$args]
}

# Now - return the current time and date in HTTP format
proc Now {} {
    return [clock format [clock seconds] -format {%a, %d %b %Y %T GMT} -gmt true]
}

# TxSend - process a single reply
proc TxSend {} {
    corovar reply
    corovar ns
    corovar socket

    # ensure the reply code is set
    if {![dict exists $reply -code]} {
	dict set reply -code [set code 200]	;# presume it's ok
    } elseif {[set code [dict get $reply -code]] < 4} {
	# presume this was a tcl error code, not an HTTP code
	Debug.httpd {[info coroutine] Tx Tcl error code ($code)}
	dict set reply -code [set code 500]
    }

    # make reply conditional if requested
    if {$code eq 200 && [dict exists $reply etag]} {
	# non-OK responses aren't conditional (?)
	set code [TxConditional]
    }

    # handle Vary field and -vary dict - unsure whether this is necessary
    dict set reply -vary accept-encoding 1
    if {[dict exists $reply -vary]} {
	if {[dict exists $reply -vary *]} {
	    dict set reply vary *
	} else {
	    dict set reply vary [join [dict keys [dict get $reply -vary]] ,]
	}
	dict unset reply -vary
    }

    # Deal with content data by response type
    if {$code == 204} {
	# 204 (no content) - responses MUST NOT include a message-body
	foreach n [dict keys $reply content-*] {
	    dict unset reply $n
	}
	foreach n {transfer-encoding} {
	    if {[dict exists $reply $n]} {
		dict unset $reply $n
	    }
	}
    } elseif {$code >= 200 && $code < 300} {
	# if {[dict exists $reply range]} {TxRange}
    }

    catch {dict unset reply transfer-encoding}	;# default is not chunked
    catch {dict unset reply content-encoding}	;# default is not gzipped

    if {[dict exists $reply content-type]} {
	set ct [string tolower [dict get $reply content-type]]
    } else {
	set ct application/octet-stream
    }

    # determine whether the content is able to be compressed/encoded
    variable ce_encodings	;# what encodings do we support?
    variable do_not_encode	;# what file types do we not encode?
    if {[lindex [split $ct /] 0] ni $do_not_encode && $ct ni $do_not_encode} {
	# choose content encoding
	if {[dict exists $reply accept-encoding]
	    && ![dict exists $reply content-encoding]} {
	    foreach en [split [dict get $reply accept-encoding] ","] {
		lassign [split $en ";"] en pref
		set en [string tolower [string trim $en]]
		if {$en in $ce_encodings} {
		    dict set reply content-encoding $en	;# determined we want to encode/gzip
		    break
		}
	    }
	}
    }

    # set up entity transmission header elements
    if {[dict get? $reply -Header method] eq "HEAD"} {
	# All responses to the HEAD request method MUST NOT
	# include a message-body but may contain all the content
	# header fields.
	if {[dict exists $reply -content]} {
	    dict unset reply -content
	}
	if {[dict exists $reply -process]} {
	    dict unset reply -process
	}
    } elseif {[dict exists $reply -content]} {
	# we have all the content, may as well gzip it if required
	TxCharset		;# charset-encode content
	if {[dict exists $reply content-encoding]} {
	    # gzip it right now - prepend a minimal gzip file header:
	    # signature, deflate compression, no flags, mtime,
	    # xfl=0, os=3
	    set gzheader [list crc 0 time [clock seconds] type [expr {[string match text/* [dict get? $reply content-type]]?"text":"binary"}]]
	    set gzheader [dict merge $gzheader [dict get? $reply -gzheader]]
	    set gzip [::zlib gzip [dict get $reply -content] -header $gzheader]
	    if {[string length $gzip] < [string length [dict get $reply -content]]} {
		# don't send gzipped if it is larger than original
		dict set reply -content $gzip	;# content becomes gzipped content
		dict set reply content-encoding gzip
	    } else {
		dict unset reply content-encoding
	    }
	}

	dict set reply content-length [string length [dict get $reply -content]] ;# set correct content-length
    } elseif {[dict exists $reply -process]} {
	dict set reply transfer-encoding chunked	;# it has to be sent chunked
    } elseif {$code in {200 201 202 203 204 205 206}} {
	# no -content, no -process ... reply is contentless
	Debug.httpdtx {Format: contentless - response empty - no content in reply ($reply)}
	set code 204
	catch {dict unset reply content-length}
	catch {dict unset reply content-type}
	catch {dict unset reply content-encoding}
    }

    # set the informational header error message
    variable Errors
    if {[dict exists $reply -error] && ![dict get $reply -error] eq ""} {
	set errmsg [dict get $reply -error]
    } elseif {[info exist Errors($code)]} {
	set errmsg $Errors($code)
    } else {
	set errmsg "Error $code"
    }
    
    ### start sending header down the socket
    corovar server_id
    set rline [list "HTTP/1.1 $code $errmsg" "Date: [Now]" "Server: $server_id"]

    Debug.httpd {'[lindex $rline 0]' response to '[dict get? $reply -Header full]'}
    TxLine $socket {*}$rline

    if {$code == 204} {
	${ns}::TxHeaders $socket $reply	;# emit $reply's headers
	return
    }

    if {[dict exists $reply -process]} {
	# we have an asynchronous content generator, start it up
	# this will continue to block new responses until $reply is unset

	# set up content-encoding infrastructure
	switch -- [dict get? $reply content-encoding] {
	    gzip {
		corovar gzipper
		# comment - Add the given comment to the header of the gzip-format data.
		# crc - compute a CRC of the header? Note that if the data is to be interchanged with the gzip program, a header CRC should not be computed.
		# filename - The name of the file that the data to be compressed came from.
		# os - The operating system type code, which should be one of the values described in RFC 1952.
		# time   The time that the file named in the filename key was last modified. This will be in the same as is returned by clock seconds or file mtime.
		# type   The type of the data being compressed, being binary or text.
		
		set gzheader [list crc 0 time [clock seconds] type [expr {[string match text/* [dict get? $reply content-type]]?"text":"binary"}]]
		set gzheader [dict merge $gzheader [dict get? $reply -gzheader]]
		set gzipper [zlib stream gzip -header $gzheader]
	    }
	    identity {
	    }
	    default {
		catch {dict unset reply content-encoding}
	    }
	}

	dict set reply -tx [info coroutine]
	dict set reply -ns $ns
	set reply [{*}[dict get $reply -process] $reply]
	return
    }

    ${ns}::TxHeaders $socket $reply	;# emit $reply's headers
    
    if {[dict exists $reply -content]} {
	# -content exists - we have the full content ready to go - just send it
	# send literal content entire
	chan puts -nonewline $socket [dict get $reply -content]	;# send the content in its entirety

	#set ll [open output.txt a]
	#fconfigure $ll -encoding binary -translation binary
	#chan puts -nonewline $ll [dict get $reply -content]	;# send the content in its entirety
	#close $ll

	Debug.httpd {[info coroutine] Tx sent entity: [dict get $reply content-length] bytes}

	# generate a log line
	corovar ns; catch {${ns}::log $reply}

	# this request is no longer pending
	corovar sent
	Debug.httpd {[info coroutine] Tx SENT response #$sent}
	incr sent 	;# advance lower edge of transmission window
    }

    unset reply	;# Tx is no longer busy

    chan flush $socket
}

# TxDead - called when Tx coroutine disappears
proc TxDead {coro s args} {
    #set r ""; set t ""; puts stderr "Tx DEAD '$s' [catch {set r [chan pending input $s]}]/$r [catch {set t [chan pending output $s]}]/$t [llength [chan names]]"
}

proc Tx {args} {
    # create corovars from $args with defaults
    variable tx_defaults
    set args [dict merge $tx_defaults $args]
    dict with args {}

    set ns [namespace qualifiers [info coroutine]]

    trace add command [info coroutine] delete [namespace code [list TxDead [info coroutine] $socket]]

    set pending {}	;# dict of requests pending responses
    set sent 0		;# how many contiguous packets have we sent?
    set close ""	;# set if we're required to close
    set continue 0	;# no 100-continue pending
    set trx 0

    Debug.listener {start Tx [info coroutine]}
    try {
	# while the channel is still open for transmission or is busy
	# or there are requests pending or the input socket is open
	# wait and process and wait, process and wait.
	# While $reply exists, we're actively processing it, so cannot
	# start processing a new request.
	while {[chan pending output $socket] != -1} {
	    if {![info exists reply] && ![dict size $pending] && [chan pending input $socket] == -1} break

	    set rest [lassign [yieldm $sent] op]	;# fetch next command
	    Debug.httpdtx {[info coroutine] Tx yield $op ([lindex $rest 0])}
	    switch -- $op {
		continue {
		    # Rx indicating it needs a 100-Continue sent
		    state_log {"" tx $op $socket $trx $sent [llength $pending]}

		    if {![info exists reply]} {
			# special case 100-Continue - straight out the socket
			Debug.httpdtx {[info coroutine] Tx sending 100-Continue}
			TxLine $socket "HTTP/1.1 100 Continue"
			TxLine $socket ""
			set continue 0
		    } else {
			# 100-Continue will have to wait until not-busy
			incr continue	;# set continue flag
		    }
		}

		send {
		    # the asynchronous content generator has some content to send
		    state_log {reply tx $op $socket $trx $sent [llength $pending]}
		    lassign $rest eof content

		    # apply gzipping
		    if {[info exists gzipper]} {
			if {$eof} {
			    set content [$gzipper add -finalize $content]	;# finish up the gzip
			} else {
			    set content [$gzipper add $content]	;# get some more gzipped content
			}
		    }

		    if {[dict exists $reply transfer-encoding]} {
			# send some chunks
			variable chunksize
			while {[string length $content]} {
			    set chunk [string range $content 0 $chunksize-1]
			    set content [string range $content $chunksize end]
			    TxLine $socket [format %X [string length $chunk]] $chunk
			    Debug.httpdtxlow {[info coroutine] TxChunk sent [format %X [string length $chunk]] [binary encode hex $chunk] '$chunk'}
			}
			if {$eof} {
			    TxLine $socket 0 ""
			}
		    } else {
			# send some data
			chan puts -nonewline $socket $content
		    }

		    chan flush $socket
		    if {!$eof} continue

		    # the asynchronous content generator has finished
		    if {$continue} {
			# special case 100-Continue - straight out the socket
			Debug.httpd {[info coroutine] Tx sending deferred 100-Continue}
			unset continue
			TxLine $socket "HTTP/1.1 100 Continue"
			TxLine $socket ""
			chan flush $socket
		    }
		    
		    # generate a log line
		    catch {${ns}::log $reply}

		    if {[info exists gzipper]} {
			$gzipper close	;# done with the stream
			unset gzipper	;# delete stream
		    }

		    unset reply	;# we've completed the asynchronous reply
		    incr sent	;# we've sent another reply
		}

		pending {
		    state_log {"" tx $op $socket $trx $sent [llength $pending]}

		    # Initial reception of a request is signalled by a pend command from Rx
		    # we get the -transaction, being the ordinal number of received requests
		    # indicates to Tx that a request with this transaction id
		    # has been received and is (as yet) unsatisfied
		    set trx [lindex $rest 0]	;# request transaction number
		    Debug.httpdtx {[info coroutine] Tx received indication of $trx reception}
		    dict set pending $trx {}	;# accept the pending response
		    # FIXME: what if we get a received $trx indication out of sequence?
		}

		reply {
		    # queue a response for sending - this is called by Rx or its progeny
		    set r [lindex $rest 0]		;# reply dict
		    state_log {r tx $op $socket $trx $sent [llength $pending]}
		    set trx [dict get $r -transaction]	;# reply dict's transaction count should match earlier pending
		    if {$trx <= $sent} {
			# this reply is a duplicate of an already-sent packet
			# this could happen if a processing command has sent us
			# multiple responses.  First one wins, fellers.
			Debug.error {Send discarded: duplicate (H $r)}
			continue
		    } elseif {[dict exists $pending $trx]
			      && [dict size [dict get $pending $trx]]
			  } {
			# a duplicate response has been sent - discard this
			# this could happen if a dispatcher sends a response,
			# and subsequently gets an error which we try to send out
			Debug.error {Send discarded: duplicate (H $r) - sent:([dict get $pending $trx])}
			continue	;# duplicate response - just ignore
		    } else {
			dict set pending $trx $r	;# queue the pending response
		    }
		}

		closing {
		    # Rx indicates it's closing
		    state_log {"" tx $op $socket $trx $sent [llength $pending]}
		    append close "Rx dying"		;# Tx should close too
		}
		
		default {
		    state_log {"" tx $op $socket $trx $sent [llength $pending]}
		    Debug.error {[info coroutine] Tx got '$op' op.  with ($rest)}
		}
	    }

	    if {[info exists reply]} continue	;# go back to yield for any updates

	    # if Tx is idle, process any pending replies
	    # received a response - if Tx is idle, process all pending responses.
	    # requests are stored in order of reception because of pending message,
	    # so we process each pending request in natural key order.  Thank you dkf for that ability.
	    foreach next [dict keys $pending] {
		if {[info exists reply]} break

		if {$next > $sent+1} {
		    Debug.httpdtx {[info coroutine] Tx exhausted $next vs [expr {$sent+1}] ([dict size $pending] remain - [dict keys $pending])}
		    break	;# pipeline is blocked pending more replies
		}
		
		# respond to the next transaction in trx order
		# consume the next reply from pending queue
		if {![dict size [dict get $pending $next]]} {
		    Debug.httpdtx {[info coroutine] Tx pipeline stalled at $next}
		    break	;# merely a place-holder.  We must wait for the actual packet
		}
		
		set reply [dict get $pending $next]	;# we are now busy processing $reply
		dict unset pending $next		;# consume pending response
		Debug.httpdtx {[info coroutine] Tx sending ($reply)}
		
		${ns}::TxSend
	    }

	    if {$close ne ""
		&& ![dict size $pending]
		&& [chan pending input $socket] == -1} {
		# we have nothing pending to send and reader is gone
		break	;# we're done
	    }
	}
    } on error {e eo} {
	Debug.error {Tx $socket ERROR '$e' ($eo)}
    } on return {e eo} {
	Debug.httpdtxlow {Tx $socket RETURN '$e' ($eo)}
    } on continue {e eo} {
	Debug.httpdtxlow {Tx $socket CONTINUE '$e' ($eo)}
    } on break {e eo} {
	Debug.httpdtxlow {Tx $socket BREAK '$e' ($eo)}
    } on ok {e eo} {
	#Debug.httpdtxlow {Tx $socket OK '$e' ($eo)}
    } finally {
	if {[dict size $pending]} {
	    Debug.error {Tx [info coroutine] DEAD with [dict size $pending] pending}
	}
	Debug.httpdtx {[info coroutine] Tx close $socket}
	
	if {[info exists gzipper]} {
	    $gzipper close	;# delete stream
	}

	catch {chan close $socket write}
	state_log {"" tx closed $socket $trx $sent [llength $pending]}
    }
}
