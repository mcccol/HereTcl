# HWebSocket - H WebSocket processor
package require Debug

Debug define websocket

package require sha1
package provide HWebSocket 1.0

namespace eval H::WebSocket {

    # finished_message - when a complete and defragmented message has arrived, process it.
    proc finished_message {frame} {
	corovar chan
	if {[dict get $frame opcode] == 1} {
	    dict set frame payload [encoding convertto utf-8 [dict get $frame payload]]
	}
	Debug.websocket {[info coroutine] WebSocket finished message ($frame)}
	Chan incoming $chan [dict get $frame payload]
    }

    # process_frame - having received a frame, and its payload, switch on opcode or continue to aggregate fragments
    proc process_frame {frame payload} {
	corovar message
	corovar chan

	Debug.websocket {[info coroutine] WebSocket process_frame done ($frame) '[binary encode hex $payload]'}
	dict with frame {}
	switch -- $opcode {
	    0 {
		# denotes a continuation frame
		dict append message payload $payload
		if {[dict get $message fin]} {
		    finished_message $message
		    unset message
		}
	    }

	    1 -
	    2 {
		# 1 denotes a text frame
		# The "Payload data" is text data encoded as UTF-8.  Note that a
		# particular text frame might include a partial UTF-8 sequence;
		# however, the whole message MUST contain valid UTF-8.
		
		# 2 denotes a binary frame
		# The "Payload data" is arbitrary binary data whose interpretation
		# is solely up to the application layer.
		if {$opcode == 1} {
		    dict set frame type text
		} else {
		    dict set frame type binary
		}
		
		if {$fin} {
		    dict set frame payload $payload
		    finished_message $frame
		} elseif {![info exists message]} {
		    set message $frame
		    dict set message payload $payload
		} else {
		    error "Received new text/binary frame inside fragmented message"
		}
	    }
	    
	    3 - 4 - 5 - 5 - 7 {
		# reserved for further non-control frames
	    }
	    
	    8 {
		# denotes a connection close
		# if we have sent a close - we just close
		# if we have not sent a close - we must send a close in response
		corovar socket
		corovar closed
		if {!$closed} {
		    Debug.websocket {[info coroutine] WebSocket closed by remote}
		    catch {puts -nonewline $socket [binary format aa \x88 \0]}
		    incr closed
		    chan close $socket write
		    Chan closed $chan
		} else {
		    # the other side has closed - so now we close too
		    Debug.websocket {[info coroutine] WebSocket close acknowledged by remote}
		    chan close $socket
		}
	    }
	    
	    9 {
		# denotes a ping - must reply with a pong frame
		# A Ping frame may serve either as a keepalive
		# or as a means to verify that the remote endpoint is still responsive.
		
		# Upon receipt of a Ping frame, an endpoint MUST send a Pong frame in
		# response, unless it already received a Close frame.  It SHOULD
		# respond with Pong frame as soon as is practical.
		
		# A Pong frame sent in response to a Ping frame must have identical
		# "Application data" as found in the message body of the Ping frame
		# being replied to.
		
		# If an endpoint receives a Ping frame and has not yet sent Pong
		# frame(s) in response to previous Ping frame(s), the endpoint MAY
		# elect to send a Pong frame for only the most recently processed Ping
		# frame.
		
		if {![info exists closed]} {
		    corovar socket
		    puts -nonewline $socket [binary format aca* \x8A [dict get $frame ll] $payload]
		}
	    }
		
	    A {
		# denotes a pong - in response to a ping frame we have sent
		# A Ping frame may serve either as a keepalive
		# or as a means to verify that the remote endpoint is still responsive.
		
		# If an endpoint receives a Ping frame and has not yet sent Pong
		# frame(s) in response to previous Ping frame(s), the endpoint MAY
		# elect to send a Pong frame for only the most recently processed Ping
		# frame.

		# A Pong frame MAY be sent unsolicited.  This serves as a
		# unidirectional heartbeat.  A response to an unsolicited Pong frame is
		# not expected.

		# we don't send pings as yet
	    }

	    B - C - D - E - F {
		# reserved for further control frames
	    }

	    default {
		error "Invalid frame opcode [dict get $frame opcode]"
	    }
	}
    }

    # read_frame - read input from $socket, assembling a websocket frame.
    # upon completion, return a dict of frame fields, else return {}
    proc read_frame {socket} {
	if {[chan eof $socket]} {return {}}

	corovar incoming	;# container for current incoming frame
	if {![info exists incoming(state)]} {
	    set incoming(state) ""
	    set incoming(req) 2
	    set incoming(offset) 0
	    Debug.websocket {[info coroutine] WebSocket read_frame initialized ([array get incoming])}
	}
	set in [chan read $socket $incoming(req)]
	Debug.websocket {[info coroutine] WebSocket read_frame read '[binary encode hex $in]' [string length $in]: [array get incoming]}
	corovar stream; append stream $in
	#Debug.websocket {$incoming(req) > 0 && $incoming(offset)+$incoming(req) <= [string length $stream]}

	while {$incoming(req) > 0 && $incoming(offset)+$incoming(req) <= [string length $stream]} {
	    switch -- $incoming(state) {
		"" {
		    # initial frame header
		    set r1 [binary scan [string range $stream 0 1] B4X1h1B1X1cu1 bits incoming(opcode) incoming(mask) incoming(len)]
		    lassign [split $bits ""] incoming(fin) incoming(rsv1) incoming(rsv2) incoming(rsv3)
		    Debug.websocket {[info coroutine] WebSocket read_frame decoding $r1 [binary encode hex $stream]: [array get incoming]}

		    set incoming(len) [expr {$incoming(len) & 0x7F}] ;# and off 'mask' bit
		    set incoming(ll) [binary format cu $incoming(len)]	;# keep the length stream in case we have a ping
		    set incoming(opcode) [string toupper $incoming(opcode)]
		    set incoming(offset) 2

		    if {$incoming(len) == 126} {
			set incoming(state) paylen16
			set incoming(req) 2
		    } elseif {$incoming(len) == 127} {
			set incoming(state) paylen64
			set incoming(req) 8
		    } else {
			set incoming(state) payload
			set incoming(req) 0
		    }

		    if {$incoming(mask)} {
			set incoming(state) mask
			incr incoming(req) 4
		    } else {
			unset incoming(mask)
		    }
		    Debug.websocket {[info coroutine] WebSocket read_frame decoding $r1 [binary encode hex $stream]: [array get incoming]}
		}

		paylen16 {
		    set input [string range $stream $incoming(offset) end]
		    append incoming(ll) $input	;# keep the length stream in case we have a ping
		    binary scan $input Su1 num
		    incr incoming(offset) 2

		    set incoming(len) [expr {$incoming(len) << 16 | $num}]
		    if {$incoming(mask)} {
			set incoming(state) mask
		    } else {
			set incoming(req) 0
			set incoming(state) payload
			unset incoming(mask)
		    }
		}

		paylen64 {
		    set input [string range $stream $incoming(offset) end]
		    append incoming(ll) $input	;# keep the length stream in case we have a ping
		    binary scan $input W1 num
		    incr incoming(offset) 8

		    set incoming(len) [expr {$incoming(len) << 64 | $num}]
		    if {$incoming(mask)} {
			set incoming(state) mask
		    } else {
			set incoming(req) 0
			set incoming(state) payload
			unset incoming(mask)
		    }
		}

		mask {
		    binary scan [string range $stream $incoming(offset) end] cu4 incoming(mask)
		    incr incoming(offset) 4

		    set incoming(req) 0
		    set incoming(state) payload
		}
		payload {
		    break
		}
	    }
	}

	if {$incoming(state) eq "payload"} {
	    set incoming(header) $stream; unset stream
	    unset incoming(req)

	    Readable $socket	;# turn off reception for a bit
	    set result [array get incoming]
	    unset incoming
	    return $result
	} else {
	    return {}
	}
    }

    # active - loop around getting frames and payloads
    proc active {R} {
	corovar socket
	chan configure $socket -translation binary -translation binary -blocking 0 -buffering none

	corovar frame; set frame {}
	corovar payload; set payload ""	;# this is the payload of the current frame
	Readable $socket [info coroutine] frame
	while {1} {
	    if {[chan eof $socket]} return

	    set rest [lassign [::yieldm] op]			;# wait for event
	    switch -- $op {
		frame {
		    # reading the frame
		    set frame [read_frame $socket]
		    if {[dict size $frame]} {
			
			if {[dict get $frame len]} {
			    Readable $socket [info coroutine] payload	;# move to payload reading state
			    dict set frame req [dict get $frame len]
			    Debug.websocket {[info coroutine] WebSocket active read ($frame)}
			} else {
			    # we have 0 length payload - keep reading frames while processing this one
			    process_frame $frame ""
			    Debug.websocket {[info coroutine] WebSocket active read ($frame)}
			}
		    } else {
			# we have yet to complete the frame
		    }
		}

		payload {
		    # reading the payload
		    set bytes [chan read $socket [dict get $frame req]]
		    append payload $bytes
		    dict incr frame req -[string length $bytes]
		    Debug.websocket {[info coroutine] WebSocket payload ($frame)}
		    if {[dict get $frame req]} {
			# keep reading payload
		    } else {
			# have read complete payload - go back to reading frame
			if {[dict exists $frame mask]} {
			    # get mask bytes
			    set mask [dict get $frame mask]
			    set i -1
			
			    binary scan $payload cu* payload
			    set payload [binary format c* [lmap p $payload {
				set i [expr {($i+1) % 4}]
				expr {$p ^ [lindex $mask $i]}
			    }]]
			}

			Readable $socket [info coroutine] frame
			process_frame $frame $payload	;# process received frame
		    }
		}

		default {
		    Debug.error {[info coroutine] WebSocket got '$op' op.  with ($rest)}
		}
	    }
	}
    }

    # chan - reflected channel
    namespace eval Chan {
	variable chan; array set chan {}

	proc read {cid count} {
	    variable chan
	    Debug.websocket {read $cid $count}
	    if {[dict get $chan($cid) closed]} {
		return ""
	    }
	    if {[dict get $chan($cid) data] eq ""} {
		error EAGAIN
	    }

	    set result [dict get $chan($cid) data]
	    dict set chan($cid) data [string range $result $count end]
	    return [string range $result 0 $count-1]
	}

	# write - 
	proc write {cid data} {
	    variable chan
	    Debug.websocket {write $cid [string length $data]}

	    set socket [dict get $chan($cid) socket]
	    switch -- [dict get $chan($cid) type] {
		binary {set opcode 1}
		text {set opcode 2}
	    }
	    set opcode [expr {$opcode | 0x80}]
	    set result [binary format c $opcode]

	    if {[dict get $chan($cid) type] eq "text"} {
		set data [encoding convertto utf-8 $data]
	    }

	    set len [string length $data]
	    if {$len < 126} {
		append result [binary format c $len]
	    } elseif {$len < 65536} {
		append result [binary format cS 126 $len]
	    } else {
		set ll 127
		append result [binary format cW 127 $len]
	    }
	    append result $data

	    puts -nonewline $socket $result

	    return [string length $data]
	}

	proc configure {cid option value} {
	    variable chan
	    Debug.websocket {configure $cid $option $value}
	    switch -- $option {
		-socket {
		    dict set chan($cid) [string trimleft $option -] $value
		}

		-binary {
		    dict set chan($cid) type binary
		}

		-text {
		    dict set chan($cid) type text
		}

		default {
		    error "option $option unknown"
		}
	    }
	}

	proc watch {cid eventspec} {
	    variable chan
	    Debug.websocket {watch $cid $eventspec}
	    if {"read" in $eventspec} {
		dict set chan($cid) readable 1
	    } else {
		dict set chan($cid) readable 0
	    }
	}

	proc finalize {cid} {
	    variable chan
	    unset chan($cid)
	    Debug.websocket {finalize $cid}
	}

	proc closed {cid} {
	    variable chan
	    Debug.websocket {closed $cid}
	    dict set chan($cid) closed 1
	    if {[dict get $chan($cid) readable]} {
		::chan postevent $cid read
	    }
	}

	# incoming - incoming data from websocket
	proc incoming {cid data} {
	    variable chan
	    Debug.websocket {incoming $cid [string length $data]}
	    dict append chan($cid) data $data
	    if {[dict get $chan($cid) readable]} {
		::chan postevent $cid read
	    }
	}

	proc initialize {cid mode} {
	    variable chan
	    set chan($cid) {closed 0 readable 0 type binary data ""}
	    Debug.websocket {initialize $cid $mode}
	    return {read write configure watch finalize initialize}
	}

	namespace export -clear *
	namespace ensemble create -subcommands {}
    }

    # create - a simple-minded dispatcher for incoming WS connections
    # this example ignores the GET data, and just makes an echo coroutine
    # it is expected this will be replaced by something more complete
    proc create {R chan} {
	coroutine [namespace current]::$chan apply [list {R chan} {
	    Debug.websocket {started coroutine [namespace current]::$chan}
	    chan event $chan readable [info coroutine]
	    while {![eof $chan]} {
		Debug.websocket {yield in coroutine [namespace current]::$chan}
		yield
		set line [gets $chan]
		Debug.websocket {readable in coroutine [namespace current]::$chan got '$line'}
		puts $chan $line
		flush $chan
		Debug.websocket {sent line in coroutine [namespace current]::$chan}
	    }
	} [namespace current]] $R $chan
    }

    # connect - called from Hrx when a websocket handshake is detected
    #
    # running inside the RX coroutine, still has the TX coroutine to talk to
    proc connect {R} {
	corovar socket
	Debug.websocket {[info coroutine] WebSocket connect ($R) [chan configure $socket]}
	corovar closed; set closed 0
	try {
	    Readable $socket	;# turn off the chan event readable

	    if {![dict exists $R sec-websocket-key]} {
		Bad $R "WebSocket requires a Sec-WebSocket-Key"
	    }
	    if {![dict exists $R sec-websocket-version]} {
		Bad $R "WebSocket requires a Sec-WebSocket-Version"
	    } elseif {[dict get $R sec-websocket-version] ne "13"} {
		Bad $R "WebSocket invalid version '[dict get $R sec-websocket-version]'" 400 sec-websocket-version 13
	    }

	    corovar websocket; set websocket $R	;# remember websocket values
	    
	    # format websocket accept
	    dict update R -reply rsp {
		if {![info exists rsp]} {
		    set rsp {}
		}
		dict set rsp upgrade websocket
		dict set rsp connection Upgrade
		dict set rsp -code 101	;# Switching Protocols

		# websocket protocols - find the values requested and see what we have to match ws_$protocol
		set protocol {}
		foreach p [string tolower [H Values $R sec-websocket-protocol ,]] {
		    if {[llength [info commands ws_$p]]} {
			lappend protocol $p
			break
		    }
		}

		if {[llength $protocol]} {
		    dict set rsp sec-websocket-protocol [join $protocol ", "]
		}
		
		# websocket extensions - find the values requested and see what we have to match wse_$extension
		set extensions {}
		foreach p [string tolower [H Values $R sec-websocket-extensions ,]] {
		    if {[llength [info commands wse_$p]]} {
			lappend extensions $p
		    }
		}
		if {[llength $protocol]} {
		    dict set rsp sec-websocket-extensions [join $extensions ", "]
		}

		# websocket accept key - construct one
		set sec [dict get $R sec-websocket-key]
		append sec 258EAFA5-E914-47DA-95CA-C5AB0DC85B11
		dict set rsp sec-websocket-accept [binary encode base64 [sha1::sha1 -bin $sec]]
	    }
	} trap HTTP {e eo} {
	    Debug.httpd {[info coroutine] Httpd $e}
	} on error {e eo} {
	    Debug.error {[info coroutine] Error '$e' ($eo)}
	    set R [ServerError $R $e $eo]
	    corovar tx; $tx reply $R		;# transmit the error
	} on ok {} {
	    Debug.httpd {[info coroutine] Dispatch: OK}

	    # send WebSocket handshake accept
	    [dict get $R -tx] reply $R	;# transmit the websocket accept

	    catch {$tx websocket}		;# inform Tx coro of websocket

	    # create a refchan for the websocket interaction
	    corovar chan; set chan [chan create {read write} [namespace code Chan]]
	    chan configure $chan -socket $socket

	    create $R $chan	;# call something to create a websocket given the connection guff

	    tailcall active $R	;# go on to activate the websocket
	}

	# we have rejected the websocket procotol - close up shop
	catch {chan close $socket read}	;# close the socket read side
	catch {$tx closing}		;# inform Tx coro that we're closing
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}

    namespace import [namespace parent]::*
}
