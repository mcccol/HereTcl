# HWebSocket - H WebSocket processor
package require Debug

Debug define H.websocket

package require sha1
package provide HWS 2.0

namespace eval ws {

    # finished_message - when a complete and defragmented message has arrived, process it.
    proc finished_message {frame} {
	corovar chan
	if {[dict get $frame opcode] == 1} {
	    dict set frame payload [encoding convertto utf-8 [dict get $frame payload]]
	}
	Debug.H.websocket {[info coroutine] WebSocket finished message ($frame)}
	Chan incoming $chan [dict get $frame payload]
    }

    # process_frame - having received a frame, and its payload, switch on opcode or continue to aggregate fragments
    proc process_frame {frame payload} {
	corovar message
	corovar chan

	Debug.H.websocket {[info coroutine] WebSocket process_frame done ($frame) '[binary encode hex $payload]'}
	dict with frame {}
	switch -- $opcode {
	    0 {
		# denotes a continuation frame
		dict append message payload $payload
		if {[dict get $message fin]} {
		    if {[dict get $message type] eq "text"} {
			dict set message payload [encoding convertto utf-8 [dict get $message payload]]
		    }

		    try {
			corovar wsprocess; {*}$wsprocess [dict get $message type] $message
		    } trap {TCL LOOKUP COMMAND} {e eo} {
			# processor has gone away
			Debug.error {WS Processor '$wsprocess' has died on [dict get $message type] ($frame)}
		    } on error {e eo} {
			Debug.error {WS Error on [dict get $message type] '$e' ($eo) frame:($frame)}
		    } finally {
			unset message
		    }
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
		if {$fin} {
		    if {$opcode == 1} {
			dict set frame payload [encoding convertto utf-8 $payload]
			dict set frame type text
		    } else {
			dict set frame payload $payload
			dict set frame type binary
		    }
		    try {
			corovar wsprocess; {*}$wsprocess [dict get $frame type] $frame
		    } trap {TCL LOOKUP COMMAND} {e eo} {
			# processor has gone away
			Debug.error {WS Processor '$wsprocess' has died on [dict get $frame type] ($frame)}
		    } on error {e eo} {
			Debug.error {WS Error [dict get $frame type] '$e' ($eo) frame:($frame)}
		    }
		} elseif {![info exists message]} {
		    set message $frame

		    if {$opcode == 1} {
			dict set message type text
		    } else {
			dict set message type binary
		    }
		    dict set message payload $payload
		} else {
		    error "Received new text/binary frame inside fragmented message frame:($frame) message:($message)"
		}
	    }
	    
	    3 - 4 - 5 - 5 - 7 {
		# reserved for further non-control frames
		try {
		    corovar wsprocess; {*}$wsprocess op $opcode $frame
		} trap {TCL LOOKUP COMMAND} {e eo} {
		    # processor has gone away
		    Debug.error {WS Processor '$wsprocess' has died on $opcode ($frame)}
		} on error {e eo} {
		    Debug.error {WS Error on $opcode '$e' ($eo) frame:($frame)}
		}
	    }
	    
	    8 {
		# denotes a connection close
		# if we have sent a close - we just close
		# if we have not sent a close - we must send a close in response
		corovar socket
		if {[chan pending output $socket] != -1} {
		    Debug.H.websocket {[info coroutine] WebSocket closed by remote ($frame) [binary encode hex [dict get $frame header]]}
		    puts -nonewline $socket [binary format c 0x88][data [binary format S 1000] "Server Closed"]; chan flush $socket
		    catch {chan close $socket write}
		} else {
		    # the other side has closed - so now we close too
		    Debug.H.websocket {[info coroutine] WebSocket close acknowledged by remote [chan pending input $socket]}
		    catch {chan close $socket}
		}

		try {
		    corovar wsprocess; {*}$wsprocess closed $frame
		} trap {TCL LOOKUP COMMAND} {e eo} {
		    # processor has gone away - this is ok
		    Debug.error {WS Processor '$wsprocess' has died on close ($frame)}
		} on error {e eo} {
		    Debug.error {WS Error close '$e' ($eo) frame:($frame)}
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
		    try {
			binary format cca* 0x8A [dict get $frame ll] $payload
		    } on ok {pong} {
			puts -nonewline $socket $pong
		    } on error {e eo} {
			Debug.error {[info coroutine] WebSocket erroneous PING received ($frame) '$e' ($eo)}
		    }
		}

		try {
		    corovar wsprocess; {*}$wsprocess ping $frame
		} trap {TCL LOOKUP COMMAND} {e eo} {
		    # processor has gone away
		    Debug.error {WS Processor '$wsprocess' has died on ping ($frame)}
		} on error {e eo} {
		    Debug.error {WS Error ping '$e' ($eo) frame:($frame)}
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
		try {
		    corovar wsprocess; {*}$wsprocess pong $frame
		} trap {TCL LOOKUP COMMAND} {e eo} {
		    # processor has gone away
		    Debug.error {WS Processor '$wsprocess' has died on pong ($frame)}
		} on error {e eo} {
		    Debug.error {WS Error pong '$e' ($eo) frame:($frame)}
		}
	    }

	    B - C - D - E - F {
		# reserved for further control frames
		try {
		    corovar wsprocess; {*}$wsprocess control $opcode $frame
		} trap {TCL LOOKUP COMMAND} {e eo} {
		    # processor has gone away
		    Debug.error {WS Processor '$wsprocess' has died on $opcode ($frame)}
		} on error {e eo} {
		    Debug.error {WS Error $opcode '$e' ($eo) frame:($frame)}
		}
	    }

	    default {
		error "Invalid frame opcode [dict get $frame opcode] ($frame)"
	    }
	}
    }

    # read_frame - read input from $socket, assembling a websocket frame.
    # upon completion, return a dict of frame fields, else return {}
    proc read_frame {socket} {
	if {[catch {chan eof $socket} eof] || $eof} {return {}}

	corovar incoming	;# container for current incoming frame
	if {![info exists incoming(state)]} {
	    set incoming(state) ""
	    set incoming(req) 2
	    set incoming(offset) 0
	    Debug.H.websocket {[info coroutine] WebSocket read_frame initialized ([array get incoming])}
	}
	set in [chan read $socket $incoming(req)]
	Debug.H.websocket {[info coroutine] WebSocket read_frame read '[binary encode hex $in]' [string length $in]: [array get incoming]}
	corovar stream; append stream $in

	#Debug.H.websocket {$incoming(req) > 0 && $incoming(offset)+$incoming(req) <= [string length $stream]}
	while {$incoming(req) > 0 && $incoming(offset)+$incoming(req) <= [string length $stream]} {
	    switch -- $incoming(state) {
		"" {
		    # initial frame header
		    set r1 [binary scan [string range $stream 0 1] B4X1h1B1X1cu1 bits incoming(opcode) incoming(masked) incoming(len)]
		    lassign [split $bits ""] incoming(fin) incoming(rsv1) incoming(rsv2) incoming(rsv3)
		    Debug.H.websocket {[info coroutine] WebSocket read_frame decoding $r1 [binary encode hex $stream]: [array get incoming]}
		    if {$incoming(rsv1) || $incoming(rsv2) || $incoming(rsv3)} {
			error "[info coroutine] WebSocket $bits has reserved bits set"
		    }
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
			if {$incoming(masked)} {
			    set incoming(state) mask
			    set incoming(req) 4
			} else {
			    set incoming(state) payload
			    set incoming(req) 0
			    unset incoming(masked)
			}
		    }

		    Debug.H.websocket {[info coroutine] WebSocket read_frame decoded $r1 [binary encode hex $stream]: [array get incoming]}
		}

		paylen16 {
		    set input [string range $stream $incoming(offset) end]
		    append incoming(ll) $input	;# keep the length stream in case we have a ping
		    binary scan $input Su1 num
		    incr incoming(offset) 2

		    #set incoming(len) [expr {$incoming(len) << 16 | $num}]
		    set incoming(len) $num
		    if {$incoming(masked)} {
			set incoming(state) mask
			set incoming(req) 4
		    } else {
			set incoming(state) payload
			set incoming(req) 0
			unset incoming(masked)
		    }
		    Debug.H.websocket {[info coroutine] WebSocket read_frame paylen16 [binary encode hex $stream]: [array get incoming]}
		}

		paylen64 {
		    set input [string range $stream $incoming(offset) end]
		    append incoming(ll) $input	;# keep the length stream in case we have a ping
		    binary scan $input W1 num
		    incr incoming(offset) 8

		    #set incoming(len) [expr {$incoming(len) << 64 | $num}]
		    set incoming(len) $num
		    if {$incoming(masked)} {
			set incoming(state) mask
			incr incoming(req) 4
		    } else {
			set incoming(state) payload
			set incoming(req) 0
			unset incoming(masked)
		    }
		    Debug.H.websocket {[info coroutine] WebSocket read_frame paylen64 [binary encode hex $stream]: [array get incoming]}
		}

		mask {
		    if {[binary scan [string range $stream $incoming(offset) end] cu4 incoming(mask)] != 1} {
			error "[info coroutine] WebSocket read_frame mask could not read mask-long '[binary encode hex $stream]': [array get incoming"
		    }
		    incr incoming(offset) 4

		    set incoming(state) payload
		    set incoming(req) 0
		    Debug.H.websocket {[info coroutine] WebSocket read_frame mask '[binary encode hex $stream]': [array get incoming]}
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

    # active - loop around getting frames and payloads - in the Rx coroutine, which has no other function now
    proc active {R} {
	corovar socket; chan configure $socket -translation binary -translation binary -blocking 0 -buffering none

	corovar frame; set frame {}
	corovar payload; set payload ""	;# this is the payload of the current frame
	Readable $socket [info coroutine] frame
	set what {}

	try {
	    corovar wsprocess; {*}$wsprocess open
	} trap {TCL LOOKUP COMMAND} {e eo} {
	    # processor has gone away
	    Debug.error {WS Processor '$wsprocess' has died on open}
	} on error {e eo} {
	    Debug.error {WS Error open '$e' ($eo)}
	}

	while {1} {
	    if {[catch {chan eof $socket} eof] || $eof} return

	    set rest [lassign [::yieldm {*}$what] op]			;# wait for event
	    set what {}
	    switch -- $op {
		coroVars {
		    set what [::H coroVars {*}$rest]
		}
		send {
		    send {*}$rest	;# call the send-data command
		}
		close {
		    incr close
		}
		frame {
		    # reading the frame
		    set frame [read_frame $socket]

		    if {[dict size $frame]} {
			# valid(ish) frame
			if {[dict get $frame len]} {
			    dict set frame req [dict get $frame len]	;# we want this much payload
			    Readable $socket [info coroutine] payload	;# move to payload reading state
			    Debug.H.websocket {[info coroutine] WebSocket active read ($frame)}
			} else {
			    # we have 0 length payload - keep reading frames while processing this one
			    process_frame $frame ""
			    Debug.H.websocket {[info coroutine] WebSocket active read ($frame)}
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
		    Debug.H.websocket {[info coroutine] WebSocket payload ($frame)}

		    if {[dict get $frame req]} {
			# keep reading payload, there's more to come
		    } else {
			# have read complete payload - go back to reading frame
			if {[dict exists $frame mask]} {
			    # get mask bytes
			    set mask [dict get $frame mask]
			    binary scan $payload cu* payload
			    Debug.H.websocket {[info coroutine] WebSocket payload mask '$mask' ($frame) payload: ($payload)}
			    set i -1
			    set payload [binary format c* [lmap p $payload {
				set i [expr {($i+1) % 4}]
				expr {$p ^ [lindex $mask $i]}
			    }]]
			    Debug.H.websocket {[info coroutine] WebSocket payload [binary encode hex $payload] '$payload' ($frame)}
			} else {
			    Debug.H.websocket {[info coroutine] WebSocket payload no mask ($frame)}
			}

			Readable $socket [info coroutine] frame
			process_frame $frame $payload	;# process received frame
			unset payload
		    }
		}

		default {
		    Debug.error {[info coroutine] WebSocket got '$op' op.  with ($rest)}
		}
	    }
	}
    }

    # data - return length + data
    proc data {args} {
	set data [join $args ""]
	set len [string length $data]

	if {$len < 126} {
	    set frame [binary format c $len]
	} elseif {$len < 65536} {
	    set frame [binary format cS 126 $len]
	} else {
	    set frame [binary format cW 127 $len]
	}
	append frame $data
	return $frame
    }

    # send data to connected websocket from anywhere (including outside the ws coro)
    proc wsSend {socket {data ""} {binary 0}} {
	if {$binary ne "" && $binary} {
	    # binary content
	    set frame [binary format c 0x82]
	} else {
	    # text content
	    set frame [binary format c 0x81]
	    set data [encoding convertto utf-8 $data]
	}

	puts -nonewline $socket $frame[data $data]; chan flush $socket
    }

    # close connected websocket from anywhere (including outside the ws coro)
    proc wsClose {socket {data "Server Closed"} {status 1000}} {
	puts -nonewline $socket [binary format c 0x88][data [binary format S $status] $data]; chan flush $socket
	catch {chan close $socket write}	;# close the outbound - hoping the coro will detect it
    }

    # ping connected websocket from anywhere (including outside the ws coro)
    proc wsPing {socket {data ""}} {
	puts -nonewline $socket [binary format c 0x89][data $data]; chan flush $socket
    }

    # pong connected websocket from anywhere (including outside the ws coro)
    proc wsPong {socket {data ""}} {
	puts -nonewline $socket [binary format c 0x8A][data $data]; chan flush $socket
    }

    # close connected websocket
    proc Close {{data ""} {status ""}} {
	corovar socket; tailcall wsClose $socket $data $status
    }

    # ping connected websocket
    proc Ping {{data ""}} {
	corovar socket; tailcall wsPing $socket $data
    }

    # pong connected websocket
    proc Pong {{data ""}} {
	corovar socket; tailcall wsPong $socket $data
    }

    # send data to connected websocket from within the ws coro
    proc Send {{data ""} {binary 0}} {
	corovar socket; tailcall wsSend $socket $data $binary
    }

    # accept - called from user code within Hrx coro when a websocket handshake is detected
    # and the user code decides to accept it.
    #
    # running inside the RX coroutine, still has the TX coroutine to talk to
    proc accept {R} {
	corovar socket
	Debug.H.websocket {[info coroutine] WebSocket connect ($R) [chan configure $socket]}

	corovar closed; set closed 0

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
	dict update websocket -reply rsp {
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
	return $websocket
    }

    # Chan - create a refchan for the websocket interaction
    proc Chan {R} {
	corovar chan; set chan [chan create {read write} [namespace code [namespace current]::Chan]]

	set coro [namespace current]::$chan
	coroutine $coro apply [list {R chan} {
	    Debug.H.websocket {started coroutine [namespace current]::$chan}
	    chan event $chan readable [list [info coroutine] read]
	    set closing 0
	    while {![eof $chan]} {
		Debug.H.websocket {yield in coroutine [namespace current]::$chan}
		set rest [lassign [::yieldm] opcode]
		switch -- $opcode {
		    read {
			set line [gets $chan]
			if {[eof $chan]} {
			    # our side has closed $chan
			    break
			}

			Debug.H.websocket {readable in coroutine [namespace current]::$chan got '$line'}
			puts $chan $line
			flush $chan
			Debug.H.websocket {sent line in coroutine [namespace current]::$chan}
		    }
		    
		    text -
		    binary {
			# incoming text or binary data
		    }

		    pong {}
		    ping {}

		    closed {
			incr closing
			chan close $chan; break
		    }

		    default {
		    }
		    
		}
	    }
	    if {$closing} {
		# other side has closed
	    }
	} [namespace current]] $R $chan
	corovar socket; chan configure $chan -socket $socket -coro $coro
	dict set R -wschan $chan
	dict set R -ws $coro
	return $R
    }

    namespace export -clear *
    set x [namespace ensemble create -subcommands {}]

    namespace import [namespace parent]::*

    #puts stderr "WS NS: $x - [namespace import]"
}
