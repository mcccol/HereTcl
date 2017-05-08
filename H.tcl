# H.tcl - light Httpd 1.1

# H is designed as a collection of modules which can be assembled to form any webserver you like.

if {[info exists argv0] && ($argv0 eq [info script])} {
    # try to load the rest of Wub, if this is running as part of the ensemble of modules
    ::apply {{} {
	set home [file dirname [file normalize [info script]]]
	lappend ::auto_path $home	;#[file join [file dirname $home] Utilities] [file join [file dirname $home] extensions]
    }}
}

set ::tcl::unsupported::noReverseDNS 1	;# turn off reverse DNS

# event logging hook - default a Noop
proc ::Noop {args} {}
proc ::Identity {x} {return $x}
interp alias {} result {} ::return -level 0

# try to load the Debug module, for nice formatted debug narrative
if {[catch {
    package require Debug
}]} {
    proc ::Debug {args} {}	;# make dummy empty debug
    foreach tag {error httpd listener httpdlow httpdtx httpdtxlow entity cache cookies} {
	interp alias {} Debug.$tag {} ::Debug
    }
} else {
    Debug on error
    Debug define httpd
    Debug define process
    Debug define passthru
    Debug define listener
    Debug define httpdlow
    Debug define httpdbad
    Debug define httpdtx
    Debug define httpdtxlow
    Debug define entity
    Debug define cache
    Debug define process
    Debug define tls
}

# define [dict get?] because it's *so* useful
if {[llength [info commands ::tcl::dict::get?]] == 0} {
    # dict get? courtesy patthoyts
    proc ::tcl::dict::get? {dict args} {
	if {[dict exists $dict {*}$args]} {
	    return [dict get $dict {*}$args]
	} else {
	    return {}
	}
    }
    namespace ensemble configure ::dict -map [linsert [namespace ensemble configure ::dict -map] end get? ::tcl::dict::get?]
}

interp alias {} ::yieldm {} ::yieldto return -level 0	;# coroutine yielder

# H - take a connection and HTTP it
namespace eval H {
    variable default_port 80		;# default listener port
    variable home [file dirname [file normalize [info script]]]
    variable no_legacy 1
    proc BGERROR {lower e eo} {
	if {[dict exists $eo -errorcode]} {
	    set rest [lassign [dict get $eo -errorcode] errcode subcode]
	    switch -- $errcode {
		default {
		    puts stderr "BGERROR: '$e' ($eo)"
		}
	    }
	} else {
	    puts stderr "BGERROR: '$e' ($eo)"
	}
    }

    # corovar - used extensively to store state in the per-coro scope
    proc corovar {n} {
	uplevel 1 upvar #1 $n $n
    }

    # defaults - parse defaults structure into var value dict
    proc defaults {string} {
	set defaults {}
	foreach line [split [uplevel [list subst $string]] \n] {
	    if {[string trim $line] eq ""} continue
	    lassign [split $line \;] line
	    lappend defaults {*}[string trim $line]
	}
	return $defaults
    }

    # load - load an H package.
    variable loaded {}
    proc load {args} {
	variable home
	variable loaded
	set dir $home
	#puts stderr "load from $dir ($args)"
	foreach a $args {
	    if {[dict exists $loaded $a]} continue
	    if {[string match #* $a]} continue
	    if {[file pathtype $a] eq "relative"} {
		set file [file join $dir $a]
	    } else {
		set file $a
	    }
	    source $file
	    dict set loaded $a $file
	}

	namespace export -clear *
	namespace ensemble create -subcommands {}

	# R - namespace for all rx coroutines
	namespace eval R {
	    namespace import [namespace parent [namespace current]]::*
	}

	# T - namespace for all tx coroutines
	namespace eval T {
	    namespace import [namespace parent [namespace current]]::*
	}
    }
}

# install our own default bgerror
interp bgerror {} [list H BGERROR [interp bgerror {}]]

package provide H 8.0

# load minimal H components
H::load Hrx.tcl Htx.tcl Herr.tcl Hredir.tcl Hurl.tcl
catch {
    H::load HWS.tcl
}
H::load Hc.tcl

# more H - fill in some useful higher level functions
namespace eval H {
    # DateInSeconds - convert HTTP date to Tcl time
    proc DateInSeconds {date} {
	if {[string is integer -strict $date]} {
	    return $date
	} elseif {[catch {clock scan $date \
			-format {%a, %d %b %Y %T GMT} \
			-gmt true} result eo]
	      } {
	    #error "DateInSeconds '$date', ($result)"
	    return 0	;# oldest possible date
	} else {
	    return $result
	}
    }

    # Now - return the current time and date in HTTP format
    proc Now {} {
	return [clock format [clock seconds] -format {%a, %d %b %Y %T GMT} -gmt true]
    }

    # Date - return an HTTP date given a Tcl time
    proc Date {{seconds ""}} {
	if {$seconds eq ""} {
	    set seconds [clock seconds]
	}
	return [clock format $seconds -format {%a, %d %b %Y %T GMT} -gmt true]
    }

    # Values - handle multiple instances of a field
    proc Values {R key {delim ""}} {
	if {![dict exists $R $key]} {
	    return {}
	}
	if {[dict exists $R -Header multiple $key]} {
	    set values [dict get $R $key]
	} else {
	    set values [list [dict get $R $key]]
	}

	if {$delim eq ""} {
	    return $values	;# return undelimited fields
	}

	# return delimited fields
	set result {}
	foreach val $values {
	    foreach el [split $val $delim] {
		lappend result [string trim $el]
	    }
	}

	return $result
    }

    # rxCORS - respond to CORS request with 
    proc rxCORS {r} { 
	if {[dict get $r -Header method] eq "OPTIONS"
	    && [dict exists $r access-control-request-method]} {
	    # simplistic CORS response
	    dict set r -reply access-control-allow-origin *
	    dict set r -reply access-control-allow-methods "POST, GET, OPTIONS"
	    dict set r -reply access-control-max-age 1000
	    dict set r -reply access-control-allow-headers *
	    dict set r -reply -code 200

	    #tx_$socket $r	;# send the CORS response

	    return -code return $r	;# no more processing
	}
    }
    
    proc rdump {r} {
	dict set r -reply -content <elided>
	return $r
    }

    # Cache - HTTP contents may be Cached
    proc Cache {rq {age 0} {realm ""}} {
	dict update rq -reply rsp {
	    if {[string is integer -strict $age]} {
		# it's an age
		if {$age != 0} {
		    dict set rsp expires [Date [expr {[clock seconds] + $age}]]
		    Debug.caching {Http Cache: numeric age expires '[dict get $rsp expires]'}
		} else {
		    Debug.caching {Http Cache: turn off expires}
		    catch {dict unset rsp expires}
		    catch {dict unset rsp -expiry}
		}
	    } else {
		dict set rsp -expiry $age	;# remember expiry verbiage for caching
		dict set rsp expires [Date [clock scan $age]]
		Debug.caching {Http Cache: text age expires '$age' - '[dict get $rsp expires]'}
		set age [expr {[clock scan $age] - [clock seconds]}]
	    }

	    if {$realm ne ""} {
		dict set rsp cache-control $realm
	    }

	    if {$age} {
		if {[dict exists $rsp cache-control]} {
		    dict append rsp cache-control ",max-age=$age"
		} else {
		    dict set rsp cache-control "max-age=$age"
		}
	    }
	}

	Debug.caching {Http Cache: ($age) cache-control: [dict get? $rq -reply cache-control]}
	return $rq
    }

    # NoCache - HTTP contents may not be Cached
    proc NoCache {{rsp {}}} {
	if {[dict exists $rsp -reply]} {
	    dict set rsp -reply [NoCache [dict get $rsp -reply]]
	    return $rsp
	}
	dict set rsp cache-control "no-store, no-cache, must-revalidate, max-age=0, post-check=0, pre-check=0"; # HTTP/1.1
	dict set rsp expires "Sun, 01 Jul 2005 00:00:00 GMT"	;# deep past
	dict set rsp pragma "no-cache"	;# HTTP/1.0
	return $rsp
    }

    # NotFound - construct an HTTP NotFound response
    proc NotFound {rq args} {
	if {[llength $args]%2} {
	    set message [lindex $args end]
	    set args [lrange $args 0 end-1]
	} else {
	    set message "<P>Not Found</P>"
	}

	dict update rq -reply rsp {
	    if {[info exists rsp]} {
		set rsp [dict merge $rsp $args]
	    } else {
		set rsp $args
	    }
	    set rsp [NoCache $rsp]
	    dict set rsp -content $message
	    dict set rsp -code 404
	}
	return $rq
    }

    # NotModified - construct an HTTP NotModified response
    proc NotModified {rq args} {
	if {[llength $args]} {
	    dict set rq -reply [dict merge [dict get $rq -reply] $args]
	}
	dict update rq -reply rsp {
	    if {[info exists rsp]} {
		# the response MUST NOT include other entity-headers
		# than Date, Expires, Cache-Control, Vary, Etag, Content-Location
		set rsp [dict filter $rsp script {k v} {
		    expr {$k ni {date expires cache-control vary etag content-location} && ![string match -* $key]}
		}]
	    }
	    dict set rsp -code 304
	}

	return $rq
    }

    # Forbidden - construct an HTTP Forbidden response
    proc Forbidden {rq args} {
	if {[llength $args]%2} {
	    set message [lindex $args end]
	    set args [lrange $args 0 end-1]
	} else {
	    set message "<P>Forbidden</P>"
	}

	dict update rq -reply rsp {
	    if {[info exists rsp]} {
		set rsp [dict merge $rsp $args]
	    } else {
		set rsp $args
	    }
	    set rsp [NoCache $rsp]
	    dict set rsp -content $message
	    dict set rsp -code 403
	}
	return $rq
    }

    # construct an HTTP Bad response
    proc Bad {rq message {code 400} args} {
	Debug.httpdbad {BAD: $message $code ($rq) caller:'[info level -1]'}
	if {[dict exists $rq -Header full]} {
	    dict update rq -reply rsp {
		if {![info exists rsp]} {
		    set rsp {}
		}

		dict set rsp -content $message
		dict set rsp -code $code
		set rsp [dict merge $rsp $args]
		set rsp [NoCache $rsp]
	    }
	    [dict get $rq -tx] TxReply $rq BAD {*}$args
	} else {
	    # this isn't even HTTP - don't bother with the Tx
	}

	return -code error -errorcode [list HTTP $code] $message
    }

    proc Reply {rq args} {
	if {[llength $args]%2} {
	    set content [lindex $args end]
	    set args [lrange $args 0 end-1]
	    dict set args -content $content
	}
	dict set rq -reply [dict merge [dict get? $rq -reply] $args]
	return $rq
    }

    # Ok - construct an HTTP Ok response
    proc Ok {rq args} {
	if {[llength $args]%2} {
	    set content [lindex $args end]
	    set args [lrange $args 0 end-1]
	}

	dict update rq -reply rsp {
	    if {[info exists rsp]} {
		set rsp [dict merge $rsp $args]
	    } else {
		set rsp $args
	    }

	    if {[dict exists $rsp -code]} {
		set code [dict get $rsp -code]
	    } else {
		set code 200
	    }
	    dict set rsp -code $code

	    if {[info exists content]} {
		dict set rsp -content $content
		if {![dict exists $rsp content-length]} {
		    dict set rsp content-length [string length $content]
		}
	    }
	}

	return $rq
    }

    # TimeOut - request timeout
    proc TimeOut {rq args} {
	tailcall Ok $rq -code 408 {*}$args
    }

    # copydone - end of passthru
    proc copydone {coro input output dir args} {
	Debug.passthru {$input->$output PASSTHRU DONE: $dir $args input:([sstate $input]) / output: ([sstate $output])}
	try {
	    chan close $input read
	} on error {e eo} {
	    puts stderr "closing $input input '$e' ($eo)"
	}
	try {
	    chan flush $output
	    chan close $output write
	} on error {e eo} {
	    puts stderr "closing $output output '$e' ($eo)"
	}

	Debug.passthru {$input->$output PASSTHRU DONE2: $dir $args input:([sstate $input]) / output: ([sstate $output])}
    }

    # coroVars - debug coro
    proc coroVars {args} {
	if {![llength $args]} {
	    set args [uplevel #1 {info vars}]
	}

	#puts stderr "CV [info coroutine] $args"
	set result {}
	foreach var [lsort $args] {
	    lappend result {*}[::apply {{var} {
		upvar #1 $var __XXX
		set result {}
		try {
		    dict set result $var [set __XXX]
		} on error {e eo} {
		    if {[dict get $eo -errorcode] eq "TCL READ VARNAME"} {
			dict set result ${var}() [array get __XXX]
		    } else {
			Debug.error {coroVars Error '$e' ($eo)}
		    }
		}
		return $result
	    }} $var]
	}
	return $result
    }

    proc coroDump {coro} {
	if {$coro eq [info coroutine]} {
	    set vars [coroVars]
	} else {
	    set vars [$coro coroVars]
	}
	set result {}
	foreach {n v} $vars {
	    lappend result <td>$n</td><td>$v</td>
	}
	return <table><tr>[join $result </tr>\n<tr>]</tr></table>
    }

    proc getCV {coro args} {
	if {$coro eq [info coroutine]} {
	    set result [coroVars {*}$args]
	} else {
	    set result [$coro coroVars {*}$args]
	}
	if {[llength $args] == 1} {
	    set result [lindex $result 1]
	}
	return $result
    }

    # sstate - return socket state
    proc sstate {socket} {
	if {$socket ni [chan names] || [catch {eof $socket} eof]} {
	    return [list socket $socket eof 1 in -1 out -1 state defunct]
	} else {
	    set inp [chan pending input $socket]
	    set outp [chan pending output $socket]
	    set ev [if {$inp != -1} {llength [chan event $socket readable]} else {result -1}]
	    if {$ev == 0} {
		set state idle
	    } elseif {$ev == -1} {
		set state unreadable
	    } else {
		set state live
	    }
		
	    return [list socket $socket eof $eof in $inp out $outp ev $ev state $state]
	}
    }

    proc socketDump {} {
	variable sockets
	dict for {s v} $sockets {
	    set sstate [sstate $s]
	    catch {unset in}
	    catch {unset out}
	    dict with sstate {}
	    if {$state eq "defunct"} {
		lappend result "<p>HTTP $s DEFUNCT</p>"
		dict unset sockets $s
		continue
	    }
	    set line "HTTP $s ($state) eof:$eof listening:[expr {($ev <= 0)?0:1}]"

	    set outD {}
	    dict with v {}
	    if {[info exists input]} {
		lassign $input ic iop
		append line " Rx:"
		try {
		    set inD [getCV $ic Trace]
		    append line <a href='$ic'>$ic</a> " "
		} on error {e eo} {
		    set inD {}
		}
	    } else {
		set inD {}
	    }
	    append line "$iop $in"

	    if {[info exists output]} {
		lassign $output oc oop
		append line " Tx:"
		try {
		    set outD [getCV $oc Trace]
		    append line <a href='$oc'>$oc</a> " "
		} on error {e eo} {
		    set outD {}
		}
	    } else {
		set outD {}
	    }
	    append line "$oop $out"

	    append line " " [dict get? [chan configure $s] -peername]
	    lappend result <p>$line</p>

	    set table {}
	    dict for {t u} $inD {
		set line ""
		append line <td>$t</td> <td>[lindex [split $u] 1]</td>
		if {[dict exists $outD $t]} {
		    append line <td>[join [dict get $outD $t] </td><td>]</td>
		}
		lappend table $line
	    }
	    if {[llength $table]} {
		lappend result <table>

		lappend result <thead>
		lappend result <tr><th>[join {"" url code type length} </th><th>]</th></tr>
		lappend result </thead>

		lappend result <tbody>
		lappend result <tr>[join $table </tr>\n<tr>]</tr>
		lappend result </tbody>
		lappend result </table>
	    }
	}
	foreach chan [chan names] {
	    if {[string match sock* $chan] && ![dict exists $sockets $chan]} {
		set config [chan configure $chan]
		if {![dict exists $config -peername]} {
		    lappend result <p>Listener $chan [dict get? $config -sockname]</p>
		} else {
		    lappend result <p>Other [sstate $chan] - $config</p>
		}
	    }
	}
	return [join $result \n]
    }

    proc corodead {direction socket coro args} {
	variable sockets

	if {$socket ni [chan names]} {
	    catch {dict unset sockets $s}
	} else {
	    dict set sockets $socket $direction $coro DEAD
	}
    }

    proc Trace {op} {
	variable sockets
	corovar socket
	corovar direction
	dict set sockets $socket $direction [info coroutine] $op
    }

    # access_log - write an 
    proc access_log {r} {
	corovar access_log_fd
	corovar ipaddr
	set time [clock format [expr {[dict get $r -time]/1000}] -format {%d/%b/%Y:%T %z}]
	set request [dict get? $r -Header full]
	set code [dict get $r -reply -code]
	set size [expr {[dict exists $r -reply -size]?[dict get $r -reply -size]:0}]
	set referer [dict get? $r referer]
	set agent [dict get? $r user-agent]
	set logline [format {%s - - [%s] "%s" %d %d "%s" "%s"} $ipaddr $time $request $code $size $referer $agent]
	puts $access_log_fd $logline
	return $logline
    }

    # Pipeline - listener passes control here, with a new socket
    # this is where the action happens
    proc Pipeline {opts socket ipaddr rport} {
	try {
	    Debug.listener {Pipeline $opts $socket $ipaddr $rport}
	    if {[dict exists $opts tls] && [dict size [dict get $opts tls]]} {
		Debug.tls {TLS local: [::tls::status -local $socket]}
		Debug.tls {TLS remote: [::tls::status $socket]}
	    }

	    # set up socket encoding and translation - it should never change
	    chan configure $socket -encoding binary -translation {binary binary} -buffering none

	    # construct Rx args from defaults
	    set rx [list ipaddr $ipaddr rport $rport {*}$opts socket $socket]
	    if {[dict exists $opts rx]} {
		set rxx [dict get $opts rx]
		dict unset opts rx
		set rx [dict merge $rxx $rx]	;# listener can pass in defaults
	    }
	    # ensure there's a viable entity path
	    if {[dict exists $rx entitypath] && [dict get $rx entitypath ne ""]} {
		set entitypath [file normalize [dict get $rx entitypath]]
		file mkdir [file dirname $entitypath]
		dict set rx entitypath $entitypath
	    }

	    # construct Tx args from defaults
	    set tx [list ipaddr $ipaddr rport $rport {*}$opts socket $socket]
	    if {[dict exists $opts tx]} {
		set txx [dict get $opts tx]
		dict unset opts tx
		set tx [dict merge $tx $txx]	;# listener can pass in defaults
	    }

	    # allow listener to specify coro namespace
	    # - it will have to have T and R sub-namespaces
	    if {[dict exists $opts namespace]} {
		set namespace [dict get $opts namespace]
		dict unset opts namespace
	    } else {
		set namespace [namespace current]	;# default namespace is H
	    }

	    # coroutine names (allow listener to override the namespace)
	    set Tx ${namespace}::T::$socket
	    set Rx ${namespace}::R::$socket

	    # create a coro for rx one for tx, arrange for the socket to close respective half on termination
	    variable sockets
	    ::coroutine $Tx $namespace Tx {*}$tx rx $Rx	direction output;# create Tx coro around H::Tx command
	    dict set sockets $socket output $Tx start
	    trace add command $Tx delete [list H::corodead output $socket]

	    ::coroutine $Rx $namespace Rx {*}$rx tx $Tx	direction input;# create Rx coro around H::Rx command
	    dict set sockets $socket input $Rx start
	    trace add command $Rx delete [list H::corodead input $socket]

	    # from this point on, the coroutines have control of the socket
	} on error {e eo} {
	    if {[dict exists $eo -debug]} {
		set elevel [dict get $eo -debug]
	    } else {
		set elevel 0
	    }

	    Debug.error {Pipeline Failure: $e ($eo) opts:$opts socket:$socket ipaddr:$ipaddr rport:$rport} $elevel
	} finally {
	}
    }

    proc tls_debug {args} {
	Debug.tls {$args}
    }

    # listen - on nominated port
    proc listen {args} {
	if {[llength $args]%2} {
	    set port [lindex $args end]
	    set args [lrange $args 0 end-1]	;# port tagged on the end of args
	    dict set args port $port
	} elseif {[dict exists $args port]} {
	    set port [dict get $args port]	;# passed in port
	} else {
	    variable default_port
	    set port $default_port	;# no port specified - go default
	}
	dict set args listening $port	;# remember which port we're listening on

	# look for tls opts
	if {[dict exists $args tls] && [dict size [dict get $args tls]]} {
	    # do something with TLS
	    # Generating SSL key:
	    #  openssl genrsa -out server-private.pem 1024
	    #  openssl req -new -x509 -key server-private.pem -out server-public.pem -days 365 

	    package require tls
	    set tls [dict merge [subst {
		-certfile [file join [dict get $args home] CA server-public.pem]
		-keyfile [file join [dict get $args home] CA server-private.pem]
		-cadir [file join [dict get $args home] CA]
		-require 0
		-request 0
		-ssl3 0
		-ssl2 0
		-tls1 1
		-tls1.1 1
		-tls1.2 1
	    }] [dict get $args tls]]
	    dict set tls -server 1
	    dict set tls -command ::H::tls_debug
	    Debug.tls {TLS '[::tls::version]': ($tls) [incr ::tls::debug]}
	    ::tls::init {*}$tls 
	    set socket_cmd ::tls::socket

	    set protocols {}
	    foreach protocol {ssl2 ssl3 tls1 tls1.1 tls1.2} {
		try {
		    set ciphers [::tls::ciphers $protocol]
		} on error {e eo} {
		} on ok {} {
		    lappend protocols $protocol
		}
	    }
	    Debug.tls {TLS supports protocols: $protocols}
	} else {
	    set socket_cmd ::socket
	}

	# provide access_log_fd to Tx
	if {[dict exists $args access_log]} {
	    set access_log [dict get $args access_log]
	    set access_log_fd [open $access_log a]
	    dict unset args access_log
	} elseif {[dict exists $args access_log_fd]} {
	    set access_log_fd [dict get $args access_log_fd]
	    dict unset args access_log_fd
	} else {
	    set access_log [file join [pwd] access-$port.log]
	    set access_log_fd [open $access_log a]
	}
	dict set args access_log_fd $access_log_fd

	# start the listener
	Debug.listener {server listening ([namespace code [list Pipeline $args]]) [dict filter $args key -*] $port}
	try {
	    set socket [$socket_cmd -server [namespace code [list Pipeline $args]] {*}[dict filter $args key -*] $port]
	} trap {POSIX EADDRINUSE} {e eo} {
	    puts stderr "LISTENER ERROR: [dict filter $args key -*] port $port is already in use"
	    exit
	} on error {e eo} {
	    puts stderr "LISTENER ERROR: '$e' ($eo)"
	} finally {
	    return $socket	;# return the socket, permitting more intervention
	}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}

    namespace eval R {
	namespace import [namespace parent]::*
    }
    namespace eval T {
	namespace import [namespace parent]::*
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
}
