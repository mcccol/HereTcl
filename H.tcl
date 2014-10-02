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
interp alias {} ::H::state_log {} ::Noop
proc ::Identity {x} {return $x}

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
    Debug define listener
    Debug define httpdlow
    Debug define httpdbad
    Debug define httpdtx
    Debug define httpdtxlow
    Debug define entity
    Debug define cache
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
    proc load {args} {
	variable home
	set dir $home
	#puts stderr "load from $dir ($args)"
	foreach a $args {
	    if {[string match #* $a]} continue
	    if {[file pathtype $a] eq "relative"} {
		set a [file join $dir $a]
	    }
	    source $a
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
H::load Hrx.tcl Htx.tcl Hproc.tcl Herr.tcl

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
	dict set rsp cache-control "no-store, no-cache, must-revalidate, max-age=0, post-check=0, pre-check=0"; # HTTP/1.1
	dict set rsp expires "Sun, 01 Jul 2005 00:00:00 GMT"	;# deep past
	dict set rsp pragma "no-cache"	;# HTTP/1.0
	return $rsp
    }

    # NotFound - construct an HTTP NotFound response
    proc NotFound {rq {message "<P>Not Found</P>"}} {
	dict update rq -reply rsp {
	    set rsp [NoCache $rsp]
	    dict set rsp -content $message
	    dict set rsp -code 404
	}
	return $rq
    }

    # NotModified - construct an HTTP NotModified response
    proc NotModified {rq} {
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

    # construct an HTTP Bad response
    proc Bad {rq message {code 400} args} {
	#puts stderr "BAD: $message $code ($rq)"

	Debug.httpdbad {BAD: $message $code ($rq)}
	if {[dict exists $rq -Header full]} {
	    dict update rq -reply rsp {
		if {![info exists rsp]} {
		    set rsp {}
		}

		dict set rsp -content <p>[H armour $message]</p>
		dict set rsp -code $code
		set rsp [dict merge $rsp $args]
		set rsp [NoCache $rsp]
	    }
	    [dict get $rq -tx] reply $rq
	} else {
	    # this isn't even HTTP - don't bother with the Tx
	}
	return -code error -errorcode [list HTTP $code] $message
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

    # Pipeline - listener passes control here, with a new socket
    # this is where the action happens
    proc Pipeline {opts socket ipaddr rport} {
	try {
	    Debug.listener {Pipeline $opts $socket $ipaddr $rport}
	    state_log {"" pipeline connect $socket $ipaddr $rport}

	    # look for tls opts
	    if {[dict exists $opts tls] && [dict size [dict get $opts tls]]} {
		# do something with TLS
		package require tls
		set tls [dict merge {
		    -certfile server-public.pem
		    -keyfile server-private.pem
		    -cadir .
		    -cafile ca.pem
		    -ssl2 0
		    -ssl3 1
		    -tls1 1
		    -require 0
		    -request 1} [dict get $opts tls]]
		tls::import $socket {*}$tls		;# graft the TLS connection on socket
		tls::handshake $socket		;# start the TLS handshake
	    }

	    # set up socket encoding and translation - it should never change
	    chan configure $socket -encoding binary -translation {binary binary} ;#-blocking 0

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
	    ::coroutine $Rx $namespace Rx {*}$rx tx $Tx	;# create Rx coro around H::Rx command
	    ::coroutine $Tx $namespace Tx {*}$tx rx $Rx	;# create Tx coro around H::Tx command

	    # from this point on, the coroutines have control of the socket
	} on error {e eo} {
	    Debug.error {Pipeline Failure: $e ($eo) opts:$opts socket:$socket ipaddr:$ipaddr rport:$rport}
	} finally {
	}
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

	# start the listener
	Debug.listener {server listening ([namespace code [list Pipeline $args]]) [dict filter $args key -*] $port}
	try {
	    set socket [::socket -server [namespace code [list Pipeline $args]] {*}[dict filter $args key -*] $port]
	} trap {POSIX EADDRINUSE} {e eo} {
	    puts stderr "LISTENER ERROR: [dict filter $args key -*] port $port is already in use"
	    exit
	} on error {e eo} {
	    puts stderr "LISTENER ERROR: '$e' ($eo)"
	} finally {
	    return $socket
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
