# H.tcl - light Httpd 1.1
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
    Debug define httpdtx
    Debug define httpdtxlow
    Debug define entity
    Debug define cache
}

# define [dict get?] because it's so useful
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

# H - take a connection and HTTP it
namespace eval H {
    variable default_port 80		;# default listener port
    variable home [file dirname [file normalize [info script]]]

    # corovar - used extensively to store state in the per-coro scope
    proc corovar {n} {
	uplevel 1 upvar #1 $n $n
    }

    # yieldm - used to coordinate coro calls
    proc yieldm {args} {
	::yieldto return -level 0 {*}$args
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

    proc load {args} {
	variable home
	set dir $home
	puts stderr "load from $dir ($args)"
	foreach a $args {
	    if {[file pathtype $a] eq "relative"} {
		set a [file join $dir $a]
	    }
	    source $a
	}

	namespace export -clear *
	namespace ensemble create -subcommands {}

	namespace eval R {
	    namespace import ::H::*
	}
	namespace eval T {
	    namespace import ::H::*
	}
    }
}

package provide H 7.0

# load the H components
foreach h {
    Hrx.tcl
    Htx.tcl
} {
    H::load $h
}
#H::load Hproc.tcl

namespace eval H {
    # construct an HTTP Ok response
    proc Ok {rsp args} {
	if {[llength $args]%2} {
	    set content [lindex $args end]
	    set args [lrange $args 0 end-1]
	}

	set rsp [dict merge $rsp $args]

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

	return $rsp
    }

    # convert HTTP date to time
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

    # return an HTTP date
    proc Date {{seconds ""}} {
	if {$seconds eq ""} {
	    set seconds [clock seconds]
	}

	return [clock format $seconds -format {%a, %d %b %Y %T GMT} -gmt true]
    }

    # contents may be Cached
    proc Cache {rsp {age 0} {realm ""}} {
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

	Debug.caching {Http Cache: ($age) cache-control: [dict get? $rsp cache-control]}
	return $rsp
    }

    # contents may not be Cached
    proc NoCache {rsp} {
	dict set rsp cache-control "no-store, no-cache, must-revalidate, max-age=0, post-check=0, pre-check=0"; # HTTP/1.1
	dict set rsp expires "Sun, 01 Jul 2005 00:00:00 GMT"	;# deep past
	dict set rsp pragma "no-cache"	;# HTTP/1.0
	return $rsp
    }

    proc NotFound {rsp {message "<P>Not Found</P>"}} {
	dict set rsp -content $message
	dict set rsp -code 404
	return [NoCache $rsp]
    }

    # construct an HTTP NotModified response
    proc NotModified {rsp} {
	# remove content-related stuff
	foreach n [dict keys $rsp content-*] {
	    if {$n ne "content-location"} {
		dict unset rsp $n
	    }
	}

	# discard some fields
	set rsp [dict filter $rsp script {k v} {
	    expr {$k ni {transfer-encoding -chunked -content}}
	}]

	# the response MUST NOT include other entity-headers
	# than Date, Expires, Cache-Control, Vary, Etag, Content-Location
	dict set result -code 304

	return $result
    }

    # Pipeline - this is where the action happens
    proc Pipeline {opts socket ipaddr rport} {
	Debug.listener {Pipeline $opts $socket $ipaddr $rport}
	state_log {"" pipeline connect $socket $ipaddr $rport}

	# construct argset for Rx and Tx
	set rx [list ipaddr $ipaddr rport $rport {*}$opts socket $socket]
	if {[dict exists $opts rx]} {
	    set rx [dict merge $rx [dict get $opts rx]]
	    dict unset opts rx
	}
	set tx $rx

	if {[dict exists $opts tx]} {
	    set tx [dict merge $tx [dict get $opts tx]]
	    dict unset opts tx
	}

	if {[dict get? $opts namespace] ne ""} {
	    set namespace [dict get $opts namespace]
	    dict unset opts namespace
	} else {
	    set namespace [namespace current]
	}

	if {[dict exists $opts tls]} {
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

	# set up the encoding and translation - it should never change
	chan configure $socket -encoding binary -translation {binary binary}

	# coroutine names
	set Tx ${namespace}::T::$socket
	set Rx ${namespace}::R::$socket

	# create a coro for rx one for tx, arrange for the socket to close respective half on termination
	::coroutine $Rx $namespace Rx {*}$rx tx $Tx
	::coroutine $Tx $namespace Tx {*}$tx	;# create Tx coro
    }

    # listen on nominated port
    proc listen {args} {
	if {[llength $args]%2} {
	    set port [lindex $args end]
	    set args [lrange $args 0 end-1]
	} elseif {[dict exists $args port]} {
	    set port [dict get $args port]
	} else {
	    variable default_port; set port $default_port
	}

	# start the listener
	Debug.listener {server listening (Pipeline $args) [dict filter $args key -*] $port}
	return [::socket -server [namespace code [list Pipeline $args]] {*}[dict filter $args key -*] $port]
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}

    namespace eval R {
	namespace import ::H::*
    }
    namespace eval T {
	namespace import ::H::*
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
}
