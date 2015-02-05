# Direct.tcl - direct translation of URL to Tcl method call
if {[info exists argv0] && ($argv0 eq [info script])} {
    # try to load the rest of Wub, if this is running as part of the ensemble of modules
    apply {{} {
	set home [file dirname [file normalize [info script]]]
	lappend ::auto_path $home
    }}
}

package require Query
package require H
package require Debug
Debug define direct

package provide Direct 2.0

oo::class create Direct {
    # Define - define a command with args for this Direct domain
    method Define {m args} {
	Debug.direct {[lindex [self] 0] method $m definition: $args}
	if {[lindex $args end] eq "args"} {
	    set needargs 1
	    set params [lrange $args 1 end-1]	;# remove 'r' and args from params
	} else {
	    set needargs 0
	    set params [lrange $args 1 end]	;# remove 'r' from params
	}

	variable methods
	Debug.direct {[lindex [self] 0] method $m record definition: [list $needargs $params]}
	dict set methods $m [list $needargs $params]
    }

    method mkMethods {} {
	Debug.direct {Methods on [self]: [info object methods [self] -all -private]}
	
	# construct a dict from method name to the formal parameters of the method
	set class [info object class [self]]
	variable methods {}
	set superclass [info class superclasses $class]
	set mixins [info class mixins $class]

	foreach m [lreverse [lsort -dictionary [info object methods [self] -private -all]]] {
	    if {[string match /* $m]} {
		if {$m in [info object methods [self] -private]} {
		    set def [lindex [info object definition [self] $m] 0]
		} else {
		    foreach class [list [info object class [self]] {*}$superclass {*}$mixins] {
			if {![set unfound [catch {
			    lindex [info class definition $class $m] 0
			} def eo]]} {
			    # defined in $class, else try next mixin
			    break
			}
		    }
		    if {$unfound} {
			error "Can't find method $m in any class of [info object class [self]] $mixins"
		    }
		}
		my Define $m {*}$def
	    }
	}

	variable passthru
	foreach m [lreverse [lsort -dictionary [info object methods [self] -private -all]]] {
	    if {[string match |* $m]} {
		lappend passthru $m
	    }
	}

	Debug.direct {[lindex [self] 0] of class $class methods: ($methods) / ([info class methods $class -private -all]) - ([info object methods [self] -all -private])}

	variable wildcard
	if {![dict exists $methods $wildcard]} {
	    error "Wildcard method '$wildcard' must exist in object. ([dict keys $methods])"
	}
    }

    # add - add a command from anywhere to this object
    method add {command} {
	Define $command {*}[info args $command]
	forward $command $command
    }

    # add_ns - add all matching commands in specified namespace
    method add_ns {{ns ""}} {
	if {$ns eq ""} {
	    set ns [uplevel 1 [list namespace current]]
	}
	foreach cmd [info commands ${ns}::/*] {
	    my add $cmd
	}
    }

    # / - default/wildcard match
    # allows daisy-chaining with $next
    method / {r} {
	variable next
	if {[info exists next]} {
	    tailcall $next do $r
	} else {
	    return [H NotFound $r]
	}
    }

    method Match {suffix {wild ""}} {
	Debug.direct {Match ($suffix)}
	set suffix [string trimleft $suffix /]
	# strip extensions from each component
	set fn $suffix
	set cprefix {}
	set fprefix {}
	foreach el [split [H armour $fn] /] {
	    lappend cprefix [file rootname $el]
	    lappend fprefix $el
	}

	set extra {}
	set cmd ""
	variable methods
	while {$cmd eq "" && [llength $cprefix]} {
	    Debug.direct {searching for ($cprefix) in ($methods)}
	    set probe [dict keys $methods /[join $cprefix /]]
	    # this strict match can only have 1 or 0 results
	    if {[llength $probe] == 1} {
		set cmd $probe
		break
	    }

	    # there's no exact match, so trim cprefix and try again.
	    set cprefix [lrange $cprefix 0 end-1]
	    lappend extra [lindex $fprefix end]	;# remember trailing
	    set fprefix [lrange $fprefix 0 end-1]
	}

	# no match - use wildcard method
	if {$cmd eq ""} {
	    Debug.direct {'$cmd' not found looking for '$fn' in ($methods)}
	    if {$wild ne ""} {
		set result [list $wild $suffix / /]
	    } else {
		variable wildcard
		set result [list $wildcard $suffix / /]
	    }
	} else {
	    set result [list $cmd [join [lreverse $extra] /] [join $fprefix /] [join $cprefix /]]
	}
	Debug.direct {Match result: $result}
	return $result
    }

    # Call - invoke command
    method Call {rsp cmd args} {
	tailcall dict merge $rsp [my $cmd $rsp {*}$args]
    }

    # Parameters - extract actual parameters from request for formal parameters in method
    method Parameters {r} {
	set cmd [dict get $r -Url cmd]

	# get the formal parameters and args-status of the method
	variable methods
	lassign [dict get $methods $cmd] needargs params

	dict set r -Query [set qd [Query parse $r]]
	
	Debug.direct {cmd:'$cmd' needargs:$needargs params:'$params' qd:[dict keys $qd]}

	set argl {}
	array set used {}
	variable complain
	foreach arg $params {
	    lassign $arg arg default
	    if {[Query exists $qd $arg]} {
		Debug.direct {param '$arg' exists}
		incr used($arg)
		if {[Query numvalues $qd $arg] > 1} {
		    Debug.direct {multiple $arg: [Query values $qd $arg]}
		    lappend argl [Query values $qd $arg]
		} else {
		    Debug.direct {single $arg: [string range [Query value $qd $arg] 0 80]...}
		    lappend argl [Query value $qd $arg]
		}
	    } elseif {$complain} {
		# complain if a named parameter does not exist
		error "Required parameter '$arg' does not exist"
	    } else {
		Debug.direct {param '$arg' does not exist}
		lappend argl $default
	    }
	}

	set argll {}
	if {$needargs} {
	    foreach {name value} [Query flatten $qd] {
		if {![info exists used($name)]} {
		    Debug.direct {args $name: [string range $value 0 80]...}
		    lappend argll $name $value
		}
	    }
	}

	return [list $argl $argll]
    }

    # Url - convert request -Url to useful components
    method Url {r} {
	if {[dict exists $r -Url extra]} {
	    set suffix [dict get $r -Url extra]	;# left behind by previous Direct
	} else {
	    set suffix [dict get $r -Url path]
	}

	# search for a matching command prefix
	foreach f {cmd extra fprefix cprefix} v [my Match $suffix] {
	    set $f $v
	    dict set r -Url $f $v
	}
	return $r
    }

    # copydone - end of passthru
    method copydone {coro socket sdir fsd fdir args} {
	Debug.direct {$socket PASSTHRU DONE: $sdir $args}
	variable copydone
	if {[info exists copydone($socket.$fsd)]} {
	    # wait until both directions have closed
	    catch {chan close $socket}
	    catch {chan close $fsd}
	    unset copydone($socket.$fsd)
	} else {
	    set copydone($socket.$fsd) 1
	}
    }

    # start passthru
    method passthru {r dsock {url ""}} {
	#puts stderr "passthru '$url'"
	if {$url eq ""} {
	    lassign [split [dict get $r -Full]] method uri version
	    set url /[join [lrange [split $uri /] 2 end] /]
	}

	chan configure $dsock -encoding binary -buffering none -blocking 0 -translation binary
	lassign [split [dict get $r -Full]] method . version
	Debug.direct {[info coroutine] PASSTHRU URI: $url}
	puts -nonewline $dsock "$method $url $version\xd\xa"

	set socket [dict get $r -socket]
	chan configure $socket -encoding binary -buffering none -blocking 0 -translation binary
	chan event $socket readable {}
	chan event $socket writable {}

	chan copy $dsock $socket -command [list [self] copydone [info coroutine] $socket write $dsock read]
	chan copy $socket $dsock -command [list [self] copydone [info coroutine] $socket read $dsock write]

	return -code error -errorcode PASSTHRU	;# abort the caller command, initiate PASSTHRU mode
    }

    method wscall {args} {
	Debug.direct {[info coroutine] [self] wscall $args}
	return [my {*}$args]
    }

    method ws {r} {
	# normal HTTP dispatch, but with WebSocket
	set r [my Url $r]	;# expand out the -Url element a bit

	Debug.direct {[info coroutine] ws $r}

	variable methods
	set cmd [dict get $r -Url cmd]
	if {![dict exists $methods $cmd] eq {}} {
	    Debug.direct {default not found looking for $cmd in ($methods)}
	    return [H NotFound $r]
	}

	variable parameters
	if {$parameters} {
	    lassign [my Parameters $r] argl argll
	} else {
	    set argl {}; set argll {}
	}

	Debug.direct {[self] calling websocket method $cmd [string range $argl 0 80]... [dict keys $argll]}
	upvar #1 wsprocess wsprocess; set wsprocess [list [self] wscall $cmd]
	dict set r -ws [list [self] wscall $cmd]	;# default to call the direct method for each WS event
	return [my $cmd connect $r {*}$argl {*}$argll]	;# perform the direct call
    }

    method do {r} {
	# try for passthru first
	if {[dict get $r -Header state] eq "Initial"} {
	    set r [H Header $r 1]
	    set full [dict get $r -Full]	;# the request hasn't been fully parsed
	} else {
	    set full [dict get $r -Header full]	;# fully parsed request
	}

	# handle passthru command, if this is one
	variable passthru
	if {[set cmd |[lindex [split [lindex [split $full] 1] /] 1]] in $passthru} {
	    Debug.direct {[info coroutine] PASSTHRU DO: $cmd}
	    try {
		set cmd [my $cmd $r]	;# |commands return [list $r $passthru_socket]
	    } on error {e eo} {
		# to report an error we have to fully parse the request
		if {[dict get $r -Header state] ne "Entity"} {
		    set r [H RxProcess $r]	;# finish processing if necessary
		}
		return [H ServerError $r $e $eo]	;# signal server error
	    } on ok {} {
		my passthru {*}$cmd
	    }
	}

	# not passthru - process headers if necessary
	if {[dict get $r -Header state] ne "Entity"} {
	    set r [H RxProcess $r]	;# finish processing if necessary
	}

	# normal HTTP dispatch
	set r [my Url $r]	;# expand out the -Url element a bit

	Debug.direct {[self] do $r}

	variable methods
	set cmd [dict get $r -Url cmd]
	if {![dict exists $methods $cmd] eq {}} {
	    Debug.direct {default not found looking for $cmd in ($methods)}
	    return [H NotFound $r]
	}

	variable parameters
	if {$parameters} {
	    lassign [my Parameters $r] argl argll
	} else {
	    set argl {}; set argll {}
	}
	
	Debug.direct {calling method $cmd [string range $argl 0 80]... [dict keys $argll]}
	
	tailcall my Call $r $cmd {*}$argl {*}$argll	;# perform the direct call
    }

    self method new {args} {
	Debug.direct {Direct new $name $args}
	set body [lindex $args end]
	set args [lrange $args 0 end-1]
	set obj [next {*}$args]

	oo::objdefine $obj $body
	$obj mkMethods

	return $obj
    }

    self method create {name args} {
	Debug.direct {Direct create $name $args}
	set body [lindex $args end]
	set args [lrange $args 0 end-1]
	set obj [next $name {*}$args]

	oo::objdefine $obj $body
	$obj mkMethods

	return $obj
    }

    constructor {args} {
	Debug.direct {Constructing [self]}
	variable wildcard /
	variable mount /
	variable complain 0	;# complain if a named parameter doesn't exist?
	variable parameters 1	;# bother unpacking parameters?
	variable {*}$args
	variable passthru {}	;# experimental - list of passthru URLs

	[self] mkMethods
    }
}

# DirectSimple - Direct domain with simple value return
oo::class create DirectSimple {
    # Define - define a command for a simple Direct domain (no R dict)
    method Define {m args} {
	return [next $m . {*}$args]
    }

    # Call - invoke command sans R - expect HTML
    # command may still access R using uplevel
    method Call {R cmd args} {
	dict set R -reply -content [my $cmd {*}$args]
	return $R
    }

    superclass Direct
    constructor {args} {
	next {*}$args
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    puts stderr "[package present Direct] Unit Tests"

    Debug on error
    Debug on direct
    Debug on query

    package require tcltest
    namespace import ::tcltest::*

    variable {*}$argv

    Direct create dtest var 1 {
	method / {r} {
	    return 1
	}
	method /call {r} {
	    return [dict merge $r {-result 1}]
	}
	method /arg {r a} {
	    return [dict merge $r [list -result $a]]
	}
	method /arg2 {r a b} {
	    return [dict merge $r [list -result "$a $b"]]
	}
	method /args {r a b args} {
	    return [dict merge $r [list -result "$a $b $args"]]
	}
    }

    test call {simple object call} {
	dict get [dtest do {-Url {path /call}}] -result
    } 1

    test prefix {simple object prefix call} {
	dict get [dtest do {-Url {path /call/extra}}] -result
    } 1

    test arg1 {object call with arg} {
	dict get [dtest do {-Url {path /arg query a=1}}] -result
    } 1

    test arg2 {object call with extra arg} {
	dict get [dtest do {-Url {path /arg query b=2&a=1}}] -result
    } 1

    test arg3 {object call with extra arg} {
	dict get [dtest do {-Url {path /arg2 query b=2&a=1}}] -result
    } "1 2"

    test arg4 {object call with args} {
	dict get [dtest do {-Url {path /args query b=2&a=1&c=3&d=4}}] -result
    } "1 2 c 3 d 4"
}
