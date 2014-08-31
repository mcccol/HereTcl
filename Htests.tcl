lappend ::auto_path [pwd]
package require H

# Unit tests

puts stderr "[package present H] Unit Tests"
Debug on error
Debug off listener
Debug off httpd
Debug off httpdlow
Debug off httpdtxlow
Debug off entity
Debug define cache
Debug define cookies

interp bgerror {} BGERROR
proc BGERROR {args} {
    puts stderr "*******BGERROR: $args"
}

#interp bgerror {} [list ::apply {{args} {
#    puts stderr "*******BGERROR: $args"
#    incr ::forever
#}}]

package require tcltest
namespace import ::tcltest::*
package require http
set port 8080
set maxports 500
set verbose 0
variable {*}$argv
set phase 0

set defaults [H defaults {}]

variable SETUP {set listener [H listen process [list ::apply {{r} {H Ok $r content-type text/html <p>Moop</p>}}] {*}$defaults $::port]}
variable CLEANUP {chan close $listener}

configure -verbose ep

skip unsupported-*
#skip simple-*

proc get_test {token} {
    set d {}
    catch {set d [::http::meta $token]}
    catch {dict set d -code [::http::ncode $token]}
    catch {dict set d -status [::http::status $token]}
    catch {dict set d -content [::http::data $token]}
    catch {dict set d -error [::http::error $token]}
    catch {dict set d -http [::http::code $token]}
    return $d
}

proc test_dict {token args} {
    if {[llength $args] == 1} {
	set args [lindex $args 0]
    }

    set r {}
    foreach {n v} [get_test $token] {
	set n [string tolower $n]
	if {[dict exists $args $n]} {
	    if {![string match [dict get $args $n] $v]} {
		dict set r $n $v
	    }
	    dict unset args $n
	}
    }
    ::http::cleanup $token
    return $r
}

set phase 0

# perform tests in event space
after 0 {::apply {{} {
    puts stderr "Phase:$::phase Simple Tests"
    
    test simple-GET {perform a simple GET which returns some html} -setup {
	set ::listener [H listen process [list ::apply {{r} {
	    H Ok $r content-type text/html <p>Moop</p>
	}}] {*}$::defaults $::port]
    } -body {
	set token [::http::geturl http://localhost:$::port/ -timeout 100]
	
	::http::wait $token
	
	test_dict $token {
	    -code 200
	    connection close
	    content-type {text/html; charset=utf-8}
	    server {H *}
	    vary accept-encoding
	    content-length 11
	} 
    } -cleanup {
	chan close $::listener
    }
    
    test simple-BINARY {send 1k of random bytes to the server, which echoes it back unchanged, compare received with sent data} -setup {
	set ::listener [H listen process [list ::apply {{r} {
	    set entity [dict get $r -entity]; dict unset r -entity
	    #puts Rx:[binary encode hex $entity]
	    dict set r -content $entity
	    dict set r content-length [string length $entity]
	    return $r	;# this should echo the body
	}}] {*}$::defaults $::port]
    } -body {
	# simple-BINARY just 
	set ::body ""
	for {set i 0} {$i < 1024} {incr i} {
	    append ::body [binary format c [expr {int(rand() * 256)}]]
	}
	#puts RQ:[binary encode hex $::body]
	set token [::http::geturl http://localhost:$::port/ -type application/octet-stream -timeout 100 -query $::body]
	::http::wait $token
	#puts stderr "META:[::http::meta $token]"
	if {[::http::data $token] eq $::body} {
	    return ""
	} else {
	    return "'[binary encode hex $::body]' != '[binary encode hex [::http::data $token]]'"
	}
    } -cleanup {
	chan close $::listener
    }

    test simple-ERROR {generate an error, make sure it's received} -setup {
	set ::listener [H listen error {::apply {{r args} {dict set r -code 1; return $r}}} process [list ::apply {{r} {
	    expr 1/0	;# provoke an error
	}}] {*}$::defaults $::port]
    } -body {
	set token [::http::geturl http://localhost:$::port/ -timeout 100]
	
	::http::wait $token
	
	test_dict $token {
	    -http {HTTP/1.1 500 Internal Server Error}
	    content-type {text/html; charset=utf-8}
	}
    } -cleanup { 
	chan close $::listener
    }

    test simple-ABORT {generate a protocol-level, make sure it's received} -setup {
	set ::om $H::methods
	set H::methods {}	;# no matter what we send it will abort
	set ::listener [H listen process [list ::apply {{r} {}}] $::port]
    } -body {
	set token [::http::geturl http://localhost:$::port/ -timeout 100]
	::http::wait $token
	
	test_dict $token {
	    -http {HTTP/1.1 405 Method Not Allowed}
	}
    } -cleanup {
	set H::methods $::om
	chan close $::listener
    }

    incr ::phase	;# these tests are complete
}}}

vwait phase	;# wait for this testing phase to finish

# the tests need to be in event space
after 0 {::apply {{} {
    puts stderr "Phase:$::phase Multiple Asynch Request"

    set ::listener [H listen process [list ::apply {{r} {
	#set delay [expr {int(10000 * rand())}]
	#after $delay
	set val [string trim [dict get $r -Header uri] /]
	#puts stderr "DO: $val"
	H Ok $r content-type text/html $val
    }}] {*}$::defaults $::port]

    set cmd [list ::apply {{count token} {
	if {[catch {
	    upvar #0 $token r
	    if {$r(status) ne "ok"} {
		puts stderr "FAIL-$r(status) $count/[llength [chan names]]"
	    } else {
		#puts stderr "GOT:$r(body)-[array size ::result]-[llength [chan names]]"
		if {$r(body) != $count} {
		    puts stderr "Body '$r(body)' and Count '$count' out of sync"
		} else {
		    unset ::result($r(body))
		    if {[array size ::result] == 0} {
			incr ::phase	;# completed phase
			puts stderr "Multi-GET complete"
			chan close $::listener
		    }
		}
	    }
	} e eo]} {
	    puts stderr "ERR:'$e'($eo)/([array get r])"
	}
    }}]
    
    for {set i 0} {$i < $::maxports} {incr i} {
	set ::result($i) [::http::geturl http://localhost:$::port/$i -timeout 0 -command [list {*}$cmd $i]]
    }
}}}

vwait phase	;# wait for this testing phase to finish

# the tests need to be in event space
after 0 {::apply {{} {
    puts stderr Phase:$::phase
    
    set ::listener [H listen timeout 0 process [list ::apply {{r} {
	#puts stderr "SETTING[info coroutine]/[llength [chan names]] ($r)"
	after [expr {int(10000 * rand())}] [info coroutine]
	::yieldto return -code break	;# this bit of magic causes H to leave this processing coro alone
	
	set socket [dict get $r -socket]
	set val [string trim [dict get $r -Header uri] /]
	set result [H Ok $r content-type text/html $val]
	#puts stderr "TRIGGER[info coroutine]: $val - $socket -> $result"
	return $result
    }}] $::port]

    variable port
    for {set i 0} {$i < $::maxports} {incr i} {
	set cmd [list ::apply {{count token} {
	    if {[catch {
		upvar #0 $token r
		if {$r(status) ne "ok"} {
		    puts stderr "FAIL-$r(status) $count/[llength [chan names]]"
		} else {
		    #puts stderr "GOT:$r(body)/[array size ::result]/[llength [chan names]]"
		    if {$r(body) != $count} {
			puts stderr "Body '$r(body)' and Count '$count' out of sync"
		    } else {
			unset ::result($r(body))
			if {[array size ::result] == 0} {
			    incr ::phase
			    chan close $::listener
			    puts stderr "Vari-GET complete"
			}
		    }
		}
	    } e eo]} {
		puts stderr ERR:'$e'($eo)/([array get r])
	    }
	}} $i]
	set ::result($i) [::http::geturl http://localhost:$::port/$i -timeout 0 -command $cmd]
    }
}}}

vwait phase

puts stderr Phase:$::phase
puts "Open Chans: [chan names]"
