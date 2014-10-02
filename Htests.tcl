lappend ::auto_path [pwd]
package require H
H::load Hproc.tcl

# Unit tests

puts stderr "[package present H] Unit Tests"
Debug on error
Debug off listener
Debug off httpd
Debug off httpdlow
Debug off httpdtx
Debug off httpdtxlow
Debug off process
Debug off entity
Debug define cache
Debug define cookies
Debug define httpdchan

if {0} {
    interp bgerror {} BGERROR
    proc BGERROR {args} {
	puts stderr "*******BGERROR: ($args)"
    }
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
    set d [get_test $token]
    #puts stderr META:$d
    foreach {n v} $d {
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
    
    if {1} {
	test simple-GET {perform a simple GET which returns some html} -setup {
	    set ::listener [H listen process [list ::apply {{r} {
		H Ok $r content-type text/html <p>Moop</p>
	    }}] {*}$::defaults $::port]
	} -body {
	    set token [::http::geturl http://localhost:$::port/ -timeout 100]
	    
	    ::http::wait $token
	    
	    test_dict $token {
		-code 200
		content-type text/html
		server {H *}
		vary accept-encoding
		content-length 11
	    } 
	} -cleanup {
	    chan close $::listener
	}
    }

    if {1} {
	test simple-No-Host {test no-host 400 response} -setup {
	    set ::listener [H listen rx {timeout {"" 1} ondisconnect {::apply {{coro vars} {
		upvar #1 eo eo
		set ::rxtimeout [dict get $eo -errorcode]
	    }}}} process [list ::apply {{r} {
		H Ok $r content-type text/html <p>Moop</p>
	    }}] {*}$::defaults $::port]
	} -body {
	    try {
		set waiter [::socket localhost $::port]	;# now do nothing
		chan configure $waiter -translation crlf
		puts $waiter "GET / HTTP/1.1"
		puts $waiter "content-length: 100"
		puts $waiter ""
		flush $waiter

		# now do nothing
		vwait ::rxtimeout
		close $waiter
		set ::rxtimeout
	    } on error {e eo} {
		puts stderr "ERR: $e ($eo)"
	    }
	} -cleanup {
	    chan close $::listener
	} -result {HTTP 400}
    }

    if {1} {
	test simple-phoney-HTTP {test garbage connection} -setup {
	    set ::listener [H listen rx {timeout {"" 1} ondisconnect {::apply {{coro vars} {
		upvar #1 eo eo
		set ::rxtimeout [dict get $eo -errorcode]
	    }}}} process [list ::apply {{r} {
		H Ok $r content-type text/html <p>Moop</p>
	    }}] {*}$::defaults $::port]
	} -body {
	    try {
		set waiter [::socket localhost $::port]	;# now do nothing
		chan configure $waiter -translation crlf
		puts $waiter "WE now send some crap which has nothing to do with HTTP"
		flush $waiter

		vwait ::rxtimeout
		close $waiter
		set ::rxtimeout
	    } on error {e eo} {
		puts stderr "ERR: $e ($eo)"
	    }
	} -cleanup {
	    chan close $::listener
	} -result {HTTP 400}
    }

    test simple-ERROR {generate an error, make sure it's received} -setup {
	set ::listener [H listen error {::apply {{r args} {dict set r -code 1; return $r}}} process [list ::apply {{r} {
	    Debug off error
	    expr 1/0	;# provoke an error
	    Debug on error
	}}] {*}$::defaults $::port]
    } -body {
	set token [::http::geturl http://localhost:$::port/ -timeout 100]
	
	::http::wait $token
	
	test_dict $token {
	    -http {HTTP/1.1 500 Internal Server Error}
	    content-type text/html
	}
    } -cleanup { 
	chan close $::listener
    }

    test simple-ABORT {generate a protocol-level abort, make sure it's received} -setup {
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

    test simple-BINARY {send 1k of random bytes to the server, which echoes it back unchanged, compare received with sent data} -setup {
	set ::listener [H listen process [list ::apply {{r} {
	    set entity [dict get? $r -entity]; dict unset r -entity
	    #puts Rx:[binary encode hex $entity]
	    dict set r -reply -content $entity
	    dict set r -reply content-length [string length $entity]
	    dict set r -reply content-type application/octet-stream
	    return $r	;# this should echo the body
	}}] {*}$::defaults $::port]
    } -body {
	# simple-BINARY just 
	set ::body ""
	for {set i 0} {$i < 1024} {incr i} {
	    append ::body [binary format c [expr {int(rand() * 256)}]]
	}
	#puts stderr RQ:[binary encode hex $::body]
	set token [::http::geturl http://localhost:$::port/ -type application/octet-stream -timeout 100 -query $::body]
	::http::wait $token
	#puts stderr "META:[::http::meta $token]"
	if {[::http::data $token] eq $::body} {
	    set result ""
	} else {
	    set result "'[binary encode hex $::body]' != '[binary encode hex [::http::data $token]]'"
	}
	::http::cleanup $token
	return $result
    } -cleanup {
	chan close $::listener
    }

    if {1} {
	test simple-GET-with-timeout {test 1 second timeout} -setup {
	    set ::listener [H listen rx {timeout {"" 1} ondisconnect {::apply {{coro vars} {
		upvar #1 eo eo
		set ::rxtimeout [dict get $eo -errorcode]
	    }}}} process [list ::apply {{r} {
		H Ok $r content-type text/html <p>Moop</p>
	    }}] {*}$::defaults $::port]
	} -body {
	    try {
		set waiter [::socket localhost $::port]	;# now do nothing
		vwait ::rxtimeout
		close $waiter
		set ::rxtimeout
	    } on error {e eo} {
		puts stderr "ERR: $e ($eo)"
	    }
	} -cleanup {
	    chan close $::listener
	} -result {TIMEOUT Request}
    }

    if {1} {
	test simple-GET-with-timeout2 {test 1 second entity timeout} -setup {
	    set ::listener [H listen rx {timeout {"" 1} ondisconnect {::apply {{coro vars} {
		upvar #1 eo eo
		set ::rxtimeout [dict get $eo -errorcode]
	    }}}} process [list ::apply {{r} {
		H Ok $r content-type text/html <p>Moop</p>
	    }}] {*}$::defaults $::port]
	} -body {
	    try {
		set waiter [::socket localhost $::port]	;# now do nothing
		chan configure $waiter -translation crlf
		puts $waiter "GET / HTTP/1.1"
		puts $waiter "content-length: 100"
		flush $waiter

		# now do nothing
		vwait ::rxtimeout
		close $waiter
		set ::rxtimeout
	    } on error {e eo} {
		puts stderr "ERR: $e ($eo)"
	    }
	} -cleanup {
	    chan close $::listener
	} -result {TIMEOUT Headers}
    }

    incr ::phase	;# these tests are complete
}}}

vwait phase	;# wait for this testing phase to finish
puts "Phase $phase Open Chans [llength [chan names]]: [chan names]"

# the tests need to be in event space
after 0 {::apply {{} {
    puts stderr "Phase:$::phase Multiple Asynch Request"

    set ::listener [H listen process [list ::apply {{r} {
	# this server snippet returns the trailing uri element as a text/html val
	set val [string trim [dict get $r -Header uri] /]
	#puts stderr "DO: $val"
	H Ok $r content-type text/plain $val
    }}] {*}$::defaults $::port]

    set cmd [list ::apply {{count token} {
	if {[catch {
	    upvar #0 $token r
	    if {$r(status) ne "ok"} {
		puts stderr "FAIL-$r(status)/[::http::error $token] $count [llength [chan names]]"
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
	    ::http::cleanup $token
	} e eo]} {
	    puts stderr "ERR:'$e'($eo)/([array get r])"
	}
    }}]
    
    set ::start_time [clock microseconds]
    for {set i 0} {$i < $::maxports} {incr i} {
	set ::result($i) [::http::geturl http://localhost:$::port/$i -timeout 0 -command [list {*}$cmd $i]]
    }
}}}

vwait phase	;# wait for this testing phase to finish
puts stderr "Elapsed time in [expr {$phase - 1}]: [expr {[clock microseconds] - $::start_time}]uS"
puts "Phase $phase Open Chans [llength [chan names]]: [chan names]"

# the tests need to be in event space
after 0 {::apply {{} {
    puts stderr Phase:$::phase
    #Debug on httpd
    #Debug on httpdtx

    set ::listener [H listen timeout {} process [list ::apply {{r} {
	# this server snippet pauses randomly, then returns the uri trailing element as an entity
	#puts stderr "SETTING[info coroutine]/[llength [chan names]] ($r)"
	after [expr {int(10000 * rand())}] [info coroutine]
	::yieldto return -code error -errorcode SUSPEND	;# this bit of magic causes Hproc to leave this processing coro alone
	
	set socket [dict get $r -socket]
	set val [string trim [dict get $r -Header uri] /]
	set result [H Ok $r content-type text/plain $val]
	#puts stderr "TRIGGER[info coroutine]: $val - $socket -> $result"
	[dict get $r -tx] reply $result		;# transmit the error
    }}] $::port]

    variable port
    for {set i 0} {$i < $::maxports} {incr i} {
	set cmd [list ::apply {{count token} {
	    if {[catch {
		upvar #0 $token r
		if {$r(status) ne "ok"} {
		    puts stderr "FAIL-$r(status)/[::http::ncode $token]/[::http::error $token]: $count [llength [chan names]]"
		} else {
		    #puts stderr "GOT-$r(status)/[::http::ncode $token]:'$r(body)'-$count [array size ::result]/[llength [chan names]]"
		    if {$r(body) != $count} {
			puts stderr "Body '$r(body)' and Count '$count' out of sync ([::http::meta $token])"
		    }

		    unset ::result($count)
		    if {[array size ::result] == 0} {
			incr ::phase
			chan close $::listener
			puts stderr "Vari-GET complete"
		    }
		}
	    } e eo]} {
		puts stderr ERR:'$e'($eo)/([array get r])
	    }
	    ::http::cleanup $token
	}} $i]
	set ::result($i) [::http::geturl http://localhost:$::port/$i -timeout 0 -command $cmd]
    }
}}}

vwait phase

puts stderr Phase:$::phase
puts "Open Chans [llength [chan names]]: [chan names]"
