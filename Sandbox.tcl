# sandbox.tcl - example server providing a sandbox for the H-curious

if {[info exists argv0] && ($argv0 eq [info script])} {
    # try to load the rest of H, if this is running as part of the ensemble of modules
    set ::home [file dirname [file normalize [info script]]]

    if {[file exists [file join [pwd] H]]} {
	# find a directory called H relative to the user's working directory
	# and use it for the H server sources
	set pwd [file normalize [pwd]]
    } else {
	# find a directory called H, relative to this script and use it for the H server sources
	for {set pwd $::home} {$pwd ne "/" && ![file exists [file join $pwd H]]} {set pwd [file dirname $pwd]} {}
    }
    lappend ::auto_path $::home [file join $pwd H]
}

package require Debug	;# provides debugging narrative
package require Direct	;# provides Direct domain - namespace/TclOO command access via URL
package require File	;# provides File domain - deliver content of file system via URL
package require H	;# provides the H HTTP server itself

Debug on error		;# we always want to see errors

# Create some File domains - these are invoked by the dispatcher below
# Each of these is an object which interprets requests over the file-system
File create home root $home	;# this is the directory Sandbox is running in
File create user root [pwd]	;# this is the directory the user ran Sandbox from
File create css root [file join $home css]

# toplevel is a template generating the html for the top level page
variable toplevel {<html>
    <head>
    </head>
    <body>
    <h1>H Sandbox - play around</h1>
    <p>Here is the complete <a href='/home'>file system</a> in which this instance of H is running, right now [clock format [clock seconds]]<p>
    <p>Here are the fossil repositories containing this instance.</p>
    <ul>$fossil</ul>
    </body>
    </html>
}

# dispatcher - this is the thing which determines where and how requests are processed
# dispatcher is an instance of the Direct object, it interprets HTTP requests as
# Tcl command invocations (with arguments.)
Direct create dispatcher {
    method /home {r} {
	return [home do $r]
    }
    method /css {r} {
	return [css do $r]
    }
    method /user {r} {
	return [user do $r]
    }

    method / {r} {
	set fossil ""
	foreach f [lsort [array names ::fossil_server]] {
	    append fossil <li> "<a href='/h/$f'>$f</a>" </li> \n
	}

	return [H Ok $r content-type text/html [subst $::toplevel]]
    }

    method /favicon {r} {
	return [H NotFound $r]	;# we don't have an icon
    }

    # |h - this is a 'direct passthru' ... sorry about the jargon.
    # it arranges requests, relatively unparsed, to be passed directly
    # to the fossil instances running concurrently with this server.
    method |h {r} {
	lassign [split [dict get $r -Full]] method uri version
	set fn [lindex [split $uri /] 2]
	set uri [join [lrange [split $uri /] 3 end] /]
	return [list $r [socket localhost {*}$::fossil_port($fn)] /$uri]
    }
}

# process command line arguments when this script is called from the command line
if {[info exists argv0] && ($argv0 eq [info script])} {
    # process command line args
    set port 8080	;# this is the port on which the server will listen
    set root $home	;# by default this is where the server will look for files
    set fossil {}	;# default fossil commands
    variable {*}$argv

    # these are fossil-args - handled by the fossil passthru guff
    set fossil [dict merge {glob *.fossil dirs {. ..} port 8090} $fossil]
}

# locate fossil repos in the given fossil dirs and create a server per repo
if {[info exists argv0] && ($argv0 eq [info script])} {
    # open_fossil - this thing just starts up concurrent fossil servers
    # for each fossil repo found in the fossil dirs given above
    apply {{args} {
	dict with args {}
	# find a bunch of fossils
	
	foreach fd $dirs {
	    if {[file pathtype $fd] eq "relative"} {
		set fd [file normalize [file join $::home $fd]]
	    } else {
		set fd [file normalize $fd]
	    }

	    foreach f [glob -directory $fd -nocomplain -types f -- $glob] {
		set fn [file rootname [file tail $f]]
		if {[info exists ::fossil_repo($fn)]} continue
		
		try {
		    set opener "|fossil server --port $port --localhost --baseurl http://[info hostname]:$::port/h/$fn $f"
		    set ::fossil_server($fn) [open $opener w] 
		    set ::fossil_port($fn) $port

		    incr port
		} on error {e eo} {
		    puts stderr "FOSSIL OPEN ERR: $e ($eo)"
		    exit
		} on ok {} {
		    #puts stderr "OPENED FOSSIL $fn: $::fossil_server($fn) pid:[pid $::fossil_server($fn)] on port $::fossil_port($fn)"
		}
	    }
	}
    }} {*}$fossil
}

# we don't so much care about these other debug narratives
Debug define query
Debug off file
Debug off direct
Debug off process

Debug off httpd
Debug off listener
Debug off httpdlow
Debug off httpdtx
Debug off httpdtxlow
Debug off entity
Debug off cache
Debug off cookies

# start the H server's listener
set largs {}
lappend largs rxprocess ::Identity	;# H will not pre-process requests (permits passthru)
lappend largs dispatch {dispatcher do}	;# H will dispatch requests to $dispatch for processing
lappend largs tls {} 
set ::listener [H listen {*}$largs $::port]
puts stderr "H listening on port $::port"

::vwait forever
