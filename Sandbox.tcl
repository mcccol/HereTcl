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
package require HWebSocket

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
    <h1>H Sandbox - play around</h1> by Colin McCormack mcccol@gmail.com
    <p>Here is the complete <a href='/home'>file system</a> in which this instance of H is running, right now [clock format [clock seconds]]<p>
    <p><a href='/home/Sandbox.tcl'>This file</a> is the code running this program, right now.  It has some necessary complexities.<p>
    <p><a href='/home/Minimal.tcl'>This file</a> is a much more minimal example of an H-served site.</p>

    <p><a href='h/moop'>This link</a> shows you the default error you get if there's something wrong.</p>
    <p><a href='/home/moop'>This link</a> shows you the default error you get if there's no matching file in a file domain.</p>
    <p><a href='echo'>This link</a> is a simple test of WebSocket support - it doesn't do much yet.</p>
    <p><a href='redir'>This link</a> is a redirection test.</p>

    <p>Here are the fossil repositories containing this instance.</p>
    <ul>$fossil</ul>

    <h2>Get it, mod it, play around some more.</h2>
    <ol>
    <li>Make a nice clean directory</li>
    <li>In that directory, execute: fossil clone http://[info hostname]:$::port/h/H/ H.fossil
    <li>Execute: fossil open H.fossil
    <li>Then execute: tclsh8.6 Sandbox.tcl
    </ol>
    </body>
    </html>
}

# websocket test from https://www.websocket.org/echo.html
variable echojs {
    var wsUri = "ws://localhost:8080/echo";
    var output;

    function init() {
	output = document.getElementById("output"); testWebSocket(); 
    }

    function writeToScreen(message) {
	var pre = document.createElement("p");
	pre.style.wordWrap = "break-word";
	pre.innerHTML = message; output.appendChild(pre);
    }

    function testWebSocket() {
	websocket = new WebSocket(wsUri);
	websocket.onopen = function(evt) { onOpen(evt) };
	websocket.onclose = function(evt) { onClose(evt) };
	websocket.onmessage = function(evt) { onMessage(evt) };
	websocket.onerror = function(evt) { onError(evt) };
    }

    function onOpen(evt) {
	writeToScreen("CONNECTED");
	doSend("WebSocket test\n");
    }

    function onClose(evt) {
	writeToScreen("DISCONNECTED");
    }

    function onMessage(evt) {
	writeToScreen('<span style="color: blue;">RESPONSE: ' + evt.data+'</span>'); websocket.close();
    }

    function onError(evt) {
	writeToScreen('<span style="color: red;">ERROR:</span> ' + evt.data);
    }

    function doSend(message) {
	writeToScreen("SENT: " + message);  websocket.send(message);
    }

    window.addEventListener("load", init, false);
}

variable echo [subst {
    <!DOCTYPE html>
    <meta charset="utf-8" />
    <title>WebSocket Test</title>
    <script language="javascript" type="text/javascript">
    $echojs
    </script>
    <h2>WebSocket Test</h2>  <div id="output"></div> 
}]


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

    # echo - use websockets to echo stuff back and forth between client and server
    method /echo {r} {
	return [H Ok $r content-type text/html $::echo]
    }

    # redirection test
    method /redir {r} {
	return [H Redirect $r http://localhost:8080/redirected]
    }
    method /redirected {r} {
	return [H Ok $r content-type text/html {<html>
	    <body>
	    <p>Redirected!</p>
	    </body>
	    </html>
	}]
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
Debug on websocket
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
#lappend largs tls [list -require 0 -certfile server.crt -keyfile server.key -cadir $home]	;# for TLS - certs in $home
set ::listener [H listen {*}$largs $::port]
puts stderr "H listening on port $::port"

::vwait forever
