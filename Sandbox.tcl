# sandbox.tcl - example server providing a sandbox for the H-curious

if {[info exists argv0] && ($argv0 eq [info script])} {
    # try to load the rest of H, if this is running as part of the ensemble of modules
    set ::home [file dirname [file normalize [info script]]]

    if {[file exists [file join [pwd] H.tcl]]} {
	# find a directory called H relative to the user's working directory
	# and use it for the H server sources
	set pwd [file normalize [pwd]]
    } else {
	# find a directory called H, relative to this script and use it for the H server sources
	for {set pwd $::home} {$pwd ne "/" && ![file exists [file join $pwd H H.tcl]]} {set pwd [file dirname $pwd]} {}
    }
    lappend ::auto_path $::home [file join $pwd H]
}

package require Debug	;# provides debugging narrative

# source .hrc for local customisations
if {[file exists [file join $::home .hrc]]} {
    source [file join $::home .hrc]
}

package require Direct	;# provides Direct domain - namespace/TclOO command access via URL
package require File	;# provides File domain - deliver content of file system via URL
package require H	;# provides the H HTTP server itself

Debug on error		;# we always want to see errors
#Debug on httpdbad

# Create some File domains - these are invoked by the dispatcher below
# Each of these is an object which interprets requests over the file-system
File create home root $home	;# this is the directory Sandbox is running in
File create home_fcopy root $home fcopy 1	;# this is the directory Sandbox is running in
File create user root [pwd]	;# this is the directory the user ran Sandbox from
File create css root [file join $home css]

# toplevel is a template generating the html for the top level page
variable toplevel {<html>
    <head>
    </head>
    <body>
    <h1>HereTcl Sandbox - play around</h1> by Colin McCormack mcccol@gmail.com
    <p><a href="http://puretcl.com:8080">HereTcl</a> is a modern HTTP1.1 web server which is completely scripted and scriptable in Tcl.</p>
    <p>HereTcl architecture is clean and elegant, and lends itself to support of Web Applications.</p>
    <p>Here is the complete <a href='/home'>file system</a> in which this instance of HereTcl is running, right now [clock format [clock seconds]]<p>
    <p>Here is the same <a href='/home_fcopy'>file system</a> with asynchronous file copy - I don't think the difference is hugely significant.<p>
    <p><a href='/home/Sandbox.tcl'>This file</a> is the code running this program, right now.  It has some necessary complexities.<p>
    <p><a href='/home/Minimal.tcl'>This file</a> is a much more minimal example of an HereTcl-served site.</p>

    <p><a href='h/moop'>This link</a> shows you the default error you get if there's something wrong.</p>
    <p><a href='/home/moop'>This link</a> shows you the default error you get if there's no matching file in a file domain.</p>
    <p><a href='echo'>This link</a> is a simple test of WebSocket support.</p>
    <p><a href='redir'>This link</a> is a redirection test.</p>
    <p><a href='coro/'>Here's some internal debugging</a></p>
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
    var wsUri = "ws://$host:$port/$url";
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
	writeToScreen("CONNECTED");
	doSend("WebSocket test\n");
	websocket.close();
    }

    function onClose(evt) {
	writeToScreen("DISCONNECTED");
    }

    function onMessage(evt) {
	console.log("ws response: %o", evt.data);
	writeToScreen('<span style="color: blue;">RESPONSE: ' + evt.data+'</span>');
	//websocket.close();
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

# Dispatcher - this is the thing which determines where and how requests are processed
# Dispatcher is an instance of the Direct object, it interprets HTTP requests as
# Tcl command invocations (with arguments.)  Its implementation is in D/Direct.tcl
#
# You don't have to use Direct to dispatch on URLs, you can use any command prefix by
# specifying the command prefix to [H listener] as the 'dispatch' argument.
# HereTcl invokes {*}$dispatch $R, where $R is the (unprocessed) request dict.
# The dispatch command's result is (by default) sent back to the client as a reply.
Direct create Dispatcher rootname 0 {
    method /home {r} {
	return [home do $r]
    }
    method /home_fcopy {r} {
	return [home_fcopy do $r]
    }
    method /css {r} {
	return [css do $r]
    }
    method /user {r} {
	return [user do $r]
    }

    method /echows {opcode args} {
	puts stderr "/echows $opcode ($args)"
	switch -- $opcode {
	    connect {
		# we have received an Upgrade Websocket attempt.
		# Details are in $r, which is a normal HTTP request dict.
		# We have two choices - either error, in which case the connection fails
		# or we just return $r to establish the connection and activate the websocket processing
		# it is open to us to modify $r, which can modify the websocket handshake.
		# 
		# By default, subsequent WS events will come back to this command,
		# to change the ws process command: [dict set r -ws $cmdprefix]
		lassign $args r
		return $r
	    }

	    text {
		set rest [lassign $args message]
		::H::ws Send [dict get $message payload]
		::H::ws Ping
	    }

	    binary {
		set rest [lassign $args message]
		::H::ws Send [dict get $message payload] 1
	    }

	    pong {}

	    closed -
	    default {
	    }
	}
    }

    # echo - use websockets to echo stuff back and forth between client and server
    method /echo {r} {
	set host [dict get $r -Url host]
	set port [dict get $r -Url port]
	set url echows
	return [H Ok $r content-type text/html [subst -nobackslashes $::echo]]
    }

    method /echoc {opcode args} {
	puts stderr "/echoc $opcode ($args)"
	switch -- $opcode {
	    connect {
		lassign $args r
		set r [::H::ws Chan $r]
		return $r
	    }

	    default {
		error "/echoc got a message $opcode ($args)"
	    }
	}
    }

    # echochan - use websockets to echo stuff back and forth between client and server
    method /echochan {r} {
	set host [dict get $r -Url host]
	set port [dict get $r -Url port]
	set url echoc
	return [H Ok $r content-type text/html [subst -nobackslashes $::echo]]
    }

    # redirection test
    method /redir {r} {
	return [H Redirect $r http://[dict get $r -Url host]:[dict get $r -Url port]/redirected]
    }

    method /redirected {r} {
	return [H Ok $r content-type text/html {<html>
	    <body>
	    <p>Redirected!</p>
	    </body>
	    </html>
	}]
    }

    method /coro {r} {
	set extra [dict get? $r -Url extra]
	if {$extra eq ""} {
	    set table [H socketDump]
	    return [H NoCache [H Ok $r content-type text/html <html><body>$table</body></html>]]
	} elseif {[llength [info commands $extra]]} {
	    set table [H coroDump [dict get? $r -Url extra]]
	    return [H NoCache [H Ok $r content-type text/html <html><body>$table</body></html>]]
	} else {
	    return [H NotFound $r]
	}
    }

    method /*.ico {r args} {
	puts stderr "/*.ico - ($r)"
	return [H Ok $r content-type image/png ""]
    }

    method / {r args} {
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
    set tls {}		;# no tls by default

    variable {*}$argv

    # these are fossil-args - handled by the fossil passthru guff
    set fossil [dict merge {glob *.fossil dirs {. ..} port 8090} $fossil]
}

# locate fossil repos in the given fossil dirs and create a server per repo
if {[info exists argv0] && ($argv0 eq [info script])} {
    set ::env(FOSSIL_HOME) [file join $::home .fossil]
    # open_fossil - this thing just starts up concurrent fossil servers
    # for each fossil repo found in the fossil dirs given above
    apply {{args} {
	#puts stderr "FOSSIL ARGS: $args"
	set fargs ""
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
		    set opener "|fossil server --port $port --localhost --baseurl http://[info hostname]:$::port/h/$fn $fargs $f 2>/tmp/$fn.err >/tmp/$fn.log"
		    #puts stderr "FOSSIL: $opener"
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

# start the H server's listener
set largs {}				;# accumulate some arguments to the [H listen] command
lappend largs home $home
lappend largs dispatch {Dispatcher do}	;# H will dispatch requests to dispatch for processing
lappend largs wsprocess {Dispatcher ws}	;# H will dispatch websocket upgrades to wsprocess for processing
lappend largs tls $tls	;# for TLS - certs in $home

#lappend largs access_log_fd stdout
package require Logchan
lappend largs access_log_fd [Logchan open [pwd]/access.log daily 1]

puts stderr "H listening $largs"

set ::listener [H listen {*}$largs no_legacy false $::port]	;# create a listener on $::port
puts stderr "H listening on port $::port"

::vwait forever
