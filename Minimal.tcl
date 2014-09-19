# Minimal.tcl - minimal server
lappend ::auto_path [pwd] [file dirname [info script]]
package require H
package require File
package require Direct

set port 8080
variable {*}$argv

File create root root [file dirname [file normalize [info script]]]

Direct create dispatcher {
    method /root {r} {
	return [root do $r]
    }

    method / {r} {
	return [H Ok $r content-type text/html {<html>
	    <body>
	    <p>This is a <a href='root'>website</a></p>
	    </body>
	    </html>
	}]
    }

    method /favicon.ico {r} {
	return [H NotFound $r]
    }
}

set ::listener [H listen dispatch {dispatcher do} $::port]

::vwait forever
