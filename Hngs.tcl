# example.tcl - example server
if {[info exists argv0] && ($argv0 eq [info script])} {
    # try to load the rest of H, if this is running as part of the ensemble of modules
    ::apply {{} {
	set home [file dirname [file normalize [info script]]]
	lappend ::auto_path $home
    }}
}

package require Debug
package require Direct
package require File
package require H

Debug on error

set port 8080
variable {*}$argv

File create files root [file join [file dirname [file normalize [info script]]] files]

Direct create dispatcher {
    method /moop {r} {
	return [H Ok $r content-type text/html {
	    <h1>example</h1>
	    <p>This content is being served from a command /moop</p>
	    <ul>
	    <li><a href='moop'>This</a></li>
	    <li><a href='ls'>stream via chunk</a></li>
	    <li><a href='files/test.html'>file content</a></li>
	    </ul>
	}]
    }

    method /ls {r} {
	dict set r -process [list H TxFile [open |/bin/ls]]
	dict set r -code 200
	dict set r content-type "text/plain;charset=utf-8"
	return $r
    }

    method /find {r} {
	dict set r -process [list H TxFile [open "|/usr/bin/du .." ]]
	dict set r -code 200
	dict set r content-type "text/plain;charset=utf-8"
	return $r
    }

    method /files {r} {
	return [files do $r]
    }
}

Debug on error

Debug define query
Debug on file
Debug on direct

Debug off httpd
Debug off listener
Debug off httpdlow
Debug off httpdtxlow
Debug off entity
Debug off cache
Debug off cookies

proc Hngs {args} {
}

set ::listener [H listen process Hngs $::port]

::vwait forever
