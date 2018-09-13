# conversions - Code to implement some standard mime conversions

package require struct::list
package require Html

Debug define jsloader 10
Debug define cssloader 10

package provide conversions 1.0

namespace eval ::conversions {
    # HTML DOCTYPE header
    variable htmlhead {<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">}
    set htmlhead {<!DOCTYPE HTML>}

    # convert an HTML fragment to HTML
    proc .x-text/html-fragment.text/html {rsp} {
	Debug.convert {x-text/html-fragment conversion: $rsp}
	#puts stderr "FRAGMENT: $rsp"
	set rspcontent [dict get $rsp -reply -content]

	if {[string match "<!DOCTYPE*" [string trimleft $rspcontent]]} {
	    # the content is already fully HTML
	    return [H Ok $rsp content-type text/html $rspcontent]	;# content is already fully HTML
	}

	# if response is wrapped HTML fragment
	# (signified by existence of a -wrapper element)
	# then run an extra conversion phase over it.
	set wrapper [dict get? $rsp -reply -wrapper]
	if {$wrapper ne ""} {
	    set wtype [dict get? $wrapper -type]
	    if {$wtype eq ""} {
		set wtype .style/sinorca.x-text/html-fragment
	    }
	    dict set wrapper content $rspcontent
	    dict set rsp -reply -content $wrapper
	    dict set rsp -reply content-type $wtype
	    
	    # expect there's a conversion from $wtype to html-fragment
	    set rsp [::convert convert $rsp]
	    set content [dict get $rsp -reply -content]
	    if {[string match "<!DOCTYPE*" $content]} {
		return [H Ok $rsp content-type text/html $content]	;# content is already fully HTML
	    }
	}

	dict set rsp -reply -raw 1

	variable htmlhead
	set content "${htmlhead}\n"
	    
	append content <html> \n
	append content <head> \n
	
	if {[dict exists $rsp -reply -title]} {
	    append content [<title> [armour [dict get $rsp -reply -title]]] \n
	}

	if {[dict exists $rsp -reply -headers]} {
	    append content [join [dict get $rsp -reply -headers] \n] \n
	}

	# add script and style preloads
	set preloads {}

	if {[dict exists $rsp -reply -prescript]} {
	    dict for {n v} [dict get $rsp -reply -prescript] {
		if {[string match !* $n]} {
		    lappend preloads [<script> $v]
		} else {
		    lappend preloads [<script> src $n {*}$v]
		}
	    }
	}

	if {[dict exists $rsp -reply -style]} {
	    dict for {n v} [dict get $rsp -reply -style] {
		if {[string match !* $n]} {
		    lappend preloads $v
		} else {
		    lappend preloads [<stylesheet> $n {*}$v]
		    Debug.cssloader {$n $v}
		}
	    }
	}
	if {$preloads ne {}} {
	    append content [join $preloads \n] \n
	}
	
	append content </head> \n
	
	append content <body> \n
	append content $rspcontent
	
	# stuff to start on google api
	if {[dict exists $rsp -reply -google]} {
	    append google "google.setOnLoadCallback(function() \{" \n
	    append google [join [dict get $rsp -reply -google] \n] \n
	    append google "\});" \n
	    append content [<script> $google]
	}
	
	# add script postscripts
	if {[dict exists $rsp -reply -script]} {
	    dict for {n v} [dict get $rsp -reply -script] {
		if {[string match !* $n]} {
		    append content \n $v \n
		} else {
		    append content \n [<script> src $n {*}$v] \n
		    Debug.jsloader {$n $v}
		}
	    }
	}

	# add script postloads - can't remove this until WubWikit's fixed
	if {[dict exists $rsp -reply -postload]} {
	    append content \n [join [dict get $rsp -reply -postload] \n] \n
	}
		
	append content </body> \n
	append content </html> \n

	Debug.convert {x-text/html-fragment DONE: $rsp}
	#puts stderr "FRAGMENT done: $rsp"
	return [H Ok $rsp content-type text/html $content]
    }

    # an in-band redirection
    proc .x-system/redirect.text/html {rsp} {
	set to [dict get $rsp -reply -content]
	return [H Redirect $rsp $to]
    }

    # convert system text to an HTML fragment
    proc .x-text/system.x-text/html-fragment {rsp} {
	# split out headers
	set headers ""
	set body [split [string trimleft [dict get $rsp -reply -content] \n] \n]
	set start 0
	set headers {}

	foreach line $body {
	    set line [string trim $line]
	    if {[string match <* $line]} break

	    incr start
	    if {$line eq ""} continue

	    # this is a header line
	    set val [lassign [split $line :] tag]
	    if {$tag eq "title"} {
		dict append rsp -reply -title $val
	    } else {
		dict lappend rsp -reply -headers [<$tag> [string trim [join $val]]]
	    }
	}

	set content "[join [lrange $body $start end] \n]\n"

	return [H Ok $rsp content-type x-text/html-fragment $content]
    }
}
