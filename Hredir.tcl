# Hredir - redirection support for H
Debug define url

# localuri - return a local uri, no host scheme or port
proc localuri {x args} {
    if {[llength $args] == 1} {
	set args [lindex $args 0]
    }
    set result [dict get $x path]

    foreach {part pre post} {
	query ? ""
	fragment \# ""
    } {
	if {[dict exists $x $part]} {
	    append result "${pre}[dict get $x $part]${post}"
	}
    }
    return $result
}

# process a possibly local URI for redirection
# provides a limited ability to add query $args
# limits: overwrites existing args, ignores and removes duplicates
proc redir {defaults to args} {
    Debug.url {redir defaults:$defaults to:$to args:$args}
    #puts stderr "redir defaults:($defaults) to:$to args:$args"
    if {[llength $args] == 1} {
	set args [lindex $args 0]
    }

    set todict [freeparse_url $to $defaults]	;# parse the destination URL

    if {[dict exists $todict -query]} {
	foreach {n v} [Query flatten [Query parse $todict]] {
	    dict set query $n $v
	}
    } else {
	set query {}
    }

    # parse args as additional -query elements
    foreach {name val} $args {
	dict set query $name $val
    }

    set q ""
    dict for {n v} $query {
	if {$v ne ""} {
	    lappend q "$n=[Query encode $v]"
	} else {
	    lappend q $n
	}
    }
    if {$q ne {}} {
	dict set todict -query [join $q &]
    }

    if {([dict get? $todict -host] ni [list "" [dict get? $defaults -host]])
	|| ([dict get? $todict -port] ni [list "" [dict get? $defaults -port]])
	|| ([dict get? $todict -scheme] ni [list "" [dict get? $defaults -scheme]])
    } {
	# this is a remote URL
	set to [uri $todict]
    } else {
	# local URL - no scheme, host, port, etc.
	set to [localuri $todict]
    }

    Debug.url {redir to: $to}
    return $to
}

# internal redirection generator
proc genRedirect {title code rsp to {content ""} {ctype "text/html"} args} {
    if {$to eq ""} {
	set to [dict get $rsp -Url]
    }
    set to [redir $rsp $to {*}$args]

    if {$content ne ""} {
	dict set rsp -reply -content $content
	dict set rsp -reply content-type $ctype
    } else {
	dict set rsp -reply -content [subst {<html><head> <title>$title</title><body><h1>$title</h1><p>The page may be found here: <a href='[armour $to]'>[armour $to]</a></p></body></html>}]
	dict set rsp -reply content-type "text/html"
    }

    dict set rsp -reply location $to
    dict set rsp -reply -code $code

    return $rsp
}

# discover Referer of request
proc Referer {req} {
    if {[dict exists $req referer]} {
	return [dict get $req referer]
    } else {
	return ""
    }
}

# construct an HTTP Redirect response
proc Redirect {rsp {to ""} {content ""} {ctype "text/html"} args} {
    return [genRedirect Redirect 302 $rsp $to $content $ctype {*}$args]
}

# construct a simple HTTP Redirect response with extra query
proc Redir {rsp {to ""} args} {
    return [genRedirect Redirect 302 $rsp $to "" "" {*}$args]
}

# construct an HTTP Redirect response to Referer of request
proc RedirectReferer {rsp {content ""} {ctype ""} args} {
    set ref [Referer $rsp]
    if {$ref eq ""} {
	set ref /
    }
    return [genRedirect Redirect 302 $rsp $ref $content $ctype {*}$args]
}

# construct an HTTP Found response
proc Found {rsp {to ""} {content ""} {ctype "text/html"} args} {
    return [genRedirect Redirect 302 $rsp $to $content $ctype {*}$args]
}

# construct an HTTP Relocated response
proc Relocated {rsp {to ""} {content ""} {ctype "text/html"} args} {
    return [genRedirect Relocated 307 $rsp $to $content $ctype {*}$args]
}

# construct an HTTP SeeOther response
proc SeeOther {rsp {to ""} {content ""} {ctype "text/html"} args} {
    return [genRedirect SeeOther 303 $rsp $to $content $ctype {*}$args]
}

# construct an HTTP Moved response
proc Moved {rsp {to ""} {content ""} {ctype "text/html"} args} {
    return [genRedirect Moved 301 $rsp $to $content $ctype {*}$args]
}
