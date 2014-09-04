# Hredir - redirection support for H

# internal redirection generator
proc genRedirect {title code rsp to {content ""} {ctype "text/html"} args} {
    if {$to eq ""} {
	set to [dict get $rsp -Url]
    }
    set to [Url redir $rsp $to {*}$args]

    if {$content ne ""} {
	dict set rsp -reply -content $content
	dict set rsp -reply content-type $ctype
    } else {
	dict set rsp -reply -content [<html> {
	    [<head> {[<title> $title]}]
	    [<body> {
		[<h1> $title]
		[<p> "The page may be found here: <a href='[H armour $to]'>[H armour $to]"]
	    }]
	}]
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
    return [Http genRedirect Redirect 302 $rsp $to $content $ctype {*}$args]
}

# construct a simple HTTP Redirect response with extra query
proc Redir {rsp {to ""} args} {
    return [Http genRedirect Redirect 302 $rsp $to "" "" {*}$args]
}

# construct an HTTP Redirect response to Referer of request
proc RedirectReferer {rsp {content ""} {ctype ""} args} {
    set ref [Referer $rsp]
    if {$ref eq ""} {
	set ref /
    }
    return [Http genRedirect Redirect 302 $rsp $ref $content $ctype {*}$args]
}

# construct an HTTP Found response
proc Found {rsp {to ""} {content ""} {ctype "text/html"} args} {
    return [Http genRedirect Redirect 302 $rsp $to $content $ctype {*}$args]
}

# construct an HTTP Relocated response
proc Relocated {rsp {to ""} {content ""} {ctype "text/html"} args} {
    return [Http genRedirect Relocated 307 $rsp $to $content $ctype {*}$args]
}

# construct an HTTP SeeOther response
proc SeeOther {rsp {to ""} {content ""} {ctype "text/html"} args} {
    return [Http genRedirect SeeOther 303 $rsp $to $content $ctype {*}$args]
}

# construct an HTTP Moved response
proc Moved {rsp {to ""} {content ""} {ctype "text/html"} args} {
    return [Http genRedirect Moved 301 $rsp $to $content $ctype {*}$args]
}
