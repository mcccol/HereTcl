package provide Http 3.0

namespace eval Http {
    # convert HTTP date to time
    proc DateInSeconds {date} {
	if {[string is integer -strict $date]} {
	    return $date
	} elseif {[catch {clock scan $date \
			-format {%a, %d %b %Y %T GMT} \
			-gmt true} result eo]
	      } {
	    #error "DateInSeconds '$date', ($result)"
	    return 0	;# oldest possible date
	} else {
	    return $result
	}
    }

    # return an HTTP date
    proc Date {{seconds ""}} {
	if {$seconds eq ""} {
	    set seconds [clock seconds]
	}

	return [clock format $seconds -format {%a, %d %b %Y %T GMT} -gmt true]
    }

    # internal redirection generator
    proc genRedirect {title code rsp to {content ""} {ctype "text/html"} args} {
	if {$to eq ""} {
	    set to [dict get $rsp -url]
	}
	set to [Url redir $rsp $to {*}$args]

	if {$content ne ""} {
	    dict set rsp -content $content
	    dict set rsp content-type $ctype
	} else {
	    dict set rsp -content [<html> {
		[<head> {[<title> $title]}]
		[<body> {
		    [<h1> $title]
		    [<p> "The page may be found here: <a href='[H armour $to]'>[H armour $to]"]
		}]
	    }]
	    dict set rsp content-type "text/html"
	}

	dict set rsp location $to
	dict set rsp -code $code

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

    # construct an HTTP NotModified response
    proc NotModified {rsp} {
	# remove content-related stuff
	foreach n [dict keys $rsp content-*] {
	    if {$n ne "content-location"} {
		dict unset rsp $n
	    }
	}

	# discard some fields
	set rsp [dict filter $rsp script {k v} {
	    expr {$k ni {transfer-encoding -chunked -content}}
	}]

	# the response MUST NOT include other entity-headers
	# than Date, Expires, Cache-Control, Vary, Etag, Content-Location
	dict set result -code 304

	return $result
    }

    # contents may not be Cached
    proc NoCache {rsp} {
	dict set rsp cache-control "no-store, no-cache, must-revalidate, max-age=0, post-check=0, pre-check=0"; # HTTP/1.1
	dict set rsp expires "Sun, 01 Jul 2005 00:00:00 GMT"	;# deep past
	dict set rsp pragma "no-cache"	;# HTTP/1.0
	return $rsp
    }

    proc NotFound {rsp {message "<P>Not Found</P>"}} {
	dict set rsp -content $message
	dict set rsp -code 404
	return [NoCache $rsp]
    }
    
    # contents may be Cached
    proc Cache {rsp {age 0} {realm ""}} {
	if {[string is integer -strict $age]} {
	    # it's an age
	    if {$age != 0} {
		dict set rsp expires [Date [expr {[clock seconds] + $age}]]
		Debug.caching {Http Cache: numeric age expires '[dict get $rsp expires]'}
	    } else {
		Debug.caching {Http Cache: turn off expires}
		catch {dict unset rsp expires}
		catch {dict unset rsp -expiry}
	    }
	} else {
	    dict set rsp -expiry $age	;# remember expiry verbiage for caching
	    dict set rsp expires [Date [clock scan $age]]
	    Debug.caching {Http Cache: text age expires '$age' - '[dict get $rsp expires]'}
	    set age [expr {[clock scan $age] - [clock seconds]}]
	}

	if {$realm ne ""} {
	    dict set rsp cache-control $realm
	}

	if {$age} {
	    if {[dict exists $rsp cache-control]} {
		dict append rsp cache-control ",max-age=$age"
	    } else {
		dict set rsp cache-control "max-age=$age"
	    }
	}

	Debug.caching {Http Cache: ($age) cache-control: [dict get? $rsp cache-control]}
	return $rsp
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
