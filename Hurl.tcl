proc tclarmour {text} {
    string map {\[ "&#x5B;" \] "&#x5D;" \{ "&#x7B;" \} "&#x7D;" $ "&#x24;"} $text
}
proc armour {text} {
    string map {
	&\# &\# ' &\#39;
	\xa0 &nbsp; \xa1 &iexcl; \xa2 &cent; \xa3 &pound; \xa4 &curren;
	\xa5 &yen; \xa6 &brvbar; \xa7 &sect; \xa8 &uml; \xa9 &copy;
	\xaa &ordf; \xab &laquo; \xac &not; \xad &shy; \xae &reg;
	\xaf &macr; \xb0 &deg; \xb1 &plusmn; \xb2 &sup2; \xb3 &sup3;
	\xb4 &acute; \xb5 &micro; \xb6 &para; \xb7 &middot; \xb8 &cedil;
	\xb9 &sup1; \xba &ordm; \xbb &raquo; \xbc &frac14; \xbd &frac12;
	\xbe &frac34; \xbf &iquest; \xc0 &Agrave; \xc1 &Aacute; \xc2 &Acirc;
	\xc3 &Atilde; \xc4 &Auml; \xc5 &Aring; \xc6 &AElig; \xc7 &Ccedil;
	\xc8 &Egrave; \xc9 &Eacute; \xca &Ecirc; \xcb &Euml; \xcc &Igrave;
	\xcd &Iacute; \xce &Icirc; \xcf &Iuml; \xd0 &ETH; \xd1 &Ntilde;
	\xd2 &Ograve; \xd3 &Oacute; \xd4 &Ocirc; \xd5 &Otilde; \xd6 &Ouml;
	\xd7 &times; \xd8 &Oslash; \xd9 &Ugrave; \xda &Uacute; \xdb &Ucirc;
	\xdc &Uuml; \xdd &Yacute; \xde &THORN; \xdf &szlig; \xe0 &agrave;
	\xe1 &aacute; \xe2 &acirc; \xe3 &atilde; \xe4 &auml; \xe5 &aring;
	\xe6 &aelig; \xe7 &ccedil; \xe8 &egrave; \xe9 &eacute; \xea &ecirc;
	\xeb &euml; \xec &igrave; \xed &iacute; \xee &icirc; \xef &iuml;
	\xf0 &eth; \xf1 &ntilde; \xf2 &ograve; \xf3 &oacute; \xf4 &ocirc;
	\xf5 &otilde; \xf6 &ouml; \xf7 &divide; \xf8 &oslash; \xf9 &ugrave;
	\xfa &uacute; \xfb &ucirc; \xfc &uuml; \xfd &yacute; \xfe &thorn;
	\xff &yuml; \u192 &fnof; \u391 &Alpha; \u392 &Beta; \u393 &Gamma;
	\u394 &Delta; \u395 &Epsilon; \u396 &Zeta; \u397 &Eta; \u398 &Theta;
	\u399 &Iota; \u39A &Kappa; \u39B &Lambda; \u39C &Mu; \u39D &Nu;
	\u39E &Xi; \u39F &Omicron; \u3A0 &Pi; \u3A1 &Rho; \u3A3 &Sigma;
	\u3A4 &Tau; \u3A5 &Upsilon; \u3A6 &Phi; \u3A7 &Chi; \u3A8 &Psi;
	\u3A9 &Omega; \u3B1 &alpha; \u3B2 &beta; \u3B3 &gamma; \u3B4 &delta;
	\u3B5 &epsilon; \u3B6 &zeta; \u3B7 &eta; \u3B8 &theta; \u3B9 &iota;
	\u3BA &kappa; \u3BB &lambda; \u3BC &mu; \u3BD &nu; \u3BE &xi;
	\u3BF &omicron; \u3C0 &pi; \u3C1 &rho; \u3C2 &sigmaf; \u3C3 &sigma;
	\u3C4 &tau; \u3C5 &upsilon; \u3C6 &phi; \u3C7 &chi; \u3C8 &psi;
	\u3C9 &omega; \u3D1 &thetasym; \u3D2 &upsih; \u3D6 &piv;
	\u2022 &bull; \u2026 &hellip; \u2032 &prime; \u2033 &Prime;
	\u203E &oline; \u2044 &frasl; \u2118 &weierp; \u2111 &image;
	\u211C &real; \u2122 &trade; \u2135 &alefsym; \u2190 &larr;
	\u2191 &uarr; \u2192 &rarr; \u2193 &darr; \u2194 &harr; \u21B5 &crarr;
	\u21D0 &lArr; \u21D1 &uArr; \u21D2 &rArr; \u21D3 &dArr; \u21D4 &hArr;
	\u2200 &forall; \u2202 &part; \u2203 &exist; \u2205 &empty;
	\u2207 &nabla; \u2208 &isin; \u2209 &notin; \u220B &ni; \u220F &prod;
	\u2211 &sum; \u2212 &minus; \u2217 &lowast; \u221A &radic;
	\u221D &prop; \u221E &infin; \u2220 &ang; \u2227 &and; \u2228 &or;
	\u2229 &cap; \u222A &cup; \u222B &int; \u2234 &there4; \u223C &sim;
	\u2245 &cong; \u2248 &asymp; \u2260 &ne; \u2261 &equiv; \u2264 &le;
	\u2265 &ge; \u2282 &sub; \u2283 &sup; \u2284 &nsub; \u2286 &sube;
	\u2287 &supe; \u2295 &oplus; \u2297 &otimes; \u22A5 &perp;
	\u22C5 &sdot; \u2308 &lceil; \u2309 &rceil; \u230A &lfloor;
	\u230B &rfloor; \u2329 &lang; \u232A &rang; \u25CA &loz;
	\u2660 &spades; \u2663 &clubs; \u2665 &hearts; \u2666 &diams;
	\x22 &quot; \x26 &amp; \x3C &lt; \x3E &gt; \u152 &OElig;
	\u153 &oelig; \u160 &Scaron; \u161 &scaron; \u178 &Yuml;
	\u2C6 &circ; \u2DC &tilde; \u2002 &ensp; \u2003 &emsp; \u2009 &thinsp;
	\u200C &zwnj; \u200D &zwj; \u200E &lrm; \u200F &rlm; \u2013 &ndash;
	\u2014 &mdash; \u2018 &lsquo; \u2019 &rsquo; \u201A &sbquo;
	\u201C &ldquo; \u201D &rdquo; \u201E &bdquo; \u2020 &dagger;
	\u2021 &Dagger; \u2030 &permil; \u2039 &lsaquo; \u203A &rsaquo;
	\u20AC &euro;
    } $text
}

# Support for x-www-urlencoded character mapping
# The spec says: "non-alphanumeric characters are replaced by '%HH'"
variable dmap {%0D%0A \n %0d%0a \n %% %}

# set up non-alpha map
::apply [list {} {
    variable dmap
    for {set i 0} {$i < 256} {incr i} {
	set c [format %c $i]
	lappend dmap %[format %.2X $i] [binary format c $i]
	lappend dmap %[format %.2x $i] [binary format c $i]
    }
} [namespace current]]

# decode - decode data in www-url-encoded format.
proc decode {str} {
    variable dmap
    set str [string map $dmap $str]
    set str [encoding convertfrom utf-8 $str]
    return $str
}

# normalize - strip redundant and potentially damaging path elements from path
proc normalize {url} {
    while {[set new [regsub -all {(/+)|(^[.][.]/)|(^/[.][.])|(/[^/]+/[.][.]$)|(/[^/]+/[.][.]/)|(^[.]/)|(/[.]$)|(/[.]/)|(^[.][.]$)|(^[.]$)} $url /]] ne $url} {
	set url $new
    }
    return "/[string trimleft $url /]"
}

# path- parse a url path+fragment+query into its constituent parts
proc path {url} {
    array set x {}
    regexp {^([^?\#]*)([?]([^\#]*))?(\#(.*))?$} $url -> x(path) . x(query) . x(fragment)
    set x(path) [normalize [decode $x(path)]]	;# fix up oddities in URLs

    foreach n [array names x] {
	if {$x($n) eq ""} {
	    unset x($n)
	}
    }

    return [array get x]
}

proc url {args} {
    if {[llength $args] == 1} {
	set args [lindex $args 0]
    }
    if {![dict exists $args scheme]} {
	dict set args scheme http	;# need a default.
    }

    # minimize port
    if {[dict exists $args port]} {
	if {[dict get $args port] eq ""} {
	    dict unset args port
	} elseif {[dict get $args scheme] eq "http" && [dict get $args port] eq "80"} {
	    dict unset args port
	} elseif {[dict get $args scheme] eq "https" && [dict get $args port] eq "443"} {
	    dict unset args port
	} elseif {[dict get $args scheme] eq "ftp" && [dict get $args port] eq "21"} {
	    dict unset args port
	}
    }

    foreach {part pre post} {
	scheme "" :/
	host / ""
	port : ""
	path "" ""
    } {
	if {[dict exists $args $part]} {
	    append result "${pre}[dict get $args $part]${post}"
	}
    }

    return $result
}

# parsePath_url --
#
#	parse a url path+fragment+query into its constituent parts
#
# Arguments:
#	args	url to parse
#
# Results:
#	array form of parsed URL elements
#
# Side Effects:
#	none

proc parsePath_url {url {normalize 1}} {
    Debug.H.url {Url parsePath $url - norm? $normalize}
    array set x {}
    regexp {^([^?\#]*)([?]([^\#]*))?(\#(.*))?$} $url \
	-> x(path) . x(query) . x(fragment)

    Debug.H.url {Url parsePath 1: $url -> [array get x]}

    if {$normalize} {
	set x(path) [normalize [decode $x(path)]]	;# fix up oddities in URLs
	set x(normalized) 1
    }

    foreach n [array names x] {
	if {$x($n) eq ""} {
	    unset x($n)
	}
    }

    Debug.H.url {Url parsePath: $url -> [array get x]}

    return [array get x]
}

proc parse_url {url} {
    array set x {}
    regexp {^(([^:/?\#]+):)?(//([^/?\#]*))?([^?\#]*)([?]([^\#]*))?(\#(.*))?$} $url \
	-> . x(scheme) . x(authority) x(path) . x(query) . x(fragment)
    regexp {^(([^@]+)@)?([^@:]+)?(:([0-9]+))?$} $x(authority) \
	-> . x(authority) x(host) . x(port)

    set x(path) [normalize [decode $x(path)]]	;# fix up oddities in URLs

    foreach n [array names x] {
	if {$x($n) eq ""} {
	    unset x($n)
	}
    }

    if {[info exists x(host)]} {
	# clean up host - check its validity?
	set x(host) [string tolower $x(host)]
    }

    if {[info exists x(scheme)]} {
	# clean up scheme - check its validity?
	set x(scheme) [string tolower $x(scheme)]
    } else {
	set x(scheme) http
    }

    if {[info exists x(scheme)]} {
	set x(url) [url [array get x]]
    } else {
	#set x(scheme) http
    }

    return [array get x]
}

# freeparse_url - parse a free form url-ish string
proc freeparse_url {urlish args} {
    if {[llength $args] == 1} {
	set args [lindex $args 0]
    }

    # set defaults in url dict from args
    set result {scheme http}
    foreach {f def} {scheme host port} {
	if {[dict exists $args $f]} {
	    dict set result $f [dict get $args $f]
	}
    }

    switch -nocase -glob -- $urlish {
	http* {
	    # full URL
	    set result [parse_url $urlish]
	}
	//* {
	    # host-absolute path-absolute - parse+normalize
	    set urlish /[join [lassign [split $urlish /] -> host] /]
	    set result [dict merge $result [list host $host] [parsePath_url $urlish]]
	}
	/* {
	    # host-relative path-absolute - parse+normalize
	    set result [dict merge $result [parsePath_url $urlish]]
	}
	default {
	    # host-relative path-relative - parse but don't normalize
	    set result [dict merge $result [parsePath_url $urlish 0]]
	}
    }
    return $result
}

# host - construct the host part of a URL dict
proc host {x} {
    if {[dict exists $x port]
	&& [dict get $x port] ne ""
	&& [dict get $x port] != 80} {
	return [dict get $x host]:[dict get $x port]
    } else {
	return [dict get $x host]
    }
}
