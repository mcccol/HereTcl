package require TclOO

package require Debug
Debug define file

Debug define caching

package provide File 3.0

if {[llength [info commands ::oo::Helpers::classvar]] == 0} {
    proc ::oo::Helpers::classvar {name args} {
	set self [uplevel 1 self]
	set ns [info object namespace [info object class $self]]
	set result {}
	foreach v [list $name {*}$args] {
	    uplevel 1 [list namespace upvar $ns $v $v]
	    lappend result ${ns}::$v
	}
	return $result
    }
}

oo::class create File {
    method File {r path} {
	# set mime type
	classvar mime
	set ext [string trim [file extension $path] .]
	if {[dict exists $mime $ext]} {
	    dict set r -reply content-type [dict get $mime $ext]
	}

	# load the file contents
	set fd [open $path r]
	chan configure $fd -translation binary
	dict set r -reply -content [read $fd]
	close $fd

	dict set r -reply -code 200
	return $r
    }

    method Directory {r path} {
	#puts stderr "Directory '$path' ($r)"
	set result {}
	lappend result "<li><a href='..'>..</a></li>"
	set upath [string trimright [dict get $r -Url path] /]
	foreach file [lsort [glob -directory $path *]] {
	    #puts stderr "DirEl '$file' - '$upath'"
	    set name [file tail $file][expr {[file type $file] eq "directory"?"/":""}]
	    lappend result "<li><a href='$upath/$name'>$name</a></li>"
	}
	return [H Ok $r content-type text/html <ul>[join $result \n]</ul>]
    }

    method Link {r path} {
	# we have found a link, now follow it
	set linked [file link $path]
	if {[file pathtype $linked] eq "relative"} {
	    set linked [file join [file dirname $path] $linked]
	}
	tailcall my [string totitle [file type $linked]] $r $linked
    }

    # method Characterspecial
    # method Blockspecial
    # method Fifo
    # method Socket

    method do {r} {
	set path {}
	foreach el [split [dict get $r -Url extra] /] {
	    if {$el eq "."} continue
	    if {$el eq ".."} {
		set path [lrange $path 0 end-1]
	    } else {
		lappend path $el
	    }
	}
	set opath $path
	variable root; set path [file join $root {*}$path]

	if {![file exists $path]} {
	    # if the file doesn't exist, say so.
	    return [H NotFound $r "<p>File '$opath' doesn't exist.</p>"]
	}

	# handle conditional request
	if {[dict exists $r if-modified-since]} {
	    set since [H DateInSeconds [dict get $r if-modified-since]]
	    if {[file mtime $path] <= $since} {
		Debug.file {NotModified: $path - [H Date [file mtime $path]] < [dict get $r if-modified-since]}
		Debug.file {if-modified-since: not modified}
		return [H NotModified $r]
	    }
	}

	Debug.file {Found file '$path' of type [file type $path]}

	try {
	    my [string totitle [file type $path]] $r $path
	} on ok {r} {
	    # allow client caching
	    variable expires
	    if {![dict exists $r -expiry]
		&& [info exists expires]} {
		dict set r -reply -expiry $expires
	    }
	    if {[dict exists $r -expiry]} {
		variable crealm; set r [H Cache $r [dict get $r -expiry] $crealm]
	    }

	    # set the file mod time
	    set mtime [file mtime $path]
	    dict set r -reply last-modified [H Date $mtime]
	} on error {e eo} {
	    Debug.file {Error $e ($eo)}
	    set r [H NotFound $r "<p>File '$opath' doesn't exist. '$path' '$e' ($eo)</p>"]
	}
	return $r
    }

    constructor {args} {
	Debug.file {constructing File with ($args)}
	#variable expires 0	;# add an expiry to each response
	variable crealm ""	;# optionally make files 'public'

	variable {*}$args	;# allow .ini file to modify defaults
    }
}

# install mime mapping in File's namespace
namespace eval [info object namespace File] {
    variable mime {multipart/x-directory {} multipart/x-search {} multipart/x-aggregate {} sylk application/x-sylk slk application/x-sylk application/x-sylk {sylk slk} csv text/csv text/csv csv yaml text/yaml text/yaml yaml application/x-characterspecial {} application/x-blockspecial {} application/x-fifo {} application/x-socket {} kml application/vnd.google-earth.kml+xml application/vnd.google-earth.kml+xml kml kmz application/vnd.google-earth.kmz application/vnd.google-earth.kmz kmz gpx application/vnd.gpx+xml application/vnd.gpx+xml gpx cacert application/x-x509-ca-cert application/x-x509-ca-cert cacert ucert application/x-x509-user-cert application/x-x509-user-cert ucert scert application/x-x509-server-cert application/x-x509-server-cert scert ecert application/x-x509-email-cert application/x-x509-email-cert ecert htmf x-text/html-fragment x-text/html-fragment htmf stx x-text/stx x-text/stx stx form x-text/form x-text/form form tml application/x-tcl-template application/x-tcl-template tml stml application/x-session-template application/x-session-template stml application/x-climb-list {} application/activemessage {} application/andrew-inset {} application/applefile {} application/atomicmail {} application/dca-rft {} application/dec-dx {} hqx application/mac-binhex40 application/mac-binhex40 hqx application/macwriteii {} doc application/msword dot application/msword application/msword {doc dot} xls application/ms-excel xlw application/ms-excel xla application/ms-excel xlc application/ms-excel xlm application/ms-excel xlt application/ms-excel application/ms-excel {xls xlw xla xlc xlm xlt} ppt application/ms-powerpoint pps application/ms-powerpoint pot application/ms-powerpoint application/ms-powerpoint {ppt pps pot} mpp application/ms-project application/ms-project mpp mdb application/x-msaccess application/x-msaccess mdb wmf application/x-msmetafile application/x-msmetafile wmf wri application/x-mswrite application/x-mswrite wri scd application/x-msschedule application/x-msschedule scd application/news-message-id {} application/news-transmission {} bin application/octet-stream core application/octet-stream exe application/octet-stream tgz application/octet-stream gz application/octet-stream z application/octet-stream zip application/zip application/octet-stream {sun4 i86pc} sun4 application/octet-stream i86pc application/octet-stream oda application/oda application/oda oda pdf application/pdf application/pdf pdf ai application/postscript eps application/postscript ps application/postscript application/postscript {ai eps ps} application/remote-printing {} rtf text/rtf text/rtf rtf application/slate {} mif application/x-mif application/x-mif mif js application/javascript application/javascript js json application/jason application/jason json application/wita {} application/wordperfect5.1 {} cgi application/x-cgi application/x-cgi cgi csh application/x-csh application/x-csh csh dvi application/x-dvi application/x-dvi dvi hdf application/x-hdf application/x-hdf hdf latex text/x-latex text/x-latex latex nc application/x-netcdf cdf application/x-netcdf application/x-netcdf {nc cdf} pac application/x-ns-proxy-autoconfig application/x-ns-proxy-autoconfig pac thtml application/x-safetcl application/x-safetcl thtml shtml application/x-server-include application/x-server-include shtml sh application/x-sh application/x-sh sh tcl application/x-tcl application/x-tcl tcl subst application/x-tcl-subst application/x-tcl-subst subst auth application/x-tcl-auth application/x-tcl-auth auth snmp application/x-tcl-snmp application/x-tcl-snmp snmp xtcl application/x-tcl-connect application/x-tcl-connect xtcl tex text/x-tex text/x-tex tex texinfo application/x-texinfo texi application/x-texinfo application/x-texinfo {texinfo texi} t text/x-troff tr text/x-troff roff text/x-troff text/x-troff {t tr roff} man application/x-troff-man application/x-troff-man man me application/x-troff-me application/x-troff-me me ms application/x-troff-ms application/x-troff-ms ms src application/x-wais-source application/x-wais-source src map application/x-imagemap application/x-imagemap map application/zip zip bcpio application/x-bcpio application/x-bcpio bcpio cpio application/x-cpio application/x-cpio cpio gtar application/x-gtar application/x-gtar gtar shar application/x-shar application/x-shar shar sv4cpio application/x-sv4cpio application/x-sv4cpio sv4cpio sv4crc application/x-sv4crc application/x-sv4crc sv4crc tar application/x-tar application/x-tar tar ustar application/x-ustar application/x-ustar ustar patch text/x-patch diff text/x-patch text/x-patch {patch diff} au audio/basic snd audio/basic audio/basic {au snd} aif audio/x-aiff aiff audio/x-aiff aifc audio/x-aiff audio/x-aiff {aif aiff aifc} wav audio/x-wav audio/x-wav wav ico image/vnd.microsoft.icon image/vnd.microsoft.icon ico png image/png image/png png svg image/svg+xml image/svg+xml svg gif image/gif image/gif gif ief image/ief image/ief ief jpeg image/jpeg jpg image/jpeg jpe image/jpeg image/jpeg {jpeg jpg jpe} tiff image/tiff tif image/tiff image/tiff {tiff tif} ras image/x-cmu-raster image/x-cmu-raster ras pnm image/x-portable-anymap image/x-portable-anymap pnm pbm image/x-portable-bitmap image/x-portable-bitmap pbm pgm image/x-portable-graymap image/x-portable-graymap pgm ppm image/x-portable-pixmap image/x-portable-pixmap ppm rgb image/x-rgb image/x-rgb rgb xbm image/x-xbitmap image/x-xbitmap xbm xpm image/x-xpixmap image/x-xpixmap xpm xwd image/x-xwindowdump image/x-xwindowdump xwd message/external-body {} message/news {} message/partial {} message/rfc822 {} multipart/alternative {} multipart/appledouble {} multipart/digest {} multipart/mixed {} multipart/parallel {} html text/html htm text/html text/html {html htm} txt text/plain text/plain txt rtx text/richtext text/richtext rtx tsv text/tab-separated-values text/tab-separated-values tsv etx text/x-setext text/x-setext etx xml text/xml text/xml xml xhtml application/xhtml+xml application/xhtml+xml xhtml css text/css text/css css mpeg video/mpeg mpg video/mpeg mpe video/mpeg video/mpeg {mpeg mpg mpe} qt video/quicktime mov video/quicktime video/quicktime {qt mov} avi video/x-msvideo video/x-msvideo avi movie video/x-sgi-movie video/x-sgi-movie movie flv video/x-flv video/x-flv flv wml text/vnd.wap.wml text/vnd.wap.wml wml wmls text/vnd.wap.wmlscript text/vnd.wap.wmlscript wmls wmlc application/vnd.wap.wmlc application/vnd.wap.wmlc wmlc wmlsc application/vnd.wap.wmscriptc application/vnd.wap.wmscriptc wmlsc wbmp image/vnd.wap.wbmp image/vnd.wap.wbmp wbmp mp3 audio/mpeg audio/mpeg mp3 nmf application/x-nacl application/x-nacl nmf
    }
}
