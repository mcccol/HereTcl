# Herr - H error reporting support
variable errStyle {
    html * { padding:0; margin:0; }
    body * { padding:10px 20px; }
    body * * { padding:0; }
    body { font:small sans-serif; }
    body>div { border-bottom:1px solid #ddd; }
    h1 { font-weight:normal; }
    h2 { margin-bottom:.8em; }
    h2 span { font-size:80%; color:#666; font-weight:normal; }
    h3 { margin:1em 0 .5em 0; }
    table {
	border:1px solid #ccc; border-collapse: collapse; background:white; }
    tbody td, tbody th { vertical-align:top; padding:2px 3px; }
    thead th {
	padding:1px 6px 1px 3px; background:#fefefe; text-align:left;
	font-weight:normal; font-size:11px; border:1px solid #ddd; }
    tbody th { text-align:right; color:#666; padding-right:.5em; }
    table.errorinfo { margin:5px 0 2px 40px; }
    table.errorinfo td, table.dict td { font-family:monospace; }
    #summary { background: #ffc; }
    #summary h2 { font-weight: normal; color: #666; }
    #errorinfo { background:#eee; }
    #details { background:#f6f6f6; padding-left:120px; }
    #details h2, #details h3 { position:relative; margin-left:-100px; }
    #details h3 { margin-bottom:-1em; }
}

# ErrorMsg - generate an error message with optional response fields
proc ErrorMsg {r error eo} {
    if {[string length $error] > 80} {
	set title [string range $error 0 80]...
    } else {
	set title $error
    }

    variable errStyle
    set html <html>\n<head>\n
    append html "<style type='text/css'>" $errStyle </style> \n
    append html <title> $title </title> \n
    append html </head> \n

    # format up the page
    append html <body> \n <H2> $title </H2> \n

    append html "<div id='summary'>" $error </div> \n
    
    append html "<div id='errorinfo'>"
    if {$eo ne ""} {
	append html <H2> "Error Code " '[dict get $eo -errorcode]' </H2>
	dict unset eo -errorcode
	
	append html <pre> [H tclarmour [H armour [dict get $eo -errorinfo]]] </pre>
	dict unset eo -errorinfo
	append html "<table class='errorinfo'>" \n
	append html <tbody> \n
	foreach {n1 v1} [dict get $eo -errorstack] {
	    append html <tr> <td> [H tclarmour [H armour $n1]] </td> <td> [H tclarmour [H armour $v1]] </td> </tr> \n
	}
	dict unset eo -errorstack
	
	dict for {n v} $eo {
	    append html <tr> <td> [H tclarmour [H armour $n]] </td> <td> <pre> [H tclarmour [H armour $v]] </pre> </td> </tr> \n
	}
	append html </tbody> \n </table> \n
    }
    
    catch {append html <p> "Caller: " <code> [H tclarmour [H armour [info level -1]]] </code> </p>}
    
    append html </div> \n </body> \n
    return [list $html]
}

# construct an HTTP response containing a server error page
proc ServerError {rq error {eo ""}} {
    #puts stderr "ServerError: $error ($eo)"
    if {[dict exists $eo -debug]} {
	set elevel [dict get $eo -debug]
    } else {
	set elevel 0
    }
    Debug.error {Server Error: '$error' ($eo) $rq} $elevel

    corovar close; set close $error	;# this will cause the reader to close

    try {
	set error [H tclarmour [H armour $error]]
	set args [lassign [ErrorMsg $rq $error $eo] content]
    } on error {e1 eo1} {
	Debug.error {Recursive Server Error: '$e1' ($eo1)}
	set content [H tclarmour [H armour $error]]\n[H tclarmour [H armour $eo]]
    } finally {
	dict update rq -reply rsp {
	    dict set rsp -code 500
	    dict set rsp content-type text/html
	    set rsp [dict merge $rsp $args]

	    # Errors are completely dynamic - no caching!
	    if {![info exists rsp]} {
		set rsp [NoCache]
	    } else {
		set rsp [NoCache $rsp]
	    }
	    dict set rsp -content $content
	}
	return $rq
    }
}
