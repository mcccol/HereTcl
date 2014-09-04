# Hproc - request processing support
#
# This is a full-service request processor, with all the extras

variable methods {GET PUT POST HEAD OPTIONS}
variable maxurilen 0	;# maximum length of URI

proc rxLint {R} {
    Debug.httpdlow {rxLint $R}
    set headers [split [dict get $R -Header full]]

    # ensure the HTTP method is acceptable
    set method [string toupper [lindex $headers 0]]
    dict set R -Header method $method

    # ensure the HTTP method is acceptable
    variable methods
    if {$method ni $methods} {
	Debug.httpdlow {rxLint - unsupported method [info level] [info coroutine]}
	#corovar socket; tailcall T::$socket reply [Bad $R "Method unsupported '$method'" 405]
	tailcall Bad $R "Method unsupported '$method'" 405
    }

    # ensure the HTTP version is acceptable
    if {[dict get $R -Header version] ni {1.1 1.0}} {
	# Send 505 for protocol != HTTP/1.0 or HTTP/1.1
	tailcall Bad $R "HTTP Version '[dict get $R -Header version]' not supported" 505
    }

    # ensure the URI is plausible
    set uri [dict get $R -Header uri]
    variable maxurilen
    if {$maxurilen && [string length $uri] > $maxurilen} {
	# send a 414 back
	tailcall Bad $R "URI too long '$uri'" 414
    }

    return $R
}

# Pre-Process the request:
# Run all the $pre commands - ignoring errors, but permitting them usurp the process
#	return - the pre-process has decided to overrule process
#	continue - the pre-process has nothing to add
#	break - the pre-process says continue straight to Process
#	error - the pre-process failed - skip it
#	ok - the preprocess has modified the request
proc Pre {r pre} {
    foreach P $pre {
	Debug.httpd {TRY pre '$P'}
	try {
	    uplevel 1 [list {*}$P $r]
	} on return {r} {
	    # the pre-process has decided to usurp processing
	    Debug.httpd {pre '$P' - the pre-process has decided to usurp processing}
	    corovar socket
	    tailcall T::$socket reply $r
	} on continue {} {
	    Debug.httpd {pre '$P' - the pre-process has nothing to add}
	    # the pre-process has nothing to add
	} on break {r} {
	    Debug.httpd {pre '$P' - the pre-process says skip the rest of the pre-processes}
	    # the pre-process says skip the rest of the pre-processes,
	    # continue with the process
	    break
	} on error {e eo} {
	    Debug.httpd {pre '$P' - the pre-process has failed '$e' ($eo)}
	    # the pre-process failed - skip it
	} on ok {r} {
	    # the preprocess returned a response, consume it
	    Debug.httpd {pre '$P' - the pre-process has returned a response ($r)}
	}
    }
    return $r
}

# Post-Process the response:
# Run all the $post commands - ignoring errors, but permitting them usurp the process
#	return - the post-process has returned a reply, use it, skip the rest of post-processing
#	continue - the post-process has nothing to add, continue with post-processing
#	break - the post-process says skip the rest of post-processing, it will handle its own reply
#	error - the post-process failed - skip it
#	ok - the post-process has modified the request, consume it and continue post-processing
proc Post {r post} {
    foreach P $post {
	try {
	    uplevel 1 [list {*}$P $r]
	} on break {e eo} {
	    # the post-process says skip the rest of the post-processes,
	    # it will handle its own response
	    return -level 2 -options $eo $r
	} on continue {} {
	    # the post-process has nothing to add
	} on error {e eo} {
	    # the pre-process failed - skip its contribution
	} on return {r} {
	    # the post-process says skip the rest of the post-processes but return its result
	    break
	} on ok {r} {
	    # the preprocess returned a response, consume it
	}
    }

    return $r
}

variable pre {rxLint}
variable post {}
proc process {R} {
    set socket [dict get $R -socket]
    set transaction [dict get $R -transaction]
    set tx [dict get $R -tx]
    variable pre
    corovar process
    variable post

    # Process the request+entity in a bespoke coroutine
    if {[llength [info commands process.$socket]]} {
	Debug.httpd {Starting process process.socket}
	process.$socket $R
    } else {
	# default - do some Pre Process and Post action
	coroutine process.$socket.$transaction ::apply [list {r socket pre process post tx} {
	    Debug.httpd {Starting process [info coroutine]}
	    set r [Pre $r $pre]		;# pre-process the request

	    # Process request: run the $process command
	    #	continue - process has nothing to add to pre-processing
	    #	break - the process will handle its own response transmission
	    #	error - the process has errored - make a ServerError response
	    #	ok - the process has returned its reply
	    set suspend 0
	    Debug.httpd {TRY process '$process'}
	    try {
		{*}$process $r
	    } on error {e eo} {
		# the process errored out - send an error message
		Debug.httpd {process has failed '$e' ($eo)}
		set r [ServerError $r $e $eo]
	    } on continue {} {
		# the process has nothing to say, post process
		Debug.httpd {process has nothing to say}
	    } on break {e eo} {
		# the process will handle its own response
		Debug.httpd {process will handle its own response}
		return -level 0 -options $eo $r
	    } on ok {result} {
		# the process returned a response, post-process then send it
		Debug.httpd {process returned ($result)}
		if {[dict size $result]} {
		    set r $result
		    if {[dict exists $result -suspend]} {
			set suspend 1
		    }
		}
	    }

	    if {$suspend} return	;# processing has suspended

	    set r [Post $r $post]	;# post-process the request
	    if {[dict exists $r -suspend]} return	;# post-process can also suspend

	    #puts stderr "DONE PROCESS [info coroutine] $socket"
	    tailcall $tx reply $r		;# finally, transmit the response and close up
	} [namespace current]] $R $socket $pre $process $post $tx
    }
}
