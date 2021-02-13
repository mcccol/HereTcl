# Hproc - request processing support
#
# This is a full-service request processor, with all the extras

variable methods {GET PUT POST HEAD OPTIONS}
variable maxurilen 0	;# maximum length of URI
Debug define H.process

proc rxLint {R} {
    Debug.H.process {rxLint $R}

    # ensure the HTTP version is acceptable
    if {[dict get $R -Header version] ni {1.1 1.0}} {
	# Send 505 for protocol != HTTP/1.0 or HTTP/1.1
	tailcall Bad $R "HTTP Version '[dict get $R -Header version]' not supported" 505
    }

    if {[dict get $R -Header type] eq "request"} {
	# ensure the HTTP method is acceptable if this is a request
	set method [string toupper [dict get $R -Header method]]

	# ensure the HTTP method is acceptable
	variable methods
	if {$method ni $methods} {
	    Debug.H.process {rxLint - unsupported method [info level] [info coroutine]}
	    #corovar socket; tailcall T::$socket reply [Bad $R "Method unsupported '$method'" 405]
	    tailcall Bad $R "Method unsupported '$method' (not one of $methods)" 405
	}

	# ensure the URI is plausible
	set uri [dict get $R -Header uri]
	variable maxurilen
	if {$maxurilen && [string length $uri] > $maxurilen} {
	    # send a 414 back
	    tailcall Bad $R "URI too long '$uri'" 414
	}
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
	Debug.H.process {TRY pre '$P'}
	try {
	    uplevel 1 [list {*}$P $r]
	} trap CONTINUE {e eo} {
	    Debug.H.process {pre '$P' - the pre-process has nothing to add}
	    # the pre-process has nothing to add
	} trap BREAK {r} {
	    Debug.H.process {pre '$P' - the pre-process says skip the rest of the pre-processes}
	    # the pre-process says skip the rest of the pre-processes,
	    # continue with the process
	    break
	} trap RETURN {r eo} {
	    # the pre-process has decided to usurp processing
	    Debug.H.process {pre '$P' - the pre-process has decided to usurp processing}
	    corovar socket
	    tailcall T::$socket reply $r
	} trap PASSTHRU {r eo} {
	    # the process will handle its own response
	    Debug.H.process {process will handle its own response}
	    return -options $eo $r
	} on error {e eo} {
	    Debug.H.process {pre the pre-process has failed in '$P' - '$e' ($eo)}
	    # the pre-process failed - skip it
	} on ok {r} {
	    # the preprocess returned a response, consume it
	    Debug.H.process {pre '$P' - the pre-process has returned a response ($r)}
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
	Debug.H.process {Post '$P'}
	try {
	    uplevel 1 [list {*}$P $r]
	} trap BREAK {e eo} {
	    # the post-process says skip the rest of the post-processes,
	    # it will handle its own response
	    return -level 2 -options $eo $r
	} trap CONTINUE {} {
	    # the post-process has nothing to add
	    Debug.H.process {Post '$P' has nothing to add}
	} trap RETURN {r} {
	    # the post-process says skip the rest of the post-processes but return its result
	    Debug.H.process {Post '$P' says skip the rest of Post processes ($r)}
	    break
	} trap PASSTHRU {r eo} {
	    # the process will handle its own response
	    Debug.H.process {process will handle its own response}
	    return -options $eo $r
	} on error {e eo} {
	    # the pre-process failed - skip its contribution
	    Debug.H.process {Post '$P' failed returned '$e' ($eo)}
	} on ok {r} {
	    # the preprocess returned a response, consume it
	    Debug.H.process {Post '$P' returned '$r'}
	}
    }

    return $r
}

variable pre {rxLint}
variable post {}

# process - take R request dict through pre-process, process, and post-process phases
proc process {R} {
    set socket [dict get $R -socket]
    set transaction [dict get $R -transaction]

    # Process the request+entity in a bespoke coroutine
    if {[llength [info commands process.$socket]]} {
	Debug.H.process {Starting process process.socket}
	process.$socket $R
    } else {
	# default - do some Pre Process and Post action
	variable pre
	corovar process
	variable post
	coroutine process.$socket.$transaction ::apply [list {r socket pre process post tx} {
	    Debug.H.process {Starting process [info coroutine]}
	    set r [Pre $r $pre]		;# pre-process the request

	    # Process request: run the $process command
	    #	CONTINUE - process has nothing to add to pre-processing
	    #	SUSPEND - the process will handle its own response transmission
	    #	PASSTHRU - the process will handle its network comms
	    #	error - the process has errored - make a ServerError response
	    #	ok - the process has returned its reply
	    try {
		Debug.H.process {TRY process '$process'}
		{*}$process $r
	    } trap CONTINUE {e eo} {
		# the process has nothing to say, post process
		Debug.H.process {process has nothing to say}
	    } trap PASSTHRU {e eo} {
		# the process will handle its own response
		Debug.H.process {process will handle its own response}
		return -options $eo $e
	    } trap SUSPEND {e eo} {
		# the process will handle its own response
		Debug.H.process {process will handle its own response}
		return
	    } on error {e eo} {
		# the process errored out - generate an error
		Debug.H.process {process has failed '$e' ($eo)}
		set r [ServerError $r $e $eo]
	    } on ok {result} {
		# the process returned a response, post-process then send it
		Debug.H.process {process returned ($result)}
		if {[dict size $result]} {
		    set r $result
		}
	    }

	    set r [Post $r $post]	;# post-process the request

	    Debug.H.process {DONE PROCESS [info coroutine] $socket}
	    return $r
	} [namespace current]] $R [dict get $R -socket] $pre $process $post [dict get $R -tx]
    }
}
