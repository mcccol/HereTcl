# Convert -- make-like content transformation
#
# intercepts responses and transforms content according to mime-type
#
# Convert translates content by invoking translator procs
# (which are commands of the form .mime/type.mime/type)
#

package require OO
package require Debug
Debug define convert

package provide HConvert 1.0


if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path [pwd]
}

set ::API(Server/Convert) {
    {
	Component for performing content negotiation.

	Convert attempts to convert content from the response content-type to something permitted by the accept request field by intercepting responses from -subdomains.

	To do this, Convert maintains a table of mappings from one mime-type to another, finds a minimal path between the content-type to some acceptable type, and calls each conversion function in turn.
    }
}

class create Convert {
    # Transform - add a single transform to the transformation graph
    method transform {from to args} {
	set prefix ${from},$to
	variable transform; set transform($prefix) $args

	# add (only) new outbound transition to graph
	variable graph;
	set out {}
	foreach t [dict get? $graph $from] {
	    dict set out $t {}
	}
	if {![dict exists $out $to]} {
	    dict lappend graph $from $to
	}
    }

    # Postprocess - add a postprocessor to the set of postprocessors
    method postprocess {from args} {
	variable postprocess; set postprocess($from) $args
    }

    # transforms - the transforms
    method transforms {} {
	variable transform; return [array get transform]
    }

    # graph - the graph structure
    method graph {} {
	variable graph; return $graph
    }

    # object - add all transformers and postprocessors from the object
    method object {object} {
	# scan the object looking for translators
	# which are methods of the form .mime/type.mime/type

	# construct a dict from method name to the formal parameters of the method
	set defclass [info object class $object]
	set methods [lreverse [lsort -dictionary [info class methods $defclass -private -all]]]

	set convertors {}
	foreach m $methods {
	    if {[string match */*.*/* $m]} {
		lassign [split $m .] from to
		lappend convertors "$from->($m)->$to"
		my transform $from $to $object $m
	    }
	}

	# scan the object looking for postprocessors
	# which are methods of the form .mime/type
	foreach m $methods {
	    if {[string match /* $m]} continue
	    if {[string match */* $m]} {
		if {[string match */*.*/* $m]} continue	;# it's a transformer
		lappend convertors "$m->($m)->$m"
		my postprocess $m $object $m
	    }
	}

	Debug.convert {[self] Object Convertors from $defclass: $convertors over $object}
    }

    # namespace - add all transformers and postprocessors from the namespace
    method namespace {ns} {
	# scan the namespace looking for translators
	# which are commands of the form .mime/type.mime/type
	set candidates [info commands ${ns}::.*/*.*/*]
	Debug.convert {[self] Namespace '$ns' transformers - $candidates}
	foreach candidate $candidates {
	    lassign [split $candidate .] -> from to
	    my transform $from $to namespace eval $ns $candidate
	}

	# scan the namespace looking for postprocessors
	# which are commands of the form .mime/type
	set postproc {}
	set candidates [info commands ${ns}::.*/*]
	Debug.convert {[self] Namespace '$ns' postprocessors - $candidates}
	foreach candidate $candidates {
	    if {[string match .*/*.*/* $candidate]} continue
	    if {[lassign [split $candidate .] x from] eq ""} {
		my postprocess $from namespace eval $ns $candidate
	    }
	}
    }

    # perform applicable postprocesses on content of given type
    method postprocessor {rsp} {
	set ctype [dict get $rsp content-type]
	variable postprocess
	while {[info exists postprocess($ctype)]
	       && $ctype ni [dict get? $rsp -transforms]
	   } {
	    Debug.convert {[self] postprocess type: $ctype '$postprocess($ctype)' ([dict merge $rsp {-content ...elided...}])}
	    # there is a postprocessor

	    #set rsp [Http loadContent $rsp] ;# read -fd content if any

	    Debug.convert {[self] postprocessing: '$postprocess($ctype)'}
	    dict lappend rsp -transforms $ctype ;# record the transforms
	    if {[lindex $postprocess($ctype) 0] eq "namespace"} {
		set rsp [list $rsp]	;# namespace eval needs an extra level of quoting
	    }
	    set rsp [{*}$postprocess($ctype) $rsp]
	    catch {dict unset rsp -file}	;# forget that there's a file connected

	    Debug.convert {[self] postprocessed $ctype: '$postprocess($ctype)' -> [dict get? $rsp content-type] ([dict size $rsp])}
	    set ctype [dict get $rsp content-type]	;# keep going with new types
	}

	Debug.convert {[expr {![info exists postprocess($ctype)]?"[self] there is no $ctype postprocessor": ($ctype in [dict get? $rsp -transforms])?"[self] has already run $ctype postprocessor":""}]}

	return $rsp
    }

    # Path
    # from sgraph - simple graph code, sufficient to give minimal acyclic paths between nodes
    # Thanks Richard Suchenwi for: http://wiki.tcl.tk/2473 and http://wiki.tcl.tk/2603
    method Path {g from to} {
	if {$from eq $to} {
	    return {}
	} elseif {[string match $to $from]} {
	    return {}
	}

	set length 999999	;# simulated infinity
	set todo $from		;# list of things to try
	while {[llength $todo]} {
	    set todo [lassign $todo try]	;# first thing to do
	    set last [lindex $try end]
	    #puts stderr "sgraph path: ($try) ($last)"
	    foreach node [dict get? $g $last] {
		if {($node eq $to)
		    || [string match $to $node]
		} {
		    if {[llength $try] < $length} {
			set length [llength $try]
		    }
		    lappend try $node
		    return $try    ;# found a path
		} elseif {[lsearch $try $node] >= 0} {
		    continue ;# detected a cycle
		} elseif {[llength $try] < $length} {
		    lappend todo [concat $try [list $node]]
		} else {
		} ;# lappend and [lassign] make a FIFO queue
	    }
	}

	return {}
    }

    # path - return a path through the transformation graph
    # between source 'from' and sink 'to'
    method path {from to} {
	Debug.convert {[self] PATH: trying to find a path from '$from' to '$to'}
	variable graph
	variable paths
	if {[info exists paths($from,$to)]} {
	    Debug.convert {[self] CACHED path '$from' -> '$to' is $paths($from,$to)}
	    #Debug.convert {[self] all known paths [array names paths]}
	    return $paths($from,$to)	;# cached match
	}

	Debug.convert {[self] PATH? $from -> $to within ($graph)}

	# try a glob match
	lassign [split $from /] fmajor
	lassign [split $to /] tmajor

	# generate transformer name pairs
	set poss [list 1.0 $from,$to]
	lappend poss 0.5 [lindex [array names paths -glob $fmajor/*,$to] 0]
	lappend poss 0.5 [lindex [array names paths -glob $from,$tmajor/*] 0]
	lappend poss 0.25 [lindex [array names paths -glob $fmajor/*,$tmajor/*] 0]
	Debug.convert {[self] POSSIBLE $from -> $to $poss}

	foreach {accept possible} $poss {
	    if {$possible eq ""} continue
	    lassign [split $possible ,] from to
	    Debug.convert {[self] ESSAY accept:$accept possible:$possible from:'$from' to:'$to'}
	    set path [my Path $graph $from $to]
	    if {[llength $path]} {
		Debug.convert {[self] SUCCESS path $from -> $to via $path}
		break
	    } else {
		Debug.convert {[self] FAIL essayed path $from -> $to}
	    }
	}

	if {[llength $path]} {
	    # turn path into pairwise transformer list
	    set pairs {}
	    foreach el $path {
		if {[info exists prev]} {
		    lappend pairs "$prev,$el"
		}
		set prev $el
	    }

	    Debug.convert {[self] path FOUND $from -> $to via '$pairs'}
	    set paths($from,$to) [list $pairs $accept]	;# cache result
	    return [list $pairs $accept]
	} else {
	    Debug.convert {[self] NO PATH FOUND $from -> $to}
	    return {}
	}
    }

    # tpath - find a match for an acceptable type,
    # or calculate a path through the transformation graph
    # which will generate an acceptable type
    method tpath {rq} {
	set ctype [string tolower [dict get $rq -reply content-type]]	;# what we have
	set accept [string tolower [dict get $rq accept]]	;# what we want

	# check transformation cache first
	variable tcache
	if {[info exists tcache(${ctype}@$accept)]} {
	    # found a cached transformation path - use it
	    return $tcache(${ctype}@$accept)
	}

	set path ""	;# how to get from ctype to one acceptable

	# search the acceptable for a match to what we have
	# otherwise, sort the acceptable by q parameter
	set order 10000; # dummy quality measure to preserve order
	foreach a [split [dict get $rq accept] ,] {
	    # client will accept type $a with quality $q
	    lassign [split [string trim $a] ";"] a q

	    Debug.convert {[self] tpath matching accept:'$a' quality:'$q' against '$ctype'}
	    if {[string match $a $ctype]} {
		# exact match - we have a direct conversion
		Debug.convert {[self] tpath: DIRECT conversion to '$a'}
		set tcache(${ctype}@$accept) *
		return *
	    }

	    if {$q eq ""} {
		# give unqualified accepts a lower order
		set q [incr order -1].0 ;# ordered quality metric
	    } else {
		# determine the accept order
		set q [lindex [split $q =] 1]
	    }

	    # fix bogus quality values
	    if {![string is double -strict $q]} {
		if {[string is integer -strict $q]} {
		    set q "${q}.0"
		} else {
		    set q 1.0
		}
	    }

	    # while not an exact match, $a would be acceptable
	    lappend acceptable $a $q
	}

	Debug.convert {[self] tpath: searching indirect among acceptable ($acceptable)}

	# there is no direct match for any accepted types
	# process acceptable types in decreasing order of acceptability
	# find all possible conversion paths (including wildcards)
	set found {}
	foreach {a q} $acceptable {
	    Debug.convert {[self] trying '$ctype->$a' quality $q}
	    lassign [my path $ctype $a] path quality ;# find a conversion path
	    if {$path ne {}} {
		# we found a path to the most acceptable available type
		Debug.convert {[self] possible path $path.  Quality $quality * $q}
		dict lappend found [expr {$quality * $q}] $path
	    } else {
		Debug.convert {[self] impossible path $ctype->$a}
	    }
	}

	Debug.convert {[self] possible successful paths: ($found)}

	# find the most acceptable path by quality
	if {[dict size $found]} {
	    set path [dict get $found [lindex [lsort -real -decreasing [dict keys $found]] 0]]
	    set path [lindex $path 0]
	    set tcache(${ctype}@$accept) $path
	}

	return $path
    }

    # transformer - Apply transformations to content yielding a desired content-type
    method transformer {rq} {
	set path [my tpath $rq]	;# find a transformation path

	if {$path eq "*"} {
	    Debug.convert {[self] transform identity}
	    return -errorcode IDENTITY $rq
	} elseif {![llength $path]} {
	    # no transformations possible
	    Debug.convert {[self] no transformations possible}
	    return -errorcode NONE $rq
	}

	# a transforming path exists
	Debug.convert {[self] TRANSFORMING: url:'[dict get? $rq -Url]' along path ($path)}

	# perform those transformations on the path
	variable transform
	foreach el $path {
	    lassign [split $el ,] oldtype expected ;# characterises the transformation

	    Debug.convert {[self] '$transform($el)' transform element: '$el' expect '$oldtype->$expected'}
	    dict lappend rsp -transforms $el	;# record the transforms

	    if {[lindex $transform($el) 0] eq "namespace"} {
		set rq [list $rq]	;# namespace eval needs an extra level of quoting
	    }

	    try {
		set rq [{*}$transform($el) $rq]	;# transform content
	    } trap SUSPEND {} {
		# the transformer wants to suspend
		Debug.convert {[self] transformer suspended}
		return -errorcode SUSPEND $rq
	    } on ok {} {
		# transformation succeeded

		# ensure the transformation is still on-path
		set ctype [dict get $rq -reply content-type]
		if {[string match $expected $ctype]} {
		    Debug.convert {[self] '$transform($el)' transformer SUCCESS: '$oldtype->$ctype'}
		} elseif {$oldtype eq $ctype} {
		    # a transformer hasn't set a new mime type.
		    dict set rsp -reply content-type $expected
		    Debug.error {Convert [self] '$transform($el)' transformer FAILED to set content type to $expected, instead it set it to $ctype - FIX please}
		} else {
		    # restart the transformation with new content type
		    Debug.convert {[self] '$transform($el)' transformer CHANGED target from '$oldtype' -> '$ctype' - we were expecting '$expected' while accepting:'[dict get? $rq accept]'}
		    return -errorcode CHANGED $rq
		}
	    }
	}

	Debug.convert {[self] transform accepting final: '[dict get? $rq accept]'}
	return -errorcode COMPLETE $rq
    }

    # convert - perform all content negotiation on a response
    method convert {rq {to ""}} {
	Debug.convert {Convert $rq to: $to}
	if {![dict exists $rq -reply -content]} {
	    Debug.convert {[self] request has no content, return}
	    return $rq
	}
	if {![dict exists $rq -reply content-type]} {
	    error "response has no content-type [dumpMsg $rsp]"
	}

	# raw responses get no conversion
	if {[dict exists $rq -reply -raw]} {
	    if {[dict get $rq -reply -raw]} {
		Debug.convert {[self] request is -raw, return}
		return $rq	;# this is raw - no conversion
	    }
	} else {
	    dict set rq -reply -raw 0
	}

	# avoid identity conversions
	set to [string tolower $to]
	if {$to ne ""} {
	    if {[dict get? $rq -reply content-type] eq $to} {
		Debug.convert {[self] Identity Conversion}
		return $rq	;# don't need to process
	    }
	    dict set rq accept $to
	}

	# condition 'accept' tag
	if {![dict exists $rq accept]} {
	    # by default, accept text/html
	    dict set rq accept "text/html"
	} else {
	    # don't allow */* as an 'accept' - they never mean it
	    dict set rq accept [string map [list */* text/html,text/plain] [dict get $rq accept]]
	}

	# condition 'content-type'
	dict set rq -reply content-type [string tolower [dict get $rq -reply content-type]]

	# perform any preprocessing on input type
	variable postprocess
	set preprocessed ""
	set ctype [dict get $rq -reply content-type]
	if {[info exists postprocess($ctype)]} {
	    Debug.convert {[self] Preprocess of '$ctype'}
	    set rq [my postprocessor $rq]
	    # remember what we've preprocessed
	    set preprocessed [dict get $rq -reply content-type]
	    if {$ctype ne $preprocessed} {
		Debug.convert {[self] preprocessor changed type from '$ctype' to '$preprocessed'}
	    }
	}

	# perform each transformation on the path
	# any step may set -raw to avoid further conversion
	while {![dict get $rq -reply -raw]} {
	    # transform according to mime type
	    # determine the current best path from the current content-type
	    # and one of the acceptable types.
	    set oldct [dict get $rq -reply content-type]
	    Debug.convert {[self] transforming from '$oldct' to one of these acceptable:'[dict get? $rq accept]'}

	    try {
		set rq [my transformer $rq]
	    } trap SUSPEND {} {
		# the transformer wants to suspend
		Debug.convert {[self] transformer suspended}
		return -options $eo $r
	    } trap CHANGED {} {
		# the transformation path took us in an unexpected direction - keep going
		Debug.convert {[self] conversion CHANGED to '[dict get? $rq -reply content-type]'}
	    } on ok {} {
		# the transformation completed normally with COMPLETE, NONE or IDENTITY
		# the content is now in an acceptable form or no further transformation is possible

		# perform any postprocessing on *transformed* type
		# avoid doing this twice - if we've already preprocessed
		set ctype [dict get $rq -reply content-type]
		Debug.convert {[self] conversion to '$ctype'}
		if {$preprocessed ne $ctype && [info exists postprocess($ctype)]} {
		    set rq [my postprocessor $rq]
		}
		break
	    } on error {e eo} {
		# a transformation error has occurred
		# generate ServerError, proceed with *it* as content
		Debug.convert {[self] conversion ERROR: '$e' ($eo)}

		Debug.convert {[self] transformer ($transform($el)) Error $e ($eo)}
		set rq [H ServerError $rq $e $eo]
	    } finally {
		if {![dict exists $rq -reply -raw]} {
		    dict set rq -reply -raw 0	;# we're going to repeat this, so ensure we have a -reply raw setting
		}
	    }

	    Debug.convert {[self] transformed from $oldct to '[dict get $rq -reply content-type]'}
	    Debug.convert {[self] accepting1: '[dict get? $rq accept]'}
	}

	Debug.convert {[self] conversion complete: }
	Debug.convert {[self] accepting final: '[dict get? $rq accept]'}
	dict set rq -reply content-length [string length [dict get -reply -content]]
	return $rq
    }

    # convert! - perform a specified transformation on a response
    method convert! {rq to {mime ""} {content ""}} {
	if {$mime ne ""} {
	    dict set rq -reply content-type $mime
	}
	if {$content ne ""} {
	    dict set rq -reply -content $content
	}

	if {[dict exists $rq accept]} {
	    set oldaccept [dict get $rq accept]
	}
	set rq [my convert $rq [string tolower $to]]
	if {[info exists oldaccept]} {
	    dict set rq accept $oldaccept
	}

	return $rq
    }

    # do - perform content negotiation and transformation
    constructor {args} {
	variable conversions 1	;# include some default conversions
	variable namespace

	variable paths; array set paths {}
	variable graph {}	;# transformation graph - all known transforms
	variable transform	;# set of mappings from,to mime-types
	array set transform {}

	# set of mappings mime-type to postprocess script
	variable postprocess
	array set postprocess {}

	variable tcache	;# cache of known transformations

	# allow .ini file to modify defaults
	#variable {*}[Site var? Convert]
	variable {*}$args

	variable conversions
	if {$conversions} {
	    package require conversions
	    Debug.convert {[self] adding in default conversions}
	    my namespace ::conversions
	}

	foreach {n ns} $args {
	    if {$n eq "namespace"} {
		my namespace ::$ns
	    }
	}

	my object [self]	;# add in any conversions by child instances
	next? {*}$args
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    error "this is out of date"
    lappend auto_path [pwd] [pwd]/stx/

    package require Stdin
    package require Listener
    package require Httpd
    package require Host
    package require File
    package require Dump
    package require Compose

    namespace eval ::xconversions {
	proc .x-text/html-fragment.text/html {rsp} {
	    set content "<html> \n"
	    append content "<header><title>Wrapped</title></header>" \n
	    append content <body> \n
	    append content [dict get $rsp -content]
	    append content </body> \n
	    append content </html> \n
	    return [dict replace $rsp \
			-content $content \
			-raw 1 \
			content-type text/html]
	}
    }

    Host localhost -name ""
    localhost Register /dump/ DumpDispatch	;# dump dispatcher

    set fdom [File %AUTO% root [file dirname [info script]]]
    localhost Register /file/ $fdom Dispatch	;# filedomain dispatcher

    set collect [Compose %AUTO% -subdomains [list $sdom $fdom]]
    set convert [Convert %AUTO% -subdomain $collect]

    #$convert namespace ::conversions

    localhost Register / $convert Dispatch

    # start Listener
    set listener [Listener %AUTO% -port 8080]
    puts stderr "Listener $listener"

    set forever 0
    vwait forever
}
