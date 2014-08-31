# Config.tcl - support tcl-like config files

if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path ~/Desktop/Work/Wub/ [file dirname [info script]]
}

if {[catch {package require Debug}]} {
    #proc Debug.config {args} {}
    proc Debug.config {args} {puts stderr HTTP@[uplevel 1 subst $args]}
} else {
    Debug define config 10
}

package require parsetcl
package provide Config 1.0

set ::API(Utilities/Config) {
    {
	Configuration parser
    }
}

oo::class create Config {
    # line - return the line number of an character position or interval
    # for error reporting
    method line {interval script} {
	lassign $interval start
	set line 1
	for {set i 0} {$i < $start} {incr i} {
	    if {[string index $script $i] eq "\n"} {
		incr line
	    }
	}
	return $line
    }

    # errors - return (optionally reset) parse errors
    method errors {{erase 0}} {
	variable errors
	if {![info exists errors]} {
	    return {}
	}
	set result $errors
	if {$erase} {
	    set errors {}
	}
	return $result
    }

    # parser_error - record a parser error
    method parser_error {script level text {location ""}} {
	if {$location ne ""} {
	    set location [my line $location $script]
	}
	set error [list $level $text $location]
	#puts stderr "CONF ERR: $error"
	Debug.config {$level: $error}
	variable errors; lappend errors $error
    }
    
    # parse a single section into raw alist, comments and metadata
    method parse_section {section script} {
	variable raw; variable comments; variable metadata
	variable clean 0

	# perform script transformation
	set varname ""	;# initial name used to collect per-section comments
	set body [parsetcl simple_parse_script $script]
	parsetcl walk_tree body index Rs {} C.* {
	    if {[llength $index] == 1} {
		# only process at script level
		set cmd [lindex $body {*}$index]

		set left [lindex $cmd 3]
		set varname [parsetcl unparse $left]	;# literal name
		if {![string match L* [lindex $left 0]]} {
		    my parser_error $script Error "Variable name '$varname' must be a literal ($left)" [lindex $cmd 1]
		} else {
		    # see if this variable duplicates an existing variable
		    if {[dict exists $raw $section $varname]} {
			my parser_error $script Warning "Variable '$section.$varname' is duplicated" [lindex $cmd 1]
		    }

		    set rest [lrange $cmd 4 end]
		    if {[llength $rest] == 1} {
			set rl [parsetcl unparse {*}$rest]
		    } else {
			set rl \"[parsetcl unparse [list {*}[lrange $cmd 0 2] {*}$rest]]\"
		    }

		    # set raw value
		    dict set raw $section $varname $rl
		    Debug.config {BCMD $index: ($left) ($rest) '$rl' - MD:($metadata)}
		}
	    } else {
		# we only transform the script top level
	    }
	} Nc {
	    # comment associates with immediately prior variable
	    dict set comments $section $varname [lindex $body {*}$index]
	} Ne {
	    set cmd [lindex $parse {*}$index]
	    my parser_error $script Error "Syntax Error ($cmd)" [lindex $cmd 1]
	}

	Debug.config {section: raw:($raw)}
    }

    # parse a file in config format
    method load_section {section file} {
	package require fileutil
	my parse_section $section [::fileutil::cat -- $file]
    }

    # merge raw, comments and metadata dicts for given section
    method merge_section {section args} {
	variable raw; variable comments; variable metadata
	variable clean 0
	lassign $args _raw _comments _metadata
	dict set raw $section [dict merge [dict get? $raw $section] $_raw]
	dict set comments $section [dict merge [dict get? $comments $section] $_comments]
	if {[string trim $_metadata] ne ""} {
	    dict append metadata $section " " $_metadata
	}
    }

    # parse a complete configuration into raw, comments and metadata
    method parse {script} {
	variable raw; variable comments; variable metadata

	set parse [parsetcl simple_parse_script $script]
	parsetcl walk_tree parse index Cd {
	    if {[llength $index] == 1} {
		set cmd [lindex $parse {*}$index]
		
		set left [lindex $cmd 3]
		set section [parsetcl unparse $left]
		if {![string match L* [lindex $left 0]]} {
		    my parser_error $script Error "Section name '$section' must be a literal ($left)" [lindex $cmd 1]
		} else {
		    if {[dict exists $raw $section]} {
			# this section duplicates an existing section
			my parser_error $script Warning "Section '$section' is duplicated" [lindex $cmd 1]
		    }

		    set rest [lrange $cmd 4 end]
		    set right [lindex $rest end]
		    if {[llength $rest] == 1} {
			# no meta
			set meta {}
		    } else {
			# some meta
			set meta [parsetcl unparse [list {*}[lrange $cmd 0 2] {*}[lrange $rest 0 end-1]]]
		    }
		    
		    # accumulate per-section metadata
		    dict set metadata $section $meta
		    #puts stderr "MD $section '$meta'"
		    
		    # parse raw body of section
		    my parse_section $section [lindex [parsetcl unparse $right] 0]
		}
	    } else {
		# we only transform the top level of script
	    }
	} C.* {
	    error "Don't know how to handle [lindex $parse {*}$index]"
	} Nc {
	    # comment
	    set cmd [lindex $parse {*}$index]
	    #puts stderr "Comment - $cmd"
	} Ne {
	    set cmd [lindex $parse {*}$index]
	    my parser_error $script Error "Syntax Error ($cmd)" [lindex $cmd 1]
	}

	Debug.config {parse: ($raw)}
    }

    # merge a raw, commants and metadata dicts
    method merge {args} {
	lassign $args _raw _comments _metadata
	foreach section [dict keys $_raw] {
	    my merge_section $section [dict get $_raw $section] [dict get? $_comments $section] [dict get? $_metadata $section]
	}
    }

    # parse a file in config format
    method load {file} {
	package require fileutil
	my errors 1
	my parse [::fileutil::cat -- $file]
	return [my errors]
    }

    # assign - assign config dict from args
    method assign {section var args} {
	variable raw
	dict set raw $section $var \"[join $args]\"
	variable clean 0
    }

    # assign? - assign configs from args
    method assign? {args} {
	variable raw;
	if {![dict exists $raw {*}[lrange $args 0 end-1]]} {
	    dict set raw {*}$args
	    Debug.config {assign? $args -> [dict get $raw {*}[lrange $args 0 end-1]]}
	}
	variable clean 0
    }

    # substitute section-relative names into value scripts
    method VarSub {script} {
	set NS [namespace current]
	Debug.config {VarSubbing: '$script'}

	# perform variable rewrite
	set body [parsetcl simple_parse_script $script]
	parsetcl walk_tree body index Sv {
	    set s [lindex $body {*}$index 3 2]
	    if {![string match ::* $s] && [string match *::* $s]} {
		# this is section-relative var - we need to make a fully qualified NS
		set s "${NS}::_C::$s"
		lset body {*}$index 3 2 $s
	    }
	    Debug.config {Varsub: $s}
	}
	set subbed [parsetcl unparse $body]
	set subbed [join [lrange [split $subbed \n] 1 end-1] \n]	;# why is this necessary?
	Debug.config {VarSubbed: '$script' -> '$subbed'}
	return $subbed
    }

    # eval_section - evaluate a section dict after variable substitution
    method eval_section {section} {
	variable raw
	set ss {}
	namespace eval _C::$section [string map [list %S% [self]] {
	    proc my {args} {
		uplevel 1 [list %S% {*}$args]
	    }
	}]
	Debug.config {evaling section '$section'}
	dict for {n v} [dict get $raw $section] {
	    set sv [my VarSub $v]
	    Debug.config {eval section '$section': '$n'->'$v' ($sv)}
	    #set _C::${section}::$n [namespace eval _C::$section return $sv]
	    namespace eval _C::$section variable $n $sv
	}

	Debug.config {evaling section metadata '$section'}
	variable metadata
	namespace eval _C::$section [list variable ""; set "" {}]
	if {[dict exists $metadata $section]} {
	    set sv [my VarSub [dict get $metadata $section]]
	    namespace eval _C::$section set \"\" \"$sv\"
	}
    }

    # evaluate a raw dict after variable substitution
    method eval {} {
	variable clean
	variable raw
	if {!$clean} {
	    # only re-evaluate if not clean
	    foreach section [dict keys $raw] {
		my eval_section $section
	    }
	    set clean 1
	    return 1
	} else {
	    return 0
	}
    }

    # sections - a list of sections
    method sections {{glob {}}} {
	variable raw; return [dict keys $raw {*}$glob]
    }

    # section - get evaluated section
    method section {section} {
	my eval	;# evaluate any changes in raw
	Debug.config {getting section '$section'}
	set result {}
	foreach var [info vars _C::${section}::*] {
	    try {
		set val [set $var]
		dict set result [namespace tail $var] $val
		Debug.config "got '$section.[namespace tail $var]' <- '$val'"
	    } on error {e eo} {
		Debug.error "Config [self]: can't read '[namespace tail $var]' while evaluating section '$section'"
	    }
	}
	return $result
    }

    # exists does the given secion and element exist?
    method exists {args} {
	variable raw
	return [dict exists $raw {*}$args]
    }

    # metadata - access metadata values
    method metadata {{section ""}} {
	if {$section eq ""} {
	    variable metadata
	    #puts stderr "metadata: $metadata"
	    return $metadata
	}
	# evaluate any changes in raw
	variable extracted
	if {[dict exists $extracted $section ""]} {
	    return [dict get $extracted $section ""]
	} else {
	    return {}
	}
    }

    # extract naming context from configuration and aggregated namespace
    method extract {{config ""}} {
	if {$config ne ""} {
	    # parse $config if proffered
	    my errors 1
	    my parse $config
	}

	# evaluate any changes in raw
	variable extracted
	if {![my eval]} {
	    if {![info exists extracted]} {
		set extracted {}
	    }
	    return $extracted
	}

	# extract the accumulated values from _C namespace children
	set extracted {}
	foreach section [my sections] {
	    dict set extracted $section [my section $section]
	}

	return $extracted
    }

    # get element from extracted dict
    method get {args} {
	my eval
	variable extracted
	set result [dict get $extracted {*}$args]
	if {[dict exists $result ""]} {
	    dict unset result ""	;# remove the metadata element (if any)
	}
	return $result
    }

    # bind - bind all values to their evaluated value
    method bind {} {
	variable raw [my extract]
    }

    # raw - access raw values
    method raw {{section ""}} {
	variable raw
	if {$section eq ""} {
	    return $raw
	} else {
	    return [dict get $raw $section]
	}
    }

    # comments - access comment values
    method comments {{section ""}} {
	variable comments
	if {$section eq ""} {
	    return $comments
	} else {
	    return [dict get $comments $section]
	}
    }

    # todict - extract elements from config as dict
    method todict {} {
	dict set result raw [my raw]
	dict set result comments [my comments]
	dict set result metadata [my metadata]
	return $result
    }

    # todict - extract elements from config as list
    method tolist {} {
	return [list [my raw] [my comments] [my metadata]]
    }

    # aggregate a list of Config objects
    method aggregate {args} {
	foreach a $args {
	    my merge {*}[$a tolist]
	}
    }

    # destroy context namespace
    method clear {} {
	variable raw {}	;# association between name and tcl script giving value
	variable comments {}	;# association between name and run-on comments
	variable metadata {}	;# association between name and metadata
	variable errors {}	;# parser error list

	# destroy evaluation namespace
	catch {namespace delete _C}
	namespace eval _C {}
	variable clean 1
    }

    method file {} {
	variable file; return $file
    }
    method configdir {} {
	variable file; return [file dirname $file]
    }

    constructor {args} {
	Debug.config {Creating Config [self] $args}
	if {[llength $args]%2} {
	    set cf [lindex $args end]
	    set args [lrange $args 0 end-1]
	    dict set args config $cf
	}
	variable {*}$args

	#catch {set args [dict merge [Site var? Config] $args]}	;# allow .ini file to modify defaults -- config is, itself, not Configurable

	my clear	;# start with a clean slate

	if {[info exists file]} {
	    set file [file normalize $file]
	    my load $file	;# parse any file passed in
	}

	if {[info exists config]} {
	    my parse $config	;# parse any literal config passed in
	}
	if {[info exists section]} {
	    my parse_section "" $section
	}
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    package require tcltest

    namespace eval ::Config::test {
	namespace import ::tcltest::*

	configure {*}$argv

	variable SETUP {
	    Config create config
	}
	variable CLEANUP {
	    config destroy
	}
	set sample_config {
	    section {
		var val	;# this is a variable
		var1 val1
		v1 2
		v2 [expr {$v1 ** 2}]
		string "hello world"
		list [list hello world]
		naked hello world
	    }
	    # another section
	    sect1 -auto $::auto_path {
		v1 [expr {$section::v1 ^ 10}]
		ap [list moop {*}$::auto_path]
		ap1 {junk}
	    }
	}
	
	test Config-test1 {} -setup $SETUP -body {
	    config extract $sample_config
	    config get section v2
	} -cleanup $CLEANUP -result 4

	test Config-test2 {} -setup $SETUP -body {
	    config extract $sample_config
	    config get sect1 ap
	} -cleanup $CLEANUP -result [list moop {*}$::auto_path]

	test Config-test3 {} -setup $SETUP -body {
	    config extract $sample_config
	    config get section string
	} -cleanup $CLEANUP -result "hello world"

	test Config-test4 {} -setup $SETUP -body {
	    config extract $sample_config
	    config get section list
	} -cleanup $CLEANUP -result "hello world"

	test Config-test4 {} -setup $SETUP -body {
	    config extract $sample_config
	    config get section naked
	} -cleanup $CLEANUP -result "hello world"

	test Config-test-api1 {} -setup $SETUP -body {
	    config assign Wub topdir "hello world"
	    config extract
	    config get Wub topdir
	} -cleanup $CLEANUP -result {hello world}

	test Config-metadata1 {} -setup $SETUP -body {
	    config extract $sample_config
	    config metadata sect1
	} -cleanup $CLEANUP -result "-auto $::auto_path"

	cleanupTests
    }
}
