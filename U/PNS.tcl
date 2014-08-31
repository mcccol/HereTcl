# PNS - persistent namespace
# code to map selected variables from a namespace to a database
#
# use the "" variable to store state of each PNS *in* each PNS
namespace eval PNS {
    variable max_prepcache 0	;# no limit to number of cached sql stmts

    # prep - prepare a stmt or reused an already cached stmt
    method prep {db stmt} {
	variable stmts	;# here are some statements we prepared earlier
	if {![info exists stmts($db)]} {
	    set stmts($db) {}
	}

	variable max_prepcache
	if {[dict exists $stmts($db) $stmt]} {
	    set s [dict get $stmts($db) $stmt]
	    if {$max_prepcache > 0} {
		# move matched element to end of cache (for LRU)
		dict unset stmts($db) $stmt
		dict set stmts($db) $stmt $s
	    }
	} else {
	    set s [$db prepare $stmt]
	    dict set stmts $stmt $s
	    if {$max_prepcache > 0 && [dict size $stmts] > $max_prepcache} {
		Debug.session {removing LRU cached statement}
		set stmts($db) [lrange $stmts 2 end]
	    }
	}

	return $s
    }

    method prep_purge {db} {
	variable stmts	;# here are some statements we prepared earlier
	set stmts($db) {}
    }

    # exec - execute a statement over db
    method exec {id stmt args} {
	namespace upvar $ns "" state; dict with state {}
	set incomplete 1
	while {$incomplete} {
	    # try to prep a statement - reconnect on connection down
	    set prepped ""
	    while {$prepped eq ""} {
		try {set prepped [my prep $db $stmt]} trap {TDBC REMOTE_DATABASE_ACCESS_ERROR} {e eo} {
		    if {[info exists reconnect] && [llength $reconnect]} {
			my prep_purge $db; {*}$reconnect $db $e $eo
		    } else {
			return -options $eo $e
		    }
		}
	    }

	    # try to execute the script around a prepped statement - reconnect on connection down
	    try {
		set result [uplevel 1 [list $prepped {*}$args]]
	    } trap {TDBC REMOTE_DATABASE_ACCESS_ERROR} {e eo} {
		if {[info exists reconnect] && [llength $reconnect]} {
		    my prep_purge $db; {*}$reconnect $db $e $eo
		} else {
		    return -options $eo $e
		}
	    } on error {e eo} {
		return -options $eo $e
	    } on ok {} {
		set incomplete 0
	    }
	}

	return $result
    }

    # fetch - fetch variables for NS $id
    method fetch {id} {
	namespace upvar $ns "" state; dict with state {}
	return [lindex [my exec "SELECT * FROM $table WHERE $key = :key" allrows -as dicts -- [list key $id]] 0]
    }

    # fields - set of fields in the related db/table
    proc fields {ns} {
	namespace upvar $ns "" state
	dict update state {
	    if {![info exists fields]} {
		set fields [dict keys [$db columns $table]]
	    }
	}
	return $fields
    }

    proc vars4ns {ns} {
	namespace upvar $ns "" state
	dict with state {}
	set flist [$db columns $table]	;# set of all columns

	# look at each of the vars in the NS, make a dict of those which are also fields
	set onlyNS {}	;# only in NS
	foreach var [info vars ${ns}::*] {
	    if {[dict exists $flist $var]} {
		dict set inNS $var [set $var]
		dict unset flist $var	;# remove from nullable
	    } else {
		lappend onlyNS $var	;# ignore these - they aren't persisted
	    }
	}

	# look for remaining records at each field in rec
	set rec [fetch $id]		;# record of all values
	set inDB {}
	foreach var [dict keys $f] {
	    if {[dict exists $rec $var]} {
		dict set $inDB $var [dict get $rec $var]
		dict unset flist $var
	    }
	}

	set diff {}
	dict for {n v} $inNS {
	    if {[dict exists $rec $n] && [dict get $rec $n] ne $v} {
		dict set diff $n $v	;# record the new value
	    }
	}

	# flist - vars existing only in field list, no value in DB or NS
	# inDB - vars existing only in DB
	# inNS - vars existing in NS
	# diff - those values which differ between NS and DB
    }

    proc persist {ns args} {
	namespace upvar $ns "" state

	if {[info exists state]} {
	    set state [dict merge $state $args]
	} else {
	    set state $args
	}
	set state [dict merge {lazy 1} $state]	;# defaults

	dict with state {}
	# db - a tdbc db command for persistent store
	# table - tdbc table containing the store
	# key - db key field - 1:1 with persistent NS
	# id - value for key which identifies namespace
	# lazy - is the namespace to be merged lazily?
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
