# Session - handle Session vars stored in a db, indexed by a cookie.
#
# This is to be run in the main thread, and to amend the -session var in
# a request, to be passed along with request to Workers.
#
# ARCHITECTURE:
#
# Cookies shouldn't be sent with a path of /, because it screws up caching of static content.
#
# Session maintains a key in cookie $cookie which is sent within a constrained path.
# (a) how can a cookie within a constrained path be used
# everywhere on a site when it's only sent for a subpath of the site - use a
# web-bug technique - load some file from session domain on every page, don't cache
# that subdomain.  Use Wub to associate the cookie with the whole session.
# (b) how can a cookie within a sub-path impact on the whole page as seen by a
# client?  Javascript.  Send javascript from the web bug.
#
# IMPLEMENTATION:
# Sessions are records in a tdbc table which are mirrored in the corovar scope (uplevel #1)
# the cookie maps to a defined coroutine which is maintained by Session, and in which all
# request processing occurs.

# If the session variables change during processing of the request, changes will
# be rewritten to the database.
if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend ::auto_path [file dirname [file dirname [file normalize [info script]]]]
}

package require Debug
Debug define session 10
package require OO
package require md5
package require Direct
package require Cookies

package provide Session 4.0
set ::API(Session) {
    {
	Session manager
    }

    lazy {to number of seconds to flush *only* periodically (default: 0 - flush every time.)}
    establish {auto-establish a persistent record for each new session? (default: yes)}

    tdbc {Which TDBC backend type should be used (default: sqlite3)}
    db {an already-connected tdbc db (default none - use $schema to create $table in $file)}
    file {a db file to be created (default none)}
    tdbc_opts {::tdbc::connection creation options (default "")}
    schemafile {a file containing a schema for table creation}
    schema {schema to create the session table, if no $db is specified}
    table {name of the session table for storing session vars (default: $cookie)}
    key {name of the field for session key var / session id (default: $cookie)}

    cookie {session cookie name (default: "session")}
    cpath {session cookie path - this (default "/" is a bad idea.)}
    expires {how long does this cookie live? (default "next month")}
    cookie_args {extra args for cookie creation}
}

class create SessionClass {
    method sessionvar {} {
	# the name of the session variable
	variable key; return $key
    }

    method id {} {
	variable key; upvar #1 $key id; return $id
    }

    # Session variable - map session variables into local scope and assign values
    method variable {args} {
	Debug.session {Session variable $args}

	set sns [namespace current]::Sessions::[my id]	;# remember session namespace
	if {[llength $args] == 1} {
	    set n [lindex $args 0]
	    namespace eval $sns [list variable $n]
	    uplevel 1 [list namespace upvar ${sns}::$n $n]
	} else {
	    foreach {n v} $args {
		namespace eval $sns [list variable $n $v]
		uplevel 1 [list namespace upvar ${sns}::$n $n]
	    }
	}
    }

    # variables - map a session variable into local scope
    method variables {args} {
	Debug.session {Session variables $args}
	set sns [namespace current]::Sessions::[my id]	;# remember session namespace
	foreach n $args {
	    namespace eval $sns [list variable $n]
	    uplevel 1 [list namespace upvar ${sns}::$n $n]
	}
    }

    # idle - return list of sessions idle longer than the proffered time
    method idle {args} {
	if {![llength $args]} {
	    set args {1 day}
	}

	variable active;
	set now [clock seconds]
	set idle {}
	foreach {session when} [lsort -integer -stride 2 -index 1 [array get active]] {
	    if {[clock add $when {*}$args] > $now} {
		lappend idle $session
	    }
	}
	return $idle
    }

    # sns_dead - the session namespace has died, clean up after it.
    method sns_dead {sns args} {
	variable varmod; catch {unset varmod($sns)}

	set id [namespace tail $sns]
	variable variables; catch {unset variables($id)}
	variable active; catch {unset active($id)}
	variable terminate; catch {unset terminate($id)}
	variable established; catch {unset established($id)}

	Debug.session {sns_dead session $id}
    }

    # open - create or fetch a Session Namespace for the session cookie $cookie
    method open {r} {
	# fetch or create a cookie session identifier
	variable cookie
	Debug.session {session cookie $cookie: [Cookies Fetch? $r -name $cookie] / ([dict get $r -cookies])}
	set id [Cookies Fetch? $r -name $cookie]

	if {$id eq ""} {
	    # There is no session cookie - create a new session, id, and cookie
	    Debug.session {create new session}

	    # create new session id
	    variable uniq; set id [::md5::md5 -hex [self][incr uniq][clock microseconds]]

	    # create the cookie
	    variable cpath; variable expires; variable cookie_args;
	    set r [Cookies Add $r -path $cpath -expires $expires {*}$cookie_args -name $cookie -value $id]
	    set established($id) 0
	    Debug.session {new session: $id - cookies [dict get $r -cookies]}
	} else {
	    # We have been given the session cookie
	    set id [dict get [Cookies Fetch $r -name $cookie] -value]
	    Debug.session {session cookie: $id}
	}

	# find active session with $id
	set sns [namespace current]::Sessions::$id	;# remember session namespace
	if {![namespace exists $sns]} {
	    namespace eval $sns {
		namespace export -clear *
		namespace ensemble create -subcommands {}
	    }	;# create sns
	    trace add command $sns delete [list [self] sns_dead]	;# watch the SNS's lifetime
	} else {
	    # the sns exists
	    variable established
	    Debug.session {existing SNS.  established? $established($id)}
	}

	variable key; upvar #1 $key $key; set $key $id
	dict set r -session $id
	return $r
    }

    constructor {args} {
	variable key ""			;# field for session key var
	variable cookie "session"	;# session cookie name
	variable cpath "/"		;# session cookie path - this default is a bad idea.
	variable expires "next month"	;# how long does this cookie live?
	variable cookie_args {}		;# extra args for cookie creation
    }
}

class create PersistSessionClass {
    # established - the established state of sessions
    method Established {} {
	variable established; array get established
    }

    # close - a session within this Session
    method close {session} {
	# close the named session
	variable terminate; set terminate($session) 1
    }

    # close_idle - close sessions which have been idle for more than a given time
    method close_idle {args} {
	foreach session [my idle {*}$args] {
	    my close $session
	}
    }

    # varmod - record and return all session variable modifications
    method varmod {args} {
	variable varmod
	if {[catch {
	    lassign $args id name1 name2 op
	    set sns [namespace current]::Sessions::$id	;# remember session namespace
	    Debug.session {varmod [string toupper $op]: $id/$sns $args}
	    # puts stderr "VARMOD $id [info frame -1]/[info frame -2]/[info frame -3]"

	    variable key
	    if {$name1 eq $key} {
		# the user has tried to modify the session variable
		# reset it to what it should be, and error.
		set ${sns}::$key $id
		if {$op eq "unset"} {
		    # have to re-establish the trace
		    ::trace add variable $name1 {write unset} [list [self] varmod $id]
		    # we can't error out of an unset ... oh well
		}
		error "if you modify the session variable, you're not gonna have a good time."
	    }

	    variable lazy
	    switch -- $op {
		write {
		    if {$lazy} {
			# store the values for later writing to db
			dict set varmod($id) write $name1 [set ${sns}::$name1]
		    } else {
			dict set varmod($id) write $name1 1
		    }
		    catch {dict unset varmod($id) unset $name1}
		}
		unset {
		    dict set varmod($id) unset $name1 1
		    catch {dict unset varmod($id) write $name1}
		}
	    }
	} e eo]} {
	    Debug.error {Session [self] varmod [info coroutine] $id $args ERROR '$e' ($eo)}
	}
    }

    # sns_dead - the session namespace has died, clean up after it.
    method sns_dead {sns args} {
	variable varmod; catch {unset varmod($sns)}

	set id [namespace tail $sns]
	variable variables; catch {unset variables($id)}
	variable active; catch {unset active($id)}
	variable terminate; catch {unset terminate($id)}
	variable established; catch {unset established($id)}

	Debug.session {sns_dead session $id}
    }

    # prep - prepare a stmt or reused an already cached stmt
    method prep {stmt} {
	variable stmts	;# here are some statements we prepared earlier
	if {![info exists stmts]} {
	    set stmts {}
	}
	variable max_prepcache
	if {[dict exists $stmts $stmt]} {
	    set s [dict get $stmts $stmt]
	    if {$max_prepcache > 0} {
		# move matched element to end of cache (for LRU)
		dict unset stmts $stmt
		dict set stmts $stmt $s
	    }
	} else {
	    set s [my db prepare $stmt]
	    dict set stmts $stmt $s
	    if {$max_prepcache > 0 && [dict size $stmts] > $max_prepcache} {
		Debug.session {removing LRU cached statement}
		set stmts [lrange $stmts 2 end]
	    }
	}
	return $s
    }

    method prep_purge {} {
	variable stmts	;# here are some statements we prepared earlier
	set stmts {}
    }

    # exec - execute a statement over db
    method exec {stmt args} {
	set incomplete 1
	while {$incomplete} {
	    # try to prep a statement - reconnect on connection down
	    set prepped ""
	    while {$prepped eq ""} {
		try {set prepped [my prep $stmt]} trap {TDBC REMOTE_DATABASE_ACCESS_ERROR} {e eo} {
		    variable reconnect
		    if {[llength $reconnect]} {
			my prep_purge
			variable db; {*}$reconnect $db $e $eo
		    } else {
			return -options $eo $e
		    }
		}
	    }

	    # try to execute the script around a prepped statement - reconnect on connection down
	    try {
		set result [uplevel 1 [list $prepped {*}$args]]
	    } trap {TDBC REMOTE_DATABASE_ACCESS_ERROR} {e eo} {
		variable reconnect
		if {[llength $reconnect]} {
		    my prep_purge
		    variable db; {*}$reconnect $db $e $eo
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

    # flush - write back session variable changes
    method flush {id args} {
	variable established
	if {!$established($id)} {
	    Debug.session {flush $id not established, not flushing}
	    return	;# not a persistent session
	}

	set write {}; set unset {}
	dict with args {}
	if {![llength $write] && ![llength $unset]} {
	    Debug.session {flush $id nothing to flush}
	    return
	}

	set sns [namespace current]::Sessions::$id	;# session namespace

	variable key
	foreach field [dict keys $unset] {
	    if {$field eq $key} continue	;# may not unset $key field
	    lappend vars $field=NULL
	}

	variable lazy
	foreach {field value} $write {
	    if {$field eq $key} continue	;# may not modify $key field
	    incr i
	    lappend vars $field=:V$i
	    if {$lazy} {
		dict set values V$i $value
	    } else {
		dict set values V$i [set ${sns}::$field]
	    }
	}

	# prepared db command nulls field
	variable table
	variable key
	dict set values key $id
	Debug.session {flush 'UPDATE $table SET [join $vars ,] WHERE $key = $id' over ($values)}
	set result [my exec "UPDATE $table SET [join $vars ,] WHERE $key = :key" allrows -- $values]

	Debug.session {flushed $result}
    }

    # flush_lazy - flush all pending mods to the db
    method flush_lazy {} {
	variable lazy
	if {!$lazy} {
	    error "Can't flush lazy unless the Session is lazy which [self] is not."
	}

	variable varmod
	foreach id [array names varmod] {
	    my flush [my id] {*}$varmod($id)
	    unset varmod($id)
	}

	after [expr {$lazy * 1000}] [list [self] flush_lazy]
    }

    # fetch - fetch variables for session $id
    method fetch {id} {
	variable table
	variable key
	set result [lindex [my exec "SELECT * FROM $table WHERE $key = :key" allrows -as dicts -- [list key $id]] 0]
	Debug.session {fetch ($result)}
	return $result
    }

    # check - does session $id have any persistent records?
    method check {id} {
	# check the state of this session
	variable table; variable key
	set check [lindex [my exec "SELECT count(*) FROM $table WHERE $key = :key" allrows -- [list key $id]] 0]
	Debug.session {CHECK $check}
	return [lindex $check 1]
    }

    # establish - create a minimal record for session
    method establish {{id ""}} {
	if {$id eq ""} {
	    set id [my id]
	}
	Debug.session {establishing $id}

	variable established
	if {$established($id)} {
	    Debug.session {establish $id - already established}
	    return	;# already established
	}

	# a session is established merely by having an entry with the $key value as $id
	variable table; variable key
	set result [my exec "INSERT INTO $table ($key) VALUES (:key)" allrows -- [list key $id]]
	set established($id) 1

	Debug.session {established 'INSERT INTO $table ($key) VALUES ($id)' -> $result}
    }

    # name of all known session $id variables
    method fields {id} {
	variable fields		;# names of all known session variables
	if {![info exists fields]} {
	    variable table; set fields [dict keys [my db columns $table]]
	}
	return $fields
    }

    # open - create or fetch a Session Namespace for the session cookie $cookie
    method open {r} {
	set r [next $r]; set id [dict get $r -session]
	variable established

	# find active session with $id
	# check the state of the session
	variable key
	set stored [my fetch $id]
	switch -- [my check $id],[dict exists $stored $key] {
	    0,0 {
		# no record for this session
		variable establish; set established($id) 0
		if {$establish} {
		    # we want all sessions to be persistent
		    Debug.session {No data for $id - make some}
		    my establish $id
		    Debug.session {CHECK [my check $id]}
		} else {
		    # we let sessions be persisted explicitly
		    Debug.session {No data for $id - no establishment}
		}
	    }

	    1,1 {
		# the session is persistent *and* has data
		Debug.session {session $id has data ($stored)}
		set established($id) 1
	    }

	    default {
		error "Impossible State ([my check $id],[dict size $stored]) checking session $id"
	    }
	}

	# fetch and instantiate session variables by key
	# do it only when we've got a real request
	set vars [my fetch $id]
	dict set vars $key $id	;# the session var always exists
	Debug.session {coro VARS ([my fields $id]) fetched ($vars)}
	foreach n [my fields $id] {
	    Debug.session {open var $n}
	    catch {::trace remove variable ${sns}::$n {write unset} [list [self] varmod $id]}
	    
	    if {[dict exists $vars $n]} {
		Debug.session {open var assigning $n<-'[dict get $vars $n]'}
		namespace ${sns} [list variable $n [dict get $vars $n]]
	    } else {
		namespace $sns [list variable $n]
	    }
	    
	    ::trace add variable ${sns}::$n {write unset} [list [self] varmod $id]
	}
    }

    constructor {args} {
	Debug.session {constructing [self] $args}
	variable tdbc sqlite3		;# TDBC backend
	variable db ""			;# already open db
	variable reconnect {}		;# cmd prefix called if remote DB disconnects
	variable file ""		;# or db file
	variable tdbc_opts {}		;# ::tdbc::connection creation options
	variable schemafile ""		;# file containing schema
	variable schema {}		;# schema for empty dbs
	variable table ""		;# table for session vars

	variable lazy 0			;# set lazy to number of seconds to flush *only* periodically
	variable establish 1		;# auto-establish a persistent record for each new session?
	#variable pre			;# an apply body to run before domain handling
	#variable post			;# an apply body to run after domain handling

	variable {*}$args
	next {*}$args

	if {$table eq ""} {
	    set table $cookie		;# default table is named for cookie
	}
	if {$key eq ""} {
	    set key $cookie		;# default session key field is named for cookie
	}

	variable varmod			;# record session var mods per coro
	array set varmod {}
	variable established		;# has this session been persisted?
	array set established {}

	# set up the DB table
	if {$db ne ""} {
	    Debug.session {provided db: '$db'}
	} elseif {$file ne ""} {
	    package require tdbc
	    package require tdbc::$tdbc

	    set ons [namespace current]
	    Debug.session {creating db: tdbc::${tdbc}::connection create [namespace current]::dbI $file $tdbc_opts}
	    file mkdir [file dirname $file]
	    set db [tdbc::${tdbc}::connection new $file {*}$tdbc_opts]
	    oo::objdefine [self] forward db $db	;# make a db command alias
	} else {
	    error "Must provide a db file or an open db"
	}
	oo::objdefine [self] forward db {*}$db

	#Debug.session {db configure: [my db configure]}
	if {[my db tables] eq "" || $table ni [my db tables]} {
	    # we don't have any tables - apply schema
	    if {$schema eq "" && $schemafile ne ""} {
		set fd [open $schemafile r]
		set schema [read $fd]
		close $fd
	    }
	    if {$schema eq ""} {
		error "Must provide a schema,schemafile or an initialized db"
	    } else {
		Debug.session {schema: $schema}
		catch {my db allrows $schema}
	    }
	}

	variable max_prepcache 0	;# no limit to number of cached sql stmts

	# prepare some sql statemtents to NULL and UPDATE session vars
	if {$lazy} {
	    after [expr {$lazy * 1000}] [list [self] flush_lazy]
	}
    }
}

class create ::SessionREST {
    method / {r} {
	Debug.session {TestSession running in [info coroutine]}
	my variable counter
	if {[info exists counter]} {
	    Debug.session {counter exists: $counter}
	} else {
	    Debug.session {counter does not exist}
	}
	incr counter

	my variable session
	append content [<p> "COUNT $counter in $session"]
	append content [<a> href /variables Variables] \n
	append content [<a> href /establish Establish] \n
	append content [<a> href /unset Unset] \n

	return [Http NoCache [Http Ok $r $content]]
    }

    method /variables {r} {
	return [Http NoCache [Http Ok $r [<p> "VARIABLES [my variables]"]]]
    }

    method /establish/1 {r} {
	my establish	;# this makes this session persist
	mt variable session
	return [Http NoCache [Http Ok $r [<p> "ESTABLISHED $session"]]]
    }

    method /establish {r} {
	puts stderr "Called /establish"
	my establish	;# this makes this session persist
	puts stderr "Called Session establish"
	return [Http NoCache [Http Ok $r [<p> "ESTABLISHED"]]]
    }

    method /unset {r} {
	my variable counter
	unset counter

	my variable session
	return [Http NoCache [Http Ok $r [<p> "unset counter in $session"]]]
    }

    method /badtime/1 {r} {
	my variable session
	catch {unset session} e eo
	return [Http NoCache [Http Ok $r [<p> "$session - if you unset the session variable, you will have a bad time, but you won't know it."]]]
    }

    method /badtime/2 {r} {
	my variable session
	catch {set session 1} e eo
	return [Http NoCache [Http Ok $r [<p> "$session - $e"]]]
    }

    superclass PersistSessionClass Direct
    constructor {args} {next {*}$args}
}

if {[info exists argv0] && [file normalize $argv0] eq [file normalize [info script]]} {
    # test the Session manager

    Debug on direct
    Debug on httpd
    Debug on session

    SessionREST create ::TestSession file test_session.db schema {
	DROP TABLE IF EXISTS session;
	CREATE TABLE session (
			      session VARCHAR(32) PRIMARY KEY,
			      counter INTEGER
			      );
    }

    ::H::load Hproc.tcl
    set port 8080
    variable {*}$argv

    set ::listener [H listen process [list ::TestSession do] $::port]

    ::vwait forever
}
