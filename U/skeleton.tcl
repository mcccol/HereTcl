#!/usr/bin/tclsh

# cp skeleton.tcl /etc/init.d/
# ln -s /etc/init.d/skeleton.tcl /etc/init.d/whatever
# update-rc.d whatever defaults
# edit /etc/default/whatever - specifically with a chdir directory

### BEGIN INIT INFO
# Provides:          tcl_daemon
# Required-Start:    $remote_fs $syslog $named
# Required-Stop:     $remote_fs $syslog $named
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Start daemon at boot time
# Description:       Enable service provided by daemon.
### END INIT INFO

# Author: Colin McCormack <mcccol@gmail.com>

set name [string trimleft [file tail [info script]] SK0123456789]

set args {}
if {[file exists /etc/default/$name]} {
    source /etc/default/$name
}

set which /usr/bin/which
set daemon [exec $which daemon]
set tclsh [exec $which tclsh$tcl_version]
set respawn 1

lappend options --name=$name

if {[info exists user]} {
    if {![info exists group]} {
	set group $user
    }
    lappend options --user=$user:$group
}

foreach var {config chroot chdir pidfile pidfiles} {
    if {[info exists $var]} {
	set $var [file normalize [set $var]]
	lappend options --$var=[set $var]
    }
}

foreach var {stdout stderr dbglog errlog} {
    if {[info exists $var]} {
	lappend options --$var=[set $var]
    }
}

foreach var {attempts acceptable delay limit} {
    if {[info exists $var] && [set $var]} {
	lappend options --$var=[set $var]
    }
}

foreach var {respawn core pty inherit unsafe safe} {
    if {[info exists $var] && [set $var]} {
	lappend options --$var
    }
}

if {![info exists script]} {
    if {[info exists chdir]} {
	if {[file exists [file join $chdir $name]]} {
	    set script [file join $chdir $name]
	} elseif {[file exists [file join $chdir $name.tcl]]} {
	    set script [file join $chdir $name].tcl
	}
    }
}

set argv [lassign $argv op]
switch -- [string tolower $op] {
    start {
	# Return
	#   0 if daemon has been started
	#   1 if daemon was already running
	#   2 if daemon could not be started
	try {
	    exec $daemon {*}$options -- $tclsh $script{*}$argv {*}$args
	} on error {e eo} {
	    exit 2
	} on ok {e eo} {
	    exit 0
	}
    }

    stop {
	# Return
	#   0 if daemon has been stopped
	#   1 if daemon was already stopped
	#   2 if daemon could not be stopped
	#   other if a failure occurred
	try {
	    exec $daemon --stop {*}$options -- $tclsh $script{*}$argv {*}$args
	} on error {e eo} {
	    exit 2
	} on ok {e eo} {
	    exit 0
	}
    }

    status {
	try {
	    exec $daemon --running {*}$options -- $tclsh $script{*}$argv {*}$args
	} on error {e eo} {
	    exit 2
	} on ok {e eo} {
	    exit 0
	}
    }

    restart - force-restart {
	try {
	    exec $daemon --respawn {*}$options -- $tclsh $script{*}$argv {*}$args
	} on error {e eo} {
	    exit 2
	} on ok {e eo} {
	    exit 0
	}
    }

    default {
	puts stderr "Usage: $argv0 {start|stop|restart|force-restart}"
    }
}

exit 1
