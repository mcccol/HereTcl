# Logchan - a channel which maintains a series of rotating and compressed logs
package require Debug
Debug define logchan

package provide Logchan 1.0

namespace eval Logchan {
    proc open {name args} {
	set fd [chan create write [namespace current]]
	chan configure $fd -buffering line name $name {*}$args
	Debug.logchan {open $name $args -> $fd}
	return $fd
    }

    proc initialize {cid mode} {
	upvar #0 [namespace current]::cid_$cid options
	array set options {rotate 4}
	Debug.logchan {initialized $cid}
	return {initialize finalize watch write configure cget cgetall}
    }

    proc finalize {cid} {
	upvar #0 [namespace current]::cid_$cid options
	Debug.logchan {finalized $cid}
	catch {flush $options(fd)}
	catch {close $options(fd)}
	unset options
    }

    proc watch {cid events} {}

    # Rotate - rotate the files for $cid stream
    proc Rotate {cid files args} {
	upvar #0 [namespace current]::cid_$cid options
	Debug.logchan {Rotating because '$args' ($files)}
	# move each file to the next-oldest file
	set elcount [llength [split [file tail $options(name)] .]]
	foreach oldest [lreverse [dict keys $files]] {
	    if {$oldest < $options(rotate)} {
		set fn [file tail [dict get $files $oldest name]]
		set els [lrange [split $fn .] $elcount+1 end]
		set newname [join [list $options(name) [expr {$oldest + 1}] {*}$els] .]
		Debug.logchan {Rotate - rotate [dict get $files $oldest name] -> $newname}
		file rename -force [dict get $files $oldest name] $newname
		if {[info exists options(compress)] && $options(compress) && [lindex $els end] ne "gz"} {
		    # compress $newname
		}
	    } else {
		# we have no more room to rotate, let this one be overwritten
		Debug.logchan {Rotate - drop $oldest}
	    }
	}

	# we've rotated all the files, now time to open a new file
	set ofd $options(fd)
	set options(fd) [::open $options(name) a]
	chan configure $options(fd) -buffering line
	Debug.logchan {Rotate - opening $options(fd)}
	flush $ofd; close $ofd
    }

    proc write {cid data} {
	Debug.logchan {write $cid}
	upvar #0 [namespace current]::cid_$cid options
	if {![info exists options(fd)]} {
	    set fd [::open $options(name) a]
	    set options(fsize) [file size $options(name)]
	    if {$options(fsize) > 0} {
		file stat $options(name) stat
		set options(ctime) $stat(ctime)
	    } else {
		set options(ctime) [clock seconds]
	    }
	    chan configure $fd -buffering line
	    set options(fd) $fd
	    Debug.logchan {open file $options(name) -> $fd}
	} else {
	    set fd $options(fd)
	}

	Debug.logchan {write '$data'}
	puts -nonewline $fd $data

	incr options(fsize) [string length $data]
	if {[info exists options(size)] && $options(size) && $options(fsize) > $options(size)} {
	    # size based rotation
	    Rotate $cid [Summary $cid] size
	} elseif {[info exists options(after)] && $options(after) && [clock seconds] > ($options(ctime)+$options(after))} {
	    # time based rotation
	    Rotate $cid [Summary $cid] [clock seconds] > ($options(ctime)+$options(after))
	}

	return [string length $data]
    }

    # Summary - create a summary of all the logfiles for this $cid
    proc Summary {cid} {
	upvar #0 [namespace current]::cid_$cid options
	set file $options(name)
	set dir [file dirname $file]
	set name [file tail $file]
	set ext [file extension $name]
	if {$ext eq ""} {
	    set ext .log
	}
	set root [file tail $name]
	set elcount [llength [split $root .]]
	set matching [glob -nocomplain -tails -path $file *]
	set files {}
	foreach fn $matching {
	    set els [lrange [split $fn .] $elcount end]
	    set num [lindex $els 0]
	    if {$num eq ""} {
		set num 0
	    }
	    file stat [file join $dir $fn] stat
	    dict set files $num [array get stat]
	    dict set files $num name [file join $dir $fn]
	}
	set files [lsort -integer -stride 2 $files]
	Debug.logchan {Summary $files}
	return $files
    }

    # Process - inspect the existing set of logfiles and rotate as required
    proc Process {cid} {
	upvar #0 [namespace current]::cid_$cid options
	set files [Summary $cid]
	if {![dict exists $files 0]} return
	set current [dict get $files 0]
	dict with current {}
	if {[info exists options(size)] && $options(size) && $size > $options(size)} {
	    # size based rotation
	    Rotate $cid $files
	} elseif {[info exists options(after)] && $options(after) && [clock seconds] > ($ctime+$options(after))} {
	    # time based rotation
	    Rotate $cid $files
	}
    }

    variable periods [list minutely 60 hourly [expr {60 * 60}] daily [expr {24 * 60 * 60}] weekly [expr {7 * 24 * 60 * 60}]]

    proc configure {cid option value} {
	upvar #0 [namespace current]::cid_$cid options
	Debug.logchan {configure $cid $option '$value'}
	switch -- $option {
	    name {
		if {[info exists options(name)]} {
		    error "Can't rename logfile '$options(name)'"
		}
		set value [file normalize $value]
		set options(name) $value
	    }

	    minutely -
	    hourly -
	    daily -
	    weekly {
		variable periods
		set options($option) 1
		set options(after) [expr {$value * [dict get $periods $option]}]
	    }

	    default {
		set options($option) $value
	    }
	}
    }

    proc cget {cid option} {
	upvar #0 [namespace current]::cid_$cid options
	return $options($option)
    }

    proc cgetall {cid} {
	upvar #0 [namespace current]::cid_$cid options
	return [array get options]
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
