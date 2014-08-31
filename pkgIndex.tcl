lappend ::auto_path [file join $dir U] [file join $dir D]
package ifneeded H 7.0 [list source [file join $dir H.tcl]]
