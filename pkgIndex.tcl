if {"[file join $dir U]" ni $::auto_path} {
    lappend ::auto_path [file join $dir U] [file join $dir D]
}
package ifneeded H 8.0 [list source [file join $dir H.tcl]]
package ifneeded HConvert 1.0 [list source [file join $dir HConvert.tcl]]
package ifneeded HWS 2.0 [list source [file join $dir HWS.tcl]]
