if {[catch {package require Tcl 8.2}]} return
package ifneeded Tktable 2.7 "package require Tk 8.2; [list load [file join $dir Tktable.so.2.7] Tktable]"
