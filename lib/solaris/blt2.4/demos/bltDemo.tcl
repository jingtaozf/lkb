#!/coli/apps/tcl8.0+tk8.0/bin/bltwish
# ----------------------------------------------------------------------------
#
#	The following code is solely a convenience so that you can test the 
#	BLT distribution without first installing it.
#
# ----------------------------------------------------------------------------

#	If we're in the ./demos directory, we can simply specify "../library"
#	as the library directory without having to install the files. 

if { [file exists ../library] } {
    global blt_library
    set blt_library ../library
}

global auto_path
lappend auto_path $blt_library

#	Try to import the blt namespace into the global scope.  If it
#	fails, we'll assume BLT was loaded into the global scope.

if { $tcl_version >= 8.0 } {
#    wm protocol . WM_DELETE_WINDOW { exit }
    catch {namespace import blt::*} 
} else {
    catch { import add blt }
}

#	Add a binding for convenience to let you exit with pressing 
#	the "quit" button.

bind all <Control-KeyPress-c> { exit 0 } 
bind all <KeyPress-q> { exit 0 }
focus .

if { [info commands "namespace"] == "namespace" } {
    if { $tcl_version >= 8.0 } {
	catch {namespace import -force blt::tile::*} 
    } else {
	catch { import add blt::tile }
    }
} else {
    foreach cmd { button checkbutton radiobutton frame label 
	scrollbar toplevel menubutton listbox } {
	if { [info command tile${cmd}] == "tile${cmd}" } {
	    rename ${cmd} ""
	    rename tile${cmd} ${cmd}
	}
    }
}
