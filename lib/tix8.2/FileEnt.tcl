# FileEnt.tcl --
#
# 	TixFileEntry Widget: an entry box for entering filenames.
#
# Copyright (c) 1993-1999 Ioi Kim Lam.
# Copyright (c) 2000      Tix Project Group.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id$

tixWidgetClassEx tixFileEntry {
    -classname TixFileEntry
    -superclass tixLabelWidget
    -method {
	invoke filedialog update
    }
    -flag {
	-activatecmd -command -dialogtype -disablecallback
	-disabledforeground -filebitmap -fileimage -selectmode -state
	-usetkdialog -validatecmd -value -variable
    }
    -forcecall {
	-variable
    }
    -static {
	-filebitmap
    }
    -configspec {
	{-activatecmd activateCmd ActivateCmd ""}
	{-command command Command ""}
	{-dialogtype dialogType DialogType ""}
	{-disablecallback disableCallback DisableCallback 0 tixVerifyBoolean}
	{-disabledforeground disabledForeground DisabledForeground  
         [tixGetDefault DISABLED]
        }
	{-filebitmap fileBitmap FileBitmap ""}
	{-fileimage fileImage FileImage ""}
	{-selectmode selectMode SelectMode normal}
	{-state state State normal}
        {-usetkdialog useTkDialog UseTkDialog 1 tixVerifyBoolean}
	{-validatecmd validateCmd ValidateCmd ""}
	{-value value Value ""}
	{-variable variable Variable ""}
    }
}

proc tixFileEntry:InitWidgetRec {w} {
    upvar #0 $w data

    tixChainMethod $w InitWidgetRec
    set data(varInited)	  0

    if {$data(-filebitmap) == "" && $data(-fileimage) == ""} {
        set data(-fileimage) [tix getimage openfold]
    }
}

proc tixFileEntry:ConstructFramedWidget {w frame} {
    upvar #0 $w data

    tixChainMethod $w ConstructFramedWidget $frame

    $frame config -bd [tixGetDefault TIX_BORDER_WIDTH] -relief sunken

    set data(w:entry)  [entry  $frame.entry -highlightthickness 0 -bd 0]
    set data(w:button) [button $frame.button -takefocus 0 \
            -highlightthickness 0]

    if {$data(-fileimage) != ""} {
        $data(w:button) config -image $data(-fileimage)
    } else {
        $data(w:button) config -bitmap $data(-filebitmap)
    }

    set data(entryfg) [$data(w:entry) cget -fg]

    pack $data(w:button) -side right -fill both
    pack $data(w:entry)  -side left  -expand yes -fill both
}

proc tixFileEntry:SetBindings {w} {
    upvar #0 $w data

    tixChainMethod $w SetBindings

    $data(w:button) config -command [list tixFileEntry:OpenFile $w]
    tixSetMegaWidget $data(w:entry) $w

    # If user press <return>, verify the value and call the -command
    #
    bind $data(w:entry) <Return> "tixFileEntry:invoke $w"
    bind $data(w:entry) <KeyPress> {
	if {[set [tixGetMegaWidget %W](-selectmode)] == "immediate"} {
	    tixFileEntry:invoke [tixGetMegaWidget %W]
	}
    }
    bind $data(w:entry) <FocusOut>  {
        if {"%d" == "NotifyNonlinear" || "%d" == "NotifyNonlinearVirtual"} {
	    tixFileEntry:invoke [tixGetMegaWidget %W]
        }
    }
    bind $w <FocusIn> "focus $data(w:entry)"
}

#----------------------------------------------------------------------
#                           CONFIG OPTIONS
#----------------------------------------------------------------------
proc tixFileEntry:config-state {w value} {
    upvar #0 $w data

    if {$value == "normal"} {
	$data(w:button) config -state $value
	$data(w:entry)  config -state $value -fg $data(entryfg)
	catch {
	    $data(w:label)  config -fg $data(entryfg)
	}
    } else {
	$data(w:button) config -state $value
	$data(w:entry)  config -state $value -fg $data(-disabledforeground)
	catch {
	    $data(w:label)  config -fg $data(-disabledforeground)
	}
    }

    return ""
}

proc tixFileEntry:config-value {w value} {
    tixFileEntry:SetValue $w $value
}

proc tixFileEntry:config-variable {w arg} {
    upvar #0 $w data

    if {[tixVariable:ConfigVariable $w $arg]} {
       # The value of data(-value) is changed if tixVariable:ConfigVariable 
       # returns true
       tixFileEntry:SetValue $w $data(-value)
    }
    catch {
	unset data(varInited)
    }
    set data(-variable) $arg
}

#----------------------------------------------------------------------
#                         User Commands
#----------------------------------------------------------------------
proc tixFileEntry:invoke {w} {
    upvar #0 $w data

    if {[catch {$data(w:entry) index sel.first}] == 0} {
	# THIS ENTRY OWNS SELECTION --> TURN IT OFF
	#
	$data(w:entry) select from end
	$data(w:entry) select to   end
    }

    tixFileEntry:SetValue $w [$data(w:entry) get]
}

proc tixFileEntry:filedialog {w args} {
    upvar #0 $w data

    if {$args == ""} {
	return [tix filedialog $data(-dialogtype)]
    } else {
	return [eval [tix filedialog $data(-dialogtype)] $args]
    }
}

proc tixFileEntry:update {w} {
    upvar #0 $w data

    if {"x[$data(w:entry) get]" != "x$data(-value)"} {
	tixFileEntry:invoke $w
    }
}
#----------------------------------------------------------------------
#                       Internal Commands
#----------------------------------------------------------------------

# Gets called when the button subwidget is pressed.
#
proc tixFileEntry:OpenFile {w} {
     upvar #0 $w data

     if {$data(-activatecmd) != ""} {
	 uplevel #0 $data(-activatecmd)
     }

     if {$data(-usetkdialog)} {
         set filename [tk_getOpenFile -parent $w]
         if {$filename != ""} {
             tixFileEntry:SetValue $w $filename
         }
     } else {
         set filedlg [tix filedialog $data(-dialogtype)]

         $filedlg config -parent [winfo toplevel $w] \
             -command [list tixFileEntry:FileDlgCallback $w]

         focus $data(w:entry)

         $filedlg popup
     }
}

proc tixFileEntry:FileDlgCallback {w args} {
    set filename [tixEvent flag V]

    tixFileEntry:SetValue $w $filename
}

proc tixFileEntry:SetValue {w value} {
    upvar #0 $w data

    set value [file nativename $value]
    if {$data(-validatecmd) != ""} {
	set value [tixEvalCmdBinding $w $data(-validatecmd) "" $value]
    }

    if {$data(-state) == "normal"} {
	$data(w:entry) delete 0 end
	$data(w:entry) insert 0 $value
	$data(w:entry) xview end
    }

    set data(-value) $value

    tixVariable:UpdateVariable $w

    if {$data(-command) != "" && !$data(-disablecallback)} {
	if {![info exists data(varInited)]} {
	    set bind(specs) ""
	    tixEvalCmdBinding $w $data(-command) bind $value
	}
    }
}

proc tixFileEntry:Destructor {w} {
    upvar #0 $w data

    tixUnsetMegaWidget $data(w:entry)
    tixVariable:DeleteVariable $w

    # Chain this to the superclass
    #
    tixChainMethod $w Destructor
}

