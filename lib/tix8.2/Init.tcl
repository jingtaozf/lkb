# Init.tcl --
#
#	Initializes the Tix library and performes version checking to
#	ensure the Tcl, Tk and Tix script libraries loaded matches
#	with the binary of the respective packages.
#
# Copyright (c) 1993-1999 Ioi Kim Lam.
# Copyright (c) 2000-2001 Tix Project Group.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id$

# __tixInit
#
#       This procedure completes the Tcl-level initialization of Tix.
#
# Arguments:
# none.

proc __tixInit {} {
    global tix tixPriv env tix_version tix_patchLevel tk_version tix_library
    global auto_path

    if {[info exists tix(initialized)]} {
	return
    }

    if {![info exists tix_library]} {
        # we're running from stand-alone module. 
        set tix_library ""
    } elseif {[file isdir $tix_library]} {
        if {![info exists auto_path] ||
            [lsearch $auto_path $tix_library] == -1} {
            lappend auto_path $tix_library
        }
    }

    # STEP 1: Version checking
    #
    #
    package require -exact Tix 8.2

    # STEP 2: Initialize file compatibility modules
    #
    #
    if {[info exists tixPriv(isWindows)]} {
	tixInitFileCmpt:Win
    } elseif {[info exists env(WINDOWS_EMU_DEBUG)]} {
	tixInitFileCmpt:Win
	tixWinFileEmu
    } else {
	tixInitFileCmpt:Unix
    }

    # STEP 3: Initialize the Tix application context
    #
    #

    tixAppContext tix

    # STEP 4: Initialize the bindings for widgets that are implemented in C
    #
    #
    if {[string compare [info command tixHList] ""]} {
	tixHListBind
    }
    if {[string compare [info command tixTList] ""]} {
	tixTListBind
    }
    if {[string compare [info command tixGrid]  ""]} {
	tixGridBind
    }
    tixComboBoxBind
    tixControlBind
    tixFloatEntryBind
    tixLabelEntryBind
    tixScrolledGridBind
    tixScrolledListBoxBind

    rename __tixInit {}
}

# tixWidgetClassEx --
#
#       This procedure is similar to tixWidgetClass, except it
#       performs a [subst] on the class declaration before evaluating
#       it. This gives us a chance to specify platform-specific widget
#       default without using a lot of ugly double quotes.
#
#       The use of subst'able entries in the class declaration should
#       be restrained to widget default values only to avoid producing
#       unreadable code.
#
# Arguments:
# name -	The name of the class to declare.
# classDecl -	Various declarations about the class. See documentation
#               of tixWidgetClass for details.

proc tixWidgetClassEx {name classDecl} {
    tixWidgetClass $name [uplevel [list subst $classDecl]]
}


#
# Some debugging stuff, for Tix core tests only
#

if {0} {
    proc tixDebug {message {level "1"}} {
        global tix_priv
        if {[info exists tix_priv(debug)] && $tix_priv(debug) > 0} {
            puts stderr $message
        }
    }
} else {
    proc tixDebug {args} {}
}

if {0} {
    #
    # old relic. should be removed soon.
    #
    rename option old_option
    proc option {args} {
        global optionlist
        lappend optionlist $args
        return [eval old_option $args]
    }

    proc showoptions {} {
        global optionlist
        foreach t [lsort $optionlist] {
            puts $t
        }
    }
}

# Hack: if env(TIX_DEBUG_INTERACTIVE) is set, then
# an interactive prompt is always printed
#
if {[info exists env(TIX_DEBUG_INTERACTIVE)] &&
    ![info exists tix_priv(slaveInterp)]} {

    # For widget programming, it is more convient to have the error
    # message printed on the terminal. For some extensive usage of
    # bindings, such as in the case of tixBalloon, the default
    # bgerror just doesn't work.
    #
    proc bgerror {err} {
        global errorInfo
        puts $err
        puts $errorInfo
    }
}

# Deprecated
proc tixFileJoin {args} {
    return [eval file join $args]
}

#
# if tix_library is not defined, we're running in SAM mode. __tixInit
# will be called later by the Tix_Init() C code.
#

if {[info exists tix_library]} {
    __tixInit
}
