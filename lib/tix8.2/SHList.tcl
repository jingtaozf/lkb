# SHList.tcl --
#
#	This file implements Scrolled HList widgets.
#
#       The automatic showing/hiding of the scroll bars is handled in
#       the superclass. See SWidget.tcl.
#
# Copyright (c) 1993-1999 Ioi Kim Lam.
# Copyright (c) 2000      Tix Project Group.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id$

tixWidgetClassEx tixScrolledHList {
    -classname TixScrolledHList
    -superclass tixScrolledWidget
    -method {
    }
    -flag {
	 -highlightbackground -highlightcolor -highlightthickness
    }
    -configspec {
	{-highlightbackground -highlightBackground HighlightBackground
         [tixGetDefault NORMAL_BG]
        }
	{-highlightcolor -highlightColor HighlightColor 
         [tixGetDefault HIGHLIGHT]
        }
	{-highlightthickness -highlightThickness HighlightThickness
         [tixGetDefault TIX_HIGHLIGHT_THICKNESS]
        }
    }
    -default {
	{.scrollbar			auto}
	{*f1.borderWidth		[tixGetDefault TIX_BORDER_WIDTH]}
	{*f1.relief     		sunken}
	{*hlist.takeFocus		1}
	{*Scrollbar.takeFocus		0}
    }
    -forcecall {
	-highlightbackground -highlightcolor -highlightthickness
    }
}

proc tixScrolledHList:ConstructWidget {w} {
    upvar #0 $w data

    tixChainMethod $w ConstructWidget

    # We need to make the HList's border to be zero and put it inside
    # a special frame. This is for the handling of window items inside
    # the HList. If the HList has a border, window items can be
    # displayed on top of the HList's border, resulting in bad visual
    # effect.
    #
    # TODO: make this proc common to STList and SGrid as well

    set data(pw:f1) \
	[frame $w.f1 -takefocus 0]
    set data(w:hlist) \
	[tixHList $w.f1.hlist -bd 0 -takefocus 1 -highlightthickness 0]

    pack $data(w:hlist) -in $data(pw:f1) -expand yes -fill both -padx 0 -pady 0

    set data(w:hsb) \
	[scrollbar $w.hsb -orient horizontal -takefocus 0]
    set data(w:vsb) \
	[scrollbar $w.vsb -orient vertical -takefocus 0]
    
    set data(pw:client) $data(pw:f1)
}

proc tixScrolledHList:SetBindings {w} {
    upvar #0 $w data

    tixChainMethod $w SetBindings

    $data(w:hlist) config \
	-xscrollcommand "$data(w:hsb) set"\
	-yscrollcommand "$data(w:vsb) set"\
	-sizecmd [list tixScrolledWidget:Configure $w]

    $data(w:hsb) config -command "$data(w:hlist) xview"
    $data(w:vsb) config -command "$data(w:hlist) yview"

}

#----------------------------------------------------------------------
#
#		option configs
#----------------------------------------------------------------------
proc tixScrolledHList:config-takefocus {w value} {
    upvar #0 $w data
  
    $data(w:hlist) config -takefocus $value
}	

proc tixScrolledHList:config-highlightbackground {w value} {
    upvar #0 $w data

    $data(pw:f1) config -highlightbackground $value
}

proc tixScrolledHList:config-highlightcolor {w value} {
    upvar #0 $w data

    $data(pw:f1) config -highlightcolor $value
}

proc tixScrolledHList:config-highlightthickness {w value} {
    upvar #0 $w data

    $data(pw:f1) config -highlightthickness $value
}


#----------------------------------------------------------------------
#
#		Widget commands
#----------------------------------------------------------------------

#----------------------------------------------------------------------
#
#		Private Methods
#----------------------------------------------------------------------
# virtual
#
proc tixScrolledHList:RepackHook {w} {
    upvar #0 $w data

if 0 {
    if {[tixGetBoolean [$data(w:hlist) cget -header]]} {
	set data(vsbPadY) [winfo height $data(w:hlist).tixsw:header]
    } else {
	set data(vsbPadY) 0
    }

    puts $data(vsbPadY)\ $data(w:hlist).tixsw:header
}
    tixChainMethod $w RepackHook
}
#----------------------------------------------------------------------
# virtual functions to query the client window's scroll requirement
#----------------------------------------------------------------------
proc tixScrolledHList:GeometryInfo {w mW mH} {
    upvar #0 $w data

    set extra [expr [$w.f1 cget -bd]+[$w.f1 cget -highlightthickness]]

    set mW [expr $mW - $extra*2]
    set mH [expr $mH - $extra*2]

    if {$mW < 1} {
	set mW 1
    }
    if {$mH < 1} {
	set mH 1
    }

    return [$data(w:hlist) geometryinfo $mW $mH]
}
