#
# bltDnd.tcl
#
# ----------------------------------------------------------------------
# Bindings for the BLT drag&drop command
# ----------------------------------------------------------------------
#   AUTHOR:  George Howlett
#            Bell Labs Innovations for Lucent Technologies
#            gah@bell-labs.com
#            http://www.tcltk.com/blt
# ----------------------------------------------------------------------
# Copyright (c) 1998  Lucent Technologies, Inc.
# ======================================================================
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted,
# provided that the above copyright notice appear in all copies and that
# both that the copyright notice and warranty disclaimer appear in
# supporting documentation, and that the names of Lucent Technologies
# any of their entities not be used in advertising or publicity
# pertaining to distribution of the software without specific, written
# prior permission.
#
# Lucent Technologies disclaims all warranties with regard to this
# software, including all implied warranties of merchantability and
# fitness.  In no event shall Lucent be liable for any special, indirect
# or consequential damages or any damages whatsoever resulting from loss
# of use, data or profits, whether in an action of contract, negligence
# or other tortuous action, arising out of or in connection with the use
# or performance of this software.
#
# ======================================================================

# ----------------------------------------------------------------------
#
# DndInit --
#
#	Invoked from C whenever a new drag&drop source is created.
#	Sets up the default bindings for the drag&drop source.
#
#	<ButtonPress-?>	 Starts the drag operation.
#	<B?-Motion>	 Updates the drag.
#	<ButtonRelease-?> Drop the data on the target.
#
# Arguments:	
#	widget		source widget
#	button		Mouse button used to activate drag.
#	cmd		"dragdrop" or "blt::dragdrop"
#
# ----------------------------------------------------------------------
proc blt::DndInit { widget button cmd } {
    set tagName "bltDnd$button"
    bind $tagName <ButtonPress-$button> [list $cmd drag %W %X %Y]
    bind $tagName <B$button-Motion> [list $cmd drag %W %X %Y]
    bind $tagName <ButtonRelease-$button> [list $cmd drop %W %X %Y]
    set tags [bindtags $widget]
    if { [lsearch $tags $tagName] < 0 } {
	bindtags $widget [linsert $tags 0 $tagName]
    }	
}
