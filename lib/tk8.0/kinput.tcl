# kinput.tcl --
#
# This file contains Tcl procedures used to input Japanese text.
#
# $Header$
#
# Copyright (c) 1993, 1999 Software Research Associates, Inc.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies and that both that
# copyright notice and this permission notice appear in supporting
# documentation, and that the name of Software Research Associates not be
# used in advertising or publicity pertaining to distribution of the
# software without specific, written prior permission.  Software Research
# Associates makes no representations about the suitability of this software
# for any purpose.  It is provided "as is" without express or implied
# warranty.
#

# Get class specific Kinput conversion style.
set kiStyle(Text) over
set kiStyle(Entry) over
foreach i {Text Entry} {
    set style [option get . tkKinputStyle($i) *]
    if {[string length $style] > 0} {
	set style [string tolower $style]
	if {[string compare $style "root"] == 0 ||
	    [string compare $style "over"]} {
	    set kiStyle($i) $style
	}
    }
}

# Get conversion start keys from X resource database.
set KIStart [option get . tkKinputStartKeys *]
if {[string length $KIStart] > 0} {
    # try to bind with the keys to check whether it is valid or not.
    foreach i $KIStart {
	if {[regexp {^<.*>$} $i] == 0} {
	    set i <${i}>
	}
	set tmp ""
	if {[catch {bind . $i {puts dummy}} tmp] == 0} {
	    catch {bind . $i ""}
	    lappend valids $i
	} else {
	    puts stderr "$tmp"
	}
	catch {unset tmp}
    }
    if {[string length $valids] > 0} {
	set KIStart $valids
    }
} else {
    set KIStart "<Control-backslash> <Control-Kanji> <Control-Shift_R>"
}

# ----------------------------------------------------------------------
# Class bindings for start Japanese text input (Kana-Kanji conversion).
# Use over-the-spot style for both text and entry widgets.
# ----------------------------------------------------------------------
set tStyle $kiStyle(Text)
set eStyle $kiStyle(Entry)
foreach i $KIStart {
    bind Text $i "kinput_start %W $tStyle"
    bind Entry $i "kinput_start %W $eStyle"
}
unset KIStart tStyle eStyle
unset kiStyle(Text)
unset kiStyle(Entry)
catch {unset kiStyle}

# The procedure below is invoked in order to start Japanese text input
# for the specified widget.  It sends a request to the input server to
# start conversion on that widget.
# Second argument specifies input style.  Valid values are "over" (for
# over-the-spot style) and "root" (for root window style). See X11R5
# Xlib manual for the meaning of these styles). The default is root
# window style.

proc kinput_start { w {style root} } {
    update
    global _kinput_priv
    if { ![ string compare $style "over" ] } {
	set spot [ $w xypos insert ]
	if { [ string length $spot ] <= 0 } {
	    set spot "[ $w cget -bo ] [ winfo height $w ]"
	}
	set font [ $w cget -font ]
	set attr [ font actual $font ]
	set compound [ lindex $attr end ]
	if { [ llength $compound ] < 1 } {
	    set font "{$font}"
	}
	trace variable _kinput_priv($w) w _kinput_trace_over
	kanjiInput start $w \
	    -variable _kinput_priv($w) \
	    -inputStyle over \
	    -foreground [ $w cget -foreground ] \
	    -background [ $w cget -background ] \
	    -fonts "$font" \
	    -clientArea [_kinput_area $w] \
	    -spot $spot
	return
    }
    trace variable _kinput_priv($w) w _kinput_trace_root
    kanjiInput start $w -variable _kinput_priv($w) -inputStyle root
}

# The procedure below is invoked to send the spot location (the XY
# coordinate of the point where characters to be inserted) to the
# input server.  It should be called whenever the location has
# been changed while in over-the-spot conversion mode.

proc kinput_send_spot {w} {
    if { [ catch { kanjiInput attribute $w } ] == 0 } {
	set spot [_kinput_spot $w]
	if { [ string length $spot ] > 0 } then {
	    kanjiInput attribute $w -spot $spot
	}
    }
}

#
# All of the procedures below are the internal procedures for this
# package.
#

# The following procedure returns the list of XY coordinate of the
# current insertion point of the specified widget.

proc _kinput_spot {w} {
    $w xypos insert
}

# The following procedure returns the list of drawing area of the
# specified widget. { x y width height }

proc _kinput_area {w} {
    set bw [ $w cget -bo ]
    return "$bw $bw [expr {[winfo width $w] - $bw*2}] [expr {[winfo height $w] - $bw*2}]"
}

# The following procedure returns the value of the specified option
# (resource).
#proc _kinput_attr {w option} {lindex [$w configure $option] 4}
proc _kinput_attr {w option} { $w cget $option }

# The two procedures below are callbacks of a variable tracing.
# The traced variable contains the text string sent from the
# input server as a conversion result.

# for root style
proc _kinput_trace_root {name1 name2 op} {
    upvar #0 $name1 trvar
    $name2 insert insert $trvar($name2)
    unset $trvar($name2)
}

# for over-the-spot style
proc _kinput_trace_over {name1 name2 op} {
    upvar #0 $name1 trvar
    $name2 insert insert $trvar($name2)
    update
    if { [ string compare [ winfo class $name2 ] "Entry" ] == 0 } {
	tkEntrySetCursor $name2 insert
    } else {
	tkTextSetCursor $name2 insert
    }
    unset $trvar($name2)
}


# tkEntryBackspace -- redefine.
# Backspace over the character just before the insertion cursor.
# If backspacing would move the cursor off the left edge of the
# window, reposition the cursor at about the middle of the window.
#
# Arguments:
# w -		The entry window in which to backspace.

proc tkEntryBackspace w {
    if {[$w selection present]} {
	$w delete sel.first sel.last
    } else {
	set x [expr {[$w index insert] - 1}]
	if {$x >= 0} {$w delete $x}
	if {[$w index @0] >= [$w index insert]} {
	    set range [$w xview]
	    set left [lindex $range 0]
	    set right [lindex $range 1]
	    $w xview moveto [expr {$left - ($right - $left)/2.0}]
	}
    }
    tkEntrySeeInsert $w
}


# tkEntrySeeInsert -- redefine.
# Make sure that the insertion cursor is visible in the entry window.
# If not, adjust the view so that it is.
#
# Arguments:
# w -           The entry window.

proc tkEntrySeeInsert w {
    set c [$w index insert]
    set left [$w index @0]
    if {$left > $c} {
        $w xview $c
	kinput_send_spot $w
        return
    }
    set x [winfo width $w]
    while {([$w index @$x] <= $c) && ($left < $c)} {
        incr left
        $w xview $left
    }
    kinput_send_spot $w
}


proc tkEntryConfigureEventProc {w} {
    if {[catch { kanjiInput attribute $w }] == 0} {
	update
	set spot [_kinput_spot $w]
	set area [_kinput_area $w]
	kanjiInput attribute $w -clientArea $area -spot $spot
    }
}
bind Entry <Configure> {tkEntryConfigureEventProc %W}

# tkTextSetCursor - redefine.
# Move the insertion cursor to a given position in a text.  Also
# clears the selection, if there is one in the text, and makes sure
# that the insertion cursor is visible.  Also, don't let the insertion
# cursor appear on the dummy last line of the text.
#
# Arguments:
# w -		The text window.
# pos -		The desired new position for the cursor in the window.

proc tkTextSetCursor {w pos} {
    if [$w compare $pos == end] {
	set pos {end - 1 chars}
    }
    $w mark set insert $pos
    $w tag remove sel 1.0 end
    $w see insert
    kinput_send_spot $w
}

proc tkTextConfigureEventProc {w} {
    if {[catch { kanjiInput attribute $w }] == 0} {
	update
	set spot [_kinput_spot $w]
	set area [_kinput_area $w]
	kanjiInput attribute $w -clientArea $area -spot $spot
    }
}
bind Text <Configure> {tkTextConfigureEventProc %W}



bind Text <1> {
    tkTextButton1 %W %x %y
    %W tag remove sel 0.0 end
    tkTextSetCursor %W insert
}

bind Text <Delete> {
    if {[%W tag nextrange sel 1.0 end] != ""} {
	%W delete sel.first sel.last
	tkTextSetCursor %W insert
    } else {
	tkTextSetCursor %W insert-1c
	%W delete insert
	%W see insert
    }
}

bind Text <Control-h> {
    if {[%W tag nextrange sel 1.0 end] != ""} {
	%W delete sel.first sel.last
	tkTextSetCursor %W insert
    } else {
	tkTextSetCursor %W insert-1c
	%W delete insert
	%W see insert
    }
}

bind Text <BackSpace> {
    if {[%W tag nextrange sel 1.0 end] != ""} {
	%W delete sel.first sel.last
	tkTextSetCursor %W insert
    } else {
	tkTextSetCursor %W insert-1c
	%W delete insert
	%W see insert
    }
}

bind Text <Return> {
    tkTextInsert %W \n
    tkTextSetCursor %W insert
}
