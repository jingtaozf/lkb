if {[catch {set tkImPriv(debug)}]} {
    set tkImPriv(debug) 0
}

proc tkGetPreferredIMInputStyle {prefStyle supportedStyle} {
    foreach i $prefStyle {
	if {[lsearch $supportedStyle $i] >= 0} {
	    return $i
	}
    }
    return ""
}


proc tkGetIMInputStyle {path} {
    global tkImPriv
    set scrn [winfo screen $path]
    
    set sList ""
    if {[catch {set sList $tkImPriv(imSupportedStyles:${scrn})}] == 1} {
	catch {set sList [imconfigure $path -supportedStyle]}
	set tkImPriv(imSupportedStyles:${scrn}) [lsort $sList]
    }
    if {[string length $sList] <= 0} {
	puts stderr "Warning: No input method server is available."
	return ""
    }

    if {[catch {lappend s $tkImPriv(imPreferredStyles:$path)}] == 0} {
	set s [tkGetPreferredIMInputStyle $s $sList]
	if {[string length $s] > 0} {
	    return $s
	}
    }

    set style ""
    set class [winfo class $path]
    catch {set style $tkImPriv(imPreferredStyles:${class})}
    if {[string length $style] > 0} {
	if {[string compare "none" [lindex $style 0]] == 0} {
	    puts stderr "Warning: The input method server can't handle: [lindex $style 1]"
	    return ""
	} else {
	    return $style
	}
    } else {
	set userPref [option get . tkPreferredImStyle($class) *]
	if {[string length $userPref] > 0} {
	    set style [tkGetPreferredIMInputStyle $userPref $sList]
	    if {[string length $style] > 0} {
		set tkImPriv(imPreferredStyles:${class}) $style
		return $style
	    } else {
		set tkImPriv(imPreferredStyles:${class}) [list none $userPref]
		puts stderr "Warning: The input method server can't handle style: {$userPref}"
		return ""
	    }
	} else {
	    switch "$class" {
		"Text" {
		    set userPref [list "PreeditPosition StatusArea" "PreeditPosition StatusNothing" "PreeditArea StatusArea" "PreeditNothing StatusNothing"]
		}
		"Entry" {
		    set userPref [list "PreeditPosition StatusNothing" "PreeditNothing StatusNothing"]
		}
		"Canvas" {
		    set userPref [list "PreeditArea StatusArea" "PreeditNothing StatusNothing"]
		}
		default {
		    set userPref [list "PreeditPosition StatusArea" "PreeditPosition StatusNothing" "PreeditArea StatusArea" "PreeditNothing StatusNothing"]
		}
	    }
	    set style [tkGetPreferredIMInputStyle $userPref $sList]
	    if {[string length $style] > 0} {
		set tkImPriv(imPreferredStyles:${class}) $style
		return $style
	    } else {
		set tkImPriv(imPreferredStyles:${class}) [list none $userPref]
		puts stderr "Warning: The input method server can't handle: {$userPref}"
		return ""
	    }
	}
    }
    return ""
}


proc tkCleanIMInputStyle {path} {
    global tkImPriv
    foreach i "imSupportedStyles:[winfo screen $path] imPreferredStyles:[winfo class $path] imPreferredStyles:${path}" {
	catch {unset tkImPriv($i)}
    }
}


# return 1 means "using on-the-spot".
proc tkConfigIm {path {force 0}} {
    global tkImPriv
    set stat ""
    catch {set stat [imconfigure $path -status]}
    if {[string length $stat] <= 0} {
	tkCleanIMInputStyle $path
	return 0
    }
    set started 0
    if {$force == 1} {
	set ret "imconfigure $path -force"
    } else {
	set ret "imconfigure $path"
    }
    set style ""
    if {[string compare $stat "never"] == 0 ||
	[catch {set tkImPriv(imSetupDeleted:$path)}] == 0} {
	catch {unset tkImPriv(imSetupDeleted:$path)}
	set style [tkGetIMInputStyle $path]
	if {[string length $style] <= 0} {
	    return 0
	}
	append ret " -style {$style}"
    } else {
	set style [imconfigure $path -style]
	set started 1
    }

    set preedit [lindex $style 0]
    set status [lindex $style 1]

    if {[string compare $status "StatusCallbacks"] == 0 ||
	[string compare $preedit "PreeditCallbacks"] == 0} {
	if {$started == 1} {
	    return 0
	}
	append ret " -callback [tkGetIMCallbackProc $path]"
	if {[catch {eval $ret} msg]} {
	    puts stderr "$msg"
	    return 0
	} else {
	    return 1
	}
    }
	    
    set fg ""
    set bg ""
    set font ""
    set spot ""
    set doStatusArea 0
    if {[string compare $status "StatusArea"] == 0} {
	set doStatusArea 1
    }
	
    if {$tkImPriv(debug) == 0} {
	if {[catch {set fg [$path cget -fg]}]} {
	    set fg black
	}
	if {[catch {set bg [$path cget -bg]}]} {
	    set bg white
	}
    } else {
	set fg red
	set bg blue
    }
    
    if {[string compare $preedit "PreeditArea"] == 0 && $doStatusArea == 1} {
	append ret " -foreground {$bg} -background {$fg}"
    } else {
	append ret " -foreground {$fg} -background {$bg}"
    }

    set fontH 0
    if {[catch {set font [$path cget -font]}]} {
	set font Mincho:Courier-12
    } else {
	if {[string length $font] <= 0} {
	    set font Mincho:Courier-12
	} else {
	    set compound ""
	    catch {set compound [font conf $font -compound]}
	    if {[string length $compound] <= 0} {
		set wFontH [expr [font metrics $font -ascent] + [font metrics $font -descent]]
		set defFont [font failsafe]
		if {[string length $defFont] > 0} {
		    set defFontH [expr [font metrics $defFont -ascent] + [font metrics $defFont -descent]]
		    if {$defFontH > $wFontH} {
			set fontH $defFontH
		    } else {
			set fontH $wFontH
		    }
		}
	    }
	}
    }
    append ret " -font {$font}"

    if {$started == 0} {
	if {[catch {eval $ret} msg]} {
            puts stderr "$msg"
	    return 0
	}
	if {$force == 1} {
	    set ret "imconfigure $path -force"
	} else {
	    set ret "imconfigure $path"
	}
    }
    
    set bo [expr [$path cget -bo] + [$path cget -highlightthickness]]
    set w [expr [winfo width $path] - $bo * 2]
    set h [expr [winfo height $path] - $bo * 2]
    if {$fontH == 0} {
	set fontH [expr [font metrics $font -ascent] + [font metrics $font -descent]]
    }
    set spotX $bo
    set spotY $fontH
    set stH $fontH
    set stW [expr $fontH * 3]
    if {$doStatusArea == 1} {
	set spArea ""
	catch {set spArea [imconfigure $path -preferredStatusArea]}
	if {[string length $spArea] > 0} {
	    set pstW [lindex $spArea 2]
	    set pstH [lindex $spArea 3]
	    if {$pstW > 0 && $pstH > 0} {
		if {[expr ${w}.0 / ${pstW}.0] <= 5.0} {
		    set pstW [expr $pstH * 3]
		}
		if {$stW < $pstW} {
		    set stW $pstW
		}
		if {$stH < $pstH} {
		    set stH $pstH
		}
	    }
	}
    }
    
    switch "$preedit" {
	"PreeditPosition" {
	    catch {set spot [$path xypos insert]}
	    if {[string length $spot] > 0} {
		set spotX [lindex $spot 0]
		set spotY [lindex $spot 1]
	    }
	    append ret " -spot {$spotX $spotY} -preeditArea {$bo $bo $w $h}"
	    if {$doStatusArea == 1} {
		set stY [expr $spotY + $bo]
		append ret " -statusArea {$spotX $stY $stW $stH}"
	    }
	}

	"PreeditArea" {
	    switch "$status" {
		"StatusNothing" {
		    append ret " -preeditArea {$bo $bo $w $h}"
		}
		"StatusArea" {
		    set H [expr $h - $stH + $bo]
		    set pX [expr $bo + $stW]
		    set pW [expr $w - $pX + $bo]
		    append ret " -preeditArea {$pX $H $pW $stH} -statusArea {$bo $H $stW $stH}"
		}
	    }
	}
    }
    if {[catch {eval $ret} msg]} {
        puts stderr $msg
    }
    return 0
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
    global tkImPriv
    set useOnTheSpot [catch {set tkImPriv(useOnTheSpot:$w)}]
    set c [$w index insert]
    set left [$w index @0]
    if {$left > $c} {
        $w xview $c
	if {$useOnTheSpot == 0} {
	    tkConfigIm $w
	}
        return
    }
    set x [winfo width $w]
    while {([$w index @$x] <= $c) && ($left < $c)} {
        incr left
        $w xview $left
    }
    if {$useOnTheSpot == 0} {
	tkConfigIm $w
    }
}


# tkTextInsert -- redefine.
# Insert a string into a text at the point of the insertion cursor.
# If there is a selection in the text, and it covers the point of the
# insertion cursor, then delete the selection before inserting.
#
# Arguments:
# w -           The text window in which to insert the string
# s -           The string to insert (usually just a single character)

proc tkTextInsert {w s} {
    if {($s == "") || ([$w cget -state] == "disabled")} {
        return
    }
    catch {
        if {[$w compare sel.first <= insert]
                && [$w compare sel.last >= insert]} {
            $w delete sel.first sel.last
        }
    }
    $w insert insert $s
    $w see insert
    global tkImPriv
    if {[catch {set tkImPriv(useOnTheSpot:$w)}]} {
	tkConfigIm $w
    }
}


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
    global tkImPriv
    if {[catch {set tkImPriv(useOnTheSpot:$w)}]} {
	tkConfigIm $w
    }
}

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


# on-the-spot
bind IMOnTheSpotTag <Destroy> {
    global tkImPriv
    tkDestroyIMStatusArea %W
}


proc tkGetIMStatusGeometry {path} {
    global tkImPriv
    set sTop ""
    catch {set sTop $tkImPriv(imStatusTop:${path})}
    if {[string length $sTop] <= 0} {
	return
    }
    set sW [winfo reqw $sTop]
    set sH [winfo reqh $sTop]
    set maxX [expr [winfo screenwidth .] - $sW]
    set maxY [expr [winfo screenheight .] - $sH]
    set X [expr [winfo rootx $path] - $sW]
    set Y [expr [winfo rooty $path] - $sH + [winfo reqh $path]]
    if {$X < 0} {
	set X 0
    }
    if {$Y < 0} {
	set Y 0
    }
    if {$X > $maxX} {
	set X $maxX
    }
    if {$Y > $maxY} {
	set Y $maxY
    }
    return "+${X}+${Y}"
}
    

proc tkGetPathOptionForIMStatusArea {path} {
    set fg ""
    set bg ""
    set font ""
    catch {set fg "-foreground [$path cget -foreground]"}
    catch {set bg "-background [$path cget -background]"}
    catch {set font "-font [$path cget -font]"}
    return "$fg $bg $font"
}


proc tkDestroyIMStatusArea {path} {
    global tkImPriv
    set sTop ""
    catch {set sTop $tkImPriv(imStatusTop:${path})}
    if {[string length $sTop] <= 0} {
	return
    }
    if {[winfo exists $sTop] == 1} {
	destroy $sTop
    }
    catch {unset tkImPriv(imStatusTop:${path})}
}


proc tkCreateIMStatusArea {path} {
    global tkImPriv
    set sTop ""
    catch {set sTop $tkImPriv(imStatusTop:${path})}
    if {[string length $sTop] > 0} {
	if {[winfo exists $sTop] == 1} {
	    return
	}
    } else {
	regsub -all {\.} $path {_} eName
	set sTop [toplevel ._imStatusArea_Of${eName} -bo 0 -highlightthickness 0]
	wm withdraw $sTop
	wm overrideredirect $sTop 1
	wm title $sTop ""
	wm geometry $sTop +0+0
	eval "label ${sTop}.l [tkGetPathOptionForIMStatusArea $path] -text {}"
	pack ${sTop}.l -padx 0 -pady 0 -expand false -fill none
	set tkImPriv(imStatusTop:${path}) $sTop
	set bTags [bindtags $path]
	if {[lsearch -exact $bTags "IMOnTheSpotTag"] < 0} {
	    append bTags " IMOnTheSpotTag"
	    bindtags $path $bTags
	}
	return $sTop
    }
}


proc tkMapIMStatusArea {path text {usesaved 0}} {
    global tkImPriv
    set sTop ""
    catch {set sTop $tkImPriv(imStatusTop:${path})}
    if {[string length $sTop] <= 0} {
	set sTop [tkCreateIMStatusArea $path]
    }
    if {[string compare {[--]} $text] == 0 ||
	[string length $text] <= 0} {
	set oText [${sTop}.l cget -text]
	if {[string length $oText] <= 0} {
	    wm withdraw ${sTop}
	    ${sTop}.l configure -text ""
	    return
	}
    }
    eval "${sTop}.l configure [tkGetPathOptionForIMStatusArea $path] -text {$text} -border 1 -rel raised"
    update idletasks
    wm geom ${sTop} [tkGetIMStatusGeometry $path]
    if {![winfo ismapped ${sTop}]} {
	wm deiconify ${sTop}
	raise ${sTop} $path
    }
}


proc tkUnmapIMStatusArea {path} {
    global tkImPriv
    set sTop ""
    catch {set sTop $tkImPriv(imStatusTop:${path})}
    if {[string length $sTop] <= 0} {
	return
    }
    if {[winfo ismapped $sTop]} {
	wm withdraw $sTop
    } else {
	${sTop}.l configure -text ""
    }
}


proc tkTextIMPreeditStart {path} {
    global tkImPriv
    set tkImPriv(imPreedit:${path}:first) [$path index insert]
    set tkImPriv(imPreedit:${path}:max) 0
}


proc tkTextIMPreeditDone {path} {
    global tkImPriv
    set max 0
    catch {set max $tkImPriv(imPreedit:${path}:max)}
    for {set i 0} {$i < $max} {incr i} {
	lappend delTags imTag:${i}
    }
    catch {eval "$path tag delete $delTags"}
    catch {unset tkImPriv(imPreedit:${path}:first)}
    catch {unset tkImPriv(imPreedit:${path}:max)}
}


proc tkTextInsertIMText {path imText} {
    global tkImPriv
    set orgPos ""
    catch {set orgPos $tkImPriv(imPreedit:${path}:first)}
    if {[string length $orgPos] <= 0} {
	tkTextIMPreeditStart $path
	catch {set orgPos $tkImPriv(imPreedit:${path}:first)}
	if {[string length $orgPos] <= 0} {
	    return
	}
    }
    if {[string length $imText] <= 0} {
	return
    }

    set caret [lindex $imText 0]
    set start [lindex $imText 1]
    set len [lindex $imText 2]
    set sLen [lindex $imText 3]
    set str [lindex $imText 4]

    set sIdx "$orgPos + $start char"

    $path delete $sIdx "$sIdx + $len char"
    $path insert $sIdx $str
    $path mark set insert "$orgPos + $caret char"

    set max $tkImPriv(imPreedit:${path}:max)
    incr max [expr -${len} +${sLen}]
    if {$max > $tkImPriv(imPreedit:${path}:max)} {
	set tkImPriv(imPreedit:${path}:max) $max
    }
    if {$sLen <= 0} {
	if {$caret == 0 && $start == 0} {
	    tkTextIMPreeditDone $path
	}
	return
    }

    regsub -all {Primary} [lindex $imText 5] Reverse mode
    regsub -all {Secondary} $mode Underline mode
    regsub -all {Tertiary} $mode Highlight mode

    set oFg [$path cget -foreground]
    set oBg [$path cget -background]
    set pos $start
    foreach i $mode {
	set fg $oFg
	set bg $oBg
	set ul "false"
	set stipl ""

	set tagName imTag:$pos
	$path tag add $tagName "$orgPos + $pos char"
	foreach j $i {
	    switch $j {
		"Reverse" {
		    set tmp $fg
		    set fg $bg
		    set bg $tmp
		}
		"Underline" {
		    set ul "true"
		}
		"Highlight" {
		    set stipl gray75
		}
	    }
	}
	$path tag configure $tagName -foreground $fg -background $bg -underline $ul -fgstipple $stipl
	incr pos
    }
}


proc tkTextMoveIMCaret {path pos cmd} {
    global tkImPriv
    catch {set orgPos $tkImPriv(imPreedit:${path}:first)}
    if {[string length $orgPos] <= 0} {
	tkTextIMPreeditStart $path
	catch {set orgPos $tkImPriv(imPreedit:${path}:first)}
	if {[string length $orgPos] <= 0} {
	    return
	}
    }
    # Only AbsolutePosition is supported rite now.

    set cIdx ""
    switch $cmd {
	"AbsolutePosition" {
	    set cIdx [$path index "$orgPos + $pos char"]
	}
    }
    if {[string length $cIdx] > 0} {
	$path mark set insert $cIdx
    }
}


proc tkGetIMCallbackProc {path} {
    global tkImPriv
    set ret ""
    catch {set ret $tkImPriv(imOnTheSpotCallback:$path)}
    if {[string length $ret] > 0} {
	return $ret
    } else {
	set class [winfo class $path]
	catch {set ret $tkImPriv(imOnTheSpotCallback:$class)}
	if {[string length $ret] > 0} {
	    return $ret
	}
    }
    return tkDummyIMCallbackProc
}


proc tkDummyIMCallbackProc {path class cmd {args ""}} {
    global tkImPriv
    if {$tkImPriv(debug) == 1} {
	puts stderr "tkDummyIMCallbackProc $path $class $cmd '$args'"
    }
    if {[string compare $cmd "PreeditStart"] == 0} {
	return -1
    }
    return ""
}


proc tkTextIMCallbackProc {path class cmd {args ""}} {
    global tkImPriv
    if {$tkImPriv(debug) == 1} {
	puts stderr "tkTextIMCallbackProc $path $class $cmd '$args'"
    }
    switch $cmd {
	"PreeditStart" {
	    return -1
	}
	"PreeditDone" {
	    tkTextIMPreeditDone $path
	}
	"PreeditDraw" {
	    tkTextInsertIMText $path $args
	}
	"PreeditCaret" {
	    tkTextMoveIMCaret $path [lindex $args 0] [lindex $args 1]
	}

	"StatusStart" {
	    tkMapIMStatusArea $path "" 1
	}
	"StatusDone" {
	    tkUnmapIMStatusArea $path
	}
	"StatusDraw" {
	    switch [lindex $args 0] {
		"text" {
		    set str [lindex $args 2]
		    if {[string length $str] <= 0 ||
			[llength $str] <= 0} {
			tkUnmapIMStatusArea $path
		    } else {
			tkMapIMStatusArea $path $str
		    }
		}
		"bitmap" {
		    tkMapIMStatusArea $path [lindex $args 2]
		}
	    }
	}
    }
}


# im bind.
bind imConfig <Configure> {
    global tkImPriv
    if {[string compare [focus] "%W"] == 0 &&
	[catch {set tkImPriv(useOnTheSpot:%W)}]} {
	tkConfigIm %W 1
    }
}
bind imConfig <Destroy> {
    global tkImPriv
    catch {unset tkImPriv(haveConfigHandler:%W)}
    catch {unset tkImPriv(imOnTheSpotCallback:%W)}
    catch {unset tkImPriv(useOnTheSpot:%W)}
    catch {unset tkImPriv(imPreferredStyles:%W)}
    catch {unset tkImPriv(imSetupDeleted:%W)}
}

proc tkAddConfigImHandler {path} {
    set tags [bindtags $path]
    if {[lsearch $tags imConfig] < 0} {
	bindtags $path "$tags imConfig"
    }
}

proc tkDeleteConfigImHandler {path} {
    set tags [bindtags $path]
    if {[lsearch $tags imConfig] >= 0} {
	regsub -all imConfig $tags "" tags
	bindtags $path $tags
    }
}


proc tkInitializeIm {path} {
    global tkImPriv
    if {[catch {set tkImPriv(useOnTheSpot:$path)}] == 0} {
	return
    }
    if {[tkConfigIm $path] == 1} {
	set tkImPriv(useOnTheSpot:$path) 1
    }
    if {[catch {set tkImPriv(haveConfigHandler:$path)}]} {
	set tkImPriv(haveConfigHandler:$path) 1
	tkAddConfigImHandler $path
    }
}


proc tkSetupIm {tags {style ""} {otsCallback ""}} {
    set tags [string trimleft $tags]
    set tags [string trimright $tags]
    global tkImPriv
    if {[string length $otsCallback] > 0} {
	set tkImPriv(imOnTheSpotCallback:$tags) $otsCallback
    }
    if {[string length $style] > 0} {
	set tkImPriv(imPreferredStyles:$tags) $style
    }
    set needBind 1
    set bTags ""
    if {[string compare [string index $tags 0] "."] == 0} {
	if {[winfo exists $tags]} {
	    set bTags [bindtags $tags]
	} else {
	    set bTags $tags
	}
    } else {
	set bTags $tags
    }
    foreach i $bTags {
	set focusCmd [bind $i <FocusIn>]
	if {[string length $focusCmd] > 0} {
	    if {[string first tkInitializeIm $focusCmd] >= 0} {
		set needBind 0
		break
	    }
	}
    }
    if {$needBind == 0} {
	return
    }

    bind $tags <FocusIn> {tkInitializeIm %W}
}


proc tkCleanIm {tags} {
    global tkImPriv
    catch {unset tkImPriv(haveConfigHandler:%W)}
    catch {unset tkImPriv(imOnTheSpotCallback:%W)}
    catch {unset tkImPriv(useOnTheSpot:%W)}
    catch {unset tkImPriv(imPreferredStyles:%W)}
    tkDeleteConfigImHandler $tags

    set isPath 0
    set bTags ""
    if {[string compare [string index $tags 0] "."] == 0} {
	if {[winfo exists $tags]} {
	    set isPath 1
	    set bTags [bindtags $tags]
	} else {
	    set bTags $tags
	}
    } else {
	set bTags $tags
    }
    foreach i $bTags {
	set focusCmd [bind $i <FocusIn>]
	if {[string length $focusCmd] > 0} {
	    if {[string first tkInitializeIm $focusCmd] >= 0} {
		bind $i <FocusIn> {}
	    }
	}
    }
    
    if {$isPath == 1} {
	set tkImPriv(imSetupDeleted:$tags) 1
    }
}


foreach i "Text Entry" {
    tkSetupIm $i "" tkTextIMCallbackProc
}
