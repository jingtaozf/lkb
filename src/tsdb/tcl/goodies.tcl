#
# [incr tsdb()] --- Competence and Performance Profiling Environment
# Copyright (c) 1996 -- 2005 Oliver Plaehn (plaehn@coli.uni-sb.de)
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 2.1 of the License, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
# 

##
## =========================
##   A Simple Table Viewer
## =========================
##
## File:    goodies.tcl
##
## Author:  Oliver Plaehn (plaehn@coli.uni-sb.de)
##
## Purpose: Defines auxiliary procedures for 'showtable'.
##

#-------------------------------------------------------------------------------------
# Usage: wfsl List CharPos [LeftBracket] [RightBracket]
#
# Returns the well-formed sublist of 'List' that contains the character at position
# 'CharPos', when 'List' is considered as an ordinary string (where the numbering
# starts at 0).  'List' might contain sublists of arbitrary depth.  'LeftBracket' and
# 'RightBracket' are the opening and closing brackets used in 'List' and default to "("
# and ")", resp.  If 'List' is not itself a well-formed list or any other error is
# encountered during processing, the empty string is returned.
#-------------------------------------------------------------------------------------

proc wfsl {List CharPos {LeftBracket "("} {RightBracket ")"}} {
    
    ## Get character at specified position
    set Char [string index $List $CharPos]
    if { $Char == "" } {
	return ""
    }
    
    ## If that character is a left bracket, get position of matching right bracket; if
    ## it is a right bracket, compute position of matching left bracket; otherwise get
    ## position of first left bracket to the left of 'CharPos' and then the position of 
    ## its matching right bracket
    if { "$Char" == "$LeftBracket" } {
	set leftpos  $CharPos
	set rightpos [_wfsl_get_matching_bracket $List $leftpos +1 $LeftBracket $RightBracket]
    } elseif { "$Char" == "$RightBracket" } {
	set rightpos $CharPos
	set leftpos  [_wfsl_get_matching_bracket $List $rightpos -1 $LeftBracket $RightBracket]
    } else {
	set leftpos  [_wfsl_get_matching_bracket $List $CharPos -1 $LeftBracket $RightBracket]
	if { $leftpos > -1 } {
	    set rightpos [_wfsl_get_matching_bracket $List $leftpos +1 $LeftBracket $RightBracket]
	}
    }

    ## Return the empty string, if an error has been encountered, or otherwise the
    ## computed substring
    if { $leftpos == -1 || $rightpos == -1 } {
	return ""
    } else {
	return [string range $List $leftpos $rightpos]
    }
}

proc _wfsl_get_matching_bracket {List StartPos DirIncr {LeftBracket "("} {RightBracket ")"} } {

    set pos $StartPos
    set char [string index $List $pos]
    if { "$char" == "$LeftBracket" || "$char" == "$RightBracket" } {
	set bracketlevel 0
    } else {
	set bracketlevel $DirIncr
    }
    while { $char != "" } {
	if { "$char" == "$LeftBracket" } {
	    incr bracketlevel
	} elseif { "$char" == "$RightBracket" } {
	    decr bracketlevel
	}
	if { $bracketlevel == 0 } {
	    return $pos
	}
	incr pos $DirIncr
	set char [string index $List $pos]
    }
    return -1
}


#-------------------------------------------------------------------------------------
# Aufruf: min A B
# bzw.  : max A B
#
# Gibt das Minimum bzw. Maximum der beiden Werte A und B zurück.
#-------------------------------------------------------------------------------------

proc min {A B} { if {$A < $B} {return $A} else {return $B} }
proc max {A B} { if {$A > $B} {return $A} else {return $B} }


#-------------------------------------------------------------------------------------
# Aufruf: odd N
# bzw.  : even N
#
# Gibt booleschen Wert zurück, ob N ungerade (bzw. gerade) ist
#-------------------------------------------------------------------------------------

proc odd  N { return [expr $N % 2] }
proc even N { return [expr 1 - $N % 2] } 


#-------------------------------------------------------------------------------------
# Aufruf: forI Var First Last Command
#
# Evaluiert Command mit Var == First, dann Var == First+1, usw. bis Var == Last
#-------------------------------------------------------------------------------------

proc forI {Var First Last Command} {
    global errorInfo errorCode
    upvar $Var v
    for {set v $First} {$v <= $Last} {incr v} {
        set code [catch {uplevel $Command} string]
        if {$code == 1} {
            return -code error -errorinfo $errorInfo -errorcode $errorCode $string
        } elseif {$code == 2} {
            return -code return $string
        } elseif {$code == 3} {
            return
        } elseif {$code > 4} {
            return -code $code $string
        }
    }
}


#-------------------------------------------------------------------------------------
# Aufruf: do Command while Test
#
# Evaluiert erst Command und dann Test. Solange Test einen Wert ungleich 0 liefert,
# wird Vorgang wiederholt.
#-------------------------------------------------------------------------------------

proc do {Command While Test} {
    global errorInfo errorCode

    if {$While != "while"} {
        error "Error in do: Syntax: \"do Command while Test\""
    }

    set code [catch {uplevel $Command} string]
    if {$code == 1} {
        return -code error -errorinfo $errorInfo -errorcode $errorCode $string
    } elseif {$code == 2} {
        return -code return $string
    } elseif {$code == 3} {
        return
    } elseif {$code > 4} {
        return -code $code $string
    }
    set code [catch {uplevel [list while $Test $Command]} string]
    if {$code == 1} {
        return -code error -errorinfo $errorInfo -errorcode $errorCode $string
    } elseif {$code == 2} {
        return -code return $string
    } elseif {$code == 3} {
        return
    } elseif {$code > 4} {
        return -code $code $string
    }
}


#-------------------------------------------------------------------------------------
# Aufruf: coordInGeometry X Y Geometry
#
# Testet, ob der Punkt (X,Y) in dem Bereich liegt, der durch 'Geometry' angegeben ist.
# 'Geometry' ist im Format 'widthxheight+x+y'.
#-------------------------------------------------------------------------------------

proc coordInGeometry {X Y Geometry} {
    scan $Geometry "%dx%d+%d+%d" width height xoffset yoffset
    
    if { $X>=$xoffset && $X<=$xoffset+$width && $Y>=$yoffset && $Y<=$yoffset+$height } {
        return 1
    } else {
        return 0
    }
}


#-------------------------------------------------------------------------------------
# Aufruf: completion ListOrArray TextVar [IgnoreCase] [IndexVar]
#
# Komplettiert den Text in 'TextVar' entsprechend den Einträgen in 'ListOrArray'.
# D.h. falls Text eindeutiger Anfang eines Elements aus 'ListOrArray' ist, wird dieses
# in 'TextVar' zurückgegeben, bei mehreren Treffern der übereinstimmende Text.
# 'ListOrArray' muß entweder eine Liste oder der Name eines gültigen Arrays sein.
# Wenn 'IndexVar' angegeben und ungleich der leeren Zeichenkette ist, wird -- falls 
# Übereinstimmungen vorhanden sind -- in der dadurch bezeichneten Variable der
# Index des ersten übereinstimmenden Elements in der Liste bzw. in dem Array
# zurückgegeben. 'IgnoreCase' ist boolescher Wert, ob Groß-/Kleinschreibung beim
# Vergleichen ignoriert werden soll oder nicht (default ist 0, d.h. "GROSS" und
# "Gross" sind verschieden.). Returnwert: Anzahl der Übereinstimmungen.
#-------------------------------------------------------------------------------------

proc completion {ListOrArray TextVar {IgnoreCase 0} {IndexVar ""}} {

    upvar $TextVar text

    ## Falls 'IndexVar' angegeben, zum Beschreiben vorbereiten

    if { $IndexVar != "" } {
        upvar $IndexVar firstindex
    }

    ## Groß-/Kleinschreibung ignorieren?
    
    if { $IgnoreCase } {
        set regexpswitch "-nocase"
    } else {
        set regexpswitch ""
    }

    ## Evtl. im Text enthaltene Sonderzeichen in bezug auf 'regexp' quoten

    set regexptext [_completion_quote $text]

    ## Noch keine Übereinstimmungen gefunden

    set found 0

    ## Schleifenrumpf

    set loopbody {
        if {[regexp [subst $regexpswitch] "^$regexptext" "$item"]} {
            incr found
            if { $found > 1 } {
                set i [string length $text]
                set end [min [string length "$match"] [string length "$item"]]
                while { $i < $end } {
                    if { [string index $match $i] == [string index $item $i] } {
                        incr i
                    } else {
                        break
                    }
                }
                set match [string range $match 0 [expr $i-1]]
            } else {
                set match $item
                set firstindex $idx
            }
        }
    }

    ## Array oder List?

    upvar $ListOrArray arr
    set idxlist [array names arr]
    if { $idxlist == "" } {             # Liste!
        set idx 0
        foreach item $ListOrArray {
            eval $loopbody
            incr idx
        }
    } else {                            # Array!
        foreach idx $idxlist {
            set item $arr($idx)
            eval $loopbody
        }
    }

    if {$found >= 1} {
        set text $match
    }

    return $found
}


proc _completion_quote Text {
    ## Geht 'Text' durch und quoted alle Zeichen mit einem Backslash.
    ## Rückgabewert ist der gequotete String.

    set new ""
    forI i 0 [expr [string length $Text] - 1] {
        set char [string index $Text $i]
        set append "\\$char"
        set new "${new}${append}"
    }

    return $new
}


#-------------------------------------------------------------------------------------
# Aufruf: decr Var [Value]
#
# Vermindert Variable 'Var' um 'Value' und gibt verminderten Wert zurück.
# Default-Wert für Value ist 1.
#-------------------------------------------------------------------------------------

proc decr {Var {Value 1}} {
    upvar $Var v
    set v [expr $v-$Value]
}


#-------------------------------------------------------------------------------------
# Aufruf: lrmdups List
#
# Entfernt aus 'List' alle doppelten Elemente, wobei die Liste sortiert wird, und gibt
# die so verarbeitete Liste zurück.
#-------------------------------------------------------------------------------------

proc lrmdups List {
    
    set l [lsort $List]
    
    set idx 1
    while { $idx < [llength $l] } {
        if { [lindex $l $idx] == [lindex $l [expr $idx - 1]] } {
            set l [lreplace $l $idx $idx]
        } else {
            incr idx
        }
    }

    return $l
}


#-------------------------------------------------------------------------------------
# Aufruf: trimzeroes String
#
# Entfernt aus einem numerischen 'String' evtl. vorhandene f"uhrende Nullen und gibt
# den so verarbeiteten String zurück. Ist 'String' nicht numerisch oder beginnt nicht
# mit einer Null, wird er unver"andert zur"uckgegeben.
#-------------------------------------------------------------------------------------

proc trimzeroes String {
    
    if { [regexp {^0[0-9]+$} $String] } {
	if { [regexp {^0+$} $String] } {
	    set String 0
	} else {
	    regsub {^0+} $String "" String
	}
    }

    return $String
}
