#!/coli/apps/bin/wish++ -f
# ==============================================================================
#
# Oliver Plaehn; plaehn@coli.uni-sb.de; April 1998
#   
# Modified version of Bryan M. Kramer's 'table.tcl':
#
# - Disambiguated the canvas configure options '-yscroll' and '-xscroll'
#   to '-yscrollcommand' and '-xscrollcommand' (proc new_table)
# - Used BLT's table widget for layout of canvas and scrollbars
#   (proc new_table)
# - Changed order of proc's and added comments to make the file more readable 
# - Decremented 'maxx' and 'maxy' variables in 'table_draw_rules_1' by one
#   pixel to fix length of rulers
# 
# ==============================================================================
# 
#    layout canvas items in rows and columns with optional lines
#
#   Version 1.2
#   Last Modified: Mon Jan  9 09:44:34 1995 by Bryan M. Kramer
#
#   Author: Bryan M. Kramer     (kramer@ai.toronto.edu)
#
#   Modifications and suggestions due to:
#	     Brian Grossman <brian@lance.colostate.edu>
#
#   To Do List:
#	    1) straddle
#
#
#
#   This has been implemented and tested using tcl7.3 and tk3.6.
#   It probably works with tk4.0.
#
#                 Copyright (c) 1995  University of Toronto
# ======================================================================
# Permission to use, copy, modify, and distribute this software and its
# documentation for any non-commercial purpose is hereby granted,
# provided that the above copyright notice appear in all copies and that
# both that the copyright notice and warranty disclaimer appear in
# supporting documentation, and that the names of the University of Toronto
# and Bryan M. Kramer not be used in advertising or publicity pertaining to
# distribution of the software without specific, written prior permission.
#
# The University of Toronto and Bryan M. Kramer disclaim all warranties with
# regard to this software, including all implied warranties of merchantability
# and fitness.  In no event shall the University of Toronto or Bryan M. Kramer
# be liable for any special, indirect or consequential
# damages or any damages whatsoever resulting from loss of use, data or
# profits, whether in an action of contract, negligence or other
# tortuous action, arising out of or in connection with the use or
# performance of this software.
#
#
#
#	USAGE example:
#
# 	set table [new_table]
# 	pack $table
# 	set canvas [table_canvas $table]
#
# 	table_col_defaults $table 5 1 5 grey
# 	table_row_defaults $table 5 1 5 grey
# 	table_col_info $table 1 5 3 5 black right
# 	table_row_info $table 1 5 3 5 black
#
# 	set i [$canvas create text 10 10 -text "item 1 1"]
# 	table_add $table $i 1 1
# 	set i [$canvas create text 10 10 -text "item 2 1"]
# 	table_add $table $i 2 1
# 	set i [$canvas create text 10 10 -text "item 2 2"]
# 	table_add $table $i 2 2
#
# 	table_layout $table
#
#   NOTE:
#	since canvases can take window items, this can be used to create
#	scrolling tables of buttons, entries etc.!
#

##
## Initialize counter for tables
##

if {! [info exists table_num]} {
    set table_num 101
}

###
### User commands
### -------------
###

proc new_table {{parent "."} args} {
    ##
    ## Usage: new_table [parent [-noscroll]]
    ## 
    ## Creates new table (ie. a frame containing a canvas and optional vertical
    ## and horizontal scrollbars). Returns identifier of the new table.
    ##
    ## parent      Parent window of the new table; defaults to "."
    ## -noscroll   If specified, skips making scrollbars and makes the canvas
    ##             big enough to show all items in it. 'table_fix_scroll' will
    ##             recalculate the canvas size instead of the scrollregion.
    ##
    global table_num
    global table_SB
    set table_SB 1
    foreach f $args {
	switch -- $f {
		-noscroll { set table_SB 0 }
		}
    	}
    if {$parent == "."} {
	set fm ".table$table_num"
    } else {
	set fm "$parent.table$table_num"
    }
    incr table_num
    frame $fm
    set cv [canvas $fm.c]
    table $fm 0,0 $cv -fill both

    if $table_SB {
	$cv configure -yscrollcommand "$fm.scrolly set"
	$cv configure -xscrollcommand "$fm.scrollx set"
	scrollbar $fm.scrolly -command "$cv yview"
	bind $fm.scrolly <FocusIn> { tk_focusContinue %W }
	scrollbar $fm.scrollx -command "$cv xview" -orient horiz
	bind $fm.scrollx <FocusIn> { tk_focusContinue %W }
	table $fm \
	    0,1 $fm.scrolly -fill y \
	    1,0 $fm.scrollx -fill x
	table configure $fm r1 c1 -resize none
    }

    return $fm
}

proc table_canvas {table} {
    ##
    ## Usage: table_canvas table
    ## 
    ## Returns the canvas on which 'table' is to be drawn. This canvas must be
    ## used for creating object that are to be contained in the table.
    ##
    ## table       The (sub)table identifier
    ##
    set canvas [table_subtable_canvas $table]
    if {$canvas == {}} {
	return "$table.c"
    } else {
	return $canvas
    }
}

proc new_subtable {tbl} {
    ##
    ## Usage: new_subtable tbl
    ## 
    ## Creates new subtable (ie. a table that will be layed out in one cell)
    ## of table 'tbl'. Returns the identifier of the new subtable. This subtable
    ## can be used in the same way as an ordinary table, ie. one can specify
    ## items, row and column formats, or subtables for this subtable. The
    ## subtable is layed out automatically when its parent is layed out.
    ##
    ## tbl         The parent table of the new subtable.
    ##
    upvar \#0 table_props$tbl props
    if {! [info exists props(subcount)]} { set props(subcount) 0 }
    incr props(subcount)
    set sub "sub$tbl$props(subcount)"
    table_subtables $tbl $sub
    table_origin $sub 0 0
    upvar \#0 table_props$sub subprops
    set subprops(subtablep) 1
    set subprops(canvas) [table_canvas $tbl]
    return $sub
}

proc table_add {table item row col} {
    ##
    ## Usage: table_add table item row col
    ## 
    ## Adds canvas item(s) or a subtable to a table.
    ##
    ## table       Table identifier as returned by 'new_[sub]table'
    ## item        ID of canvas item, tag of (a group of) canvas item(s), or
    ##             table identifier of subtable
    ## row, col    Insertion place of 'item' in 'table'; rows and columns start
    ##             with 1!
    ##
    upvar \#0 tables$table x
    set x($row,$col) $item
    if {[table_is_subtable $table]} {
	set cv [table_canvas $table]
	set tags [lindex [$cv itemconfigure $item -tags] 4]
	lappend tags $table
	$cv itemconfigure $item -tags $tags
    }
}

proc table_col_defaults {table {m1 0} {rule 0} {m2 0} {fill black} {just left}} {
    ##
    ## Usage: table_col_defaults table
    ##                      [margin1 [rule_width [margin2 [rule_colour [alignment]]]]]
    ## 
    ## Specifies the default column information.
    ##
    ## table       Table identifier as returned by 'new_[sub]table'
    ## margin1     Space between a column and the ruler to its right (in pixel);
    ##             default is 0
    ## rule_width  Width of the ruler in pixel; defaults to 0
    ## margin2     Space between ruler and the column to its right (in pixel);
    ##             default is 0
    ## rule_colour Colour of rulers; defaults to black
    ## alignment   Placement of cell items in the column: 'left', 'center', or
    ##             'right'; defaults to 'left'
    ##
    upvar \#0 table_col_def$table col_info
    set col_info(m1) $m1
    set col_info(m2) $m2
    set col_info(rule) $rule
    set col_info(fill) $fill
    set col_info(just) $just
}

proc table_row_defaults {table {m1 0} {rule 0} {m2 0} {fill black} {just top}} {
    ##
    ## Usage: table_col_defaults table
    ##                      [margin1 [rule_width [margin2 [rule_colour [alignment]]]]]
    ## 
    ## Specifies the default row information.
    ##
    ## table       Table identifier as returned by 'new_[sub]table'
    ## margin1     Space between a row and the ruler below it (in pixel);
    ##             default is 0
    ## rule_width  Width of the ruler in pixel; defaults to 0
    ## margin2     Space between ruler and the row below it (in pixel);
    ##             default is 0
    ## rule_colour Colour of rulers; defaults to black
    ## alignment   Placement of cell items in the row: 'top', 'center', or
    ##             'bottom'; defaults to 'top'
    ##
    upvar \#0 table_row_def$table row_info
    set row_info(m1) $m1
    set row_info(m2) $m2
    set row_info(rule) $rule
    set row_info(fill) $fill
    set row_info(just) $just
}

proc table_col_info {table i {m1 0} {rule 0} {m2 0} {fill black} {just left}} {
    ##
    ## Usage: table_col_defaults table column
    ##                      [margin1 [rule_width [margin2 [rule_colour [alignment]]]]]
    ## 
    ## Specifies column information for a specific column. This overrides the
    ## default column information. Ruler information is for column AFTER
    ## 'column'. Use 0 to specify leftmost ruler.
    ##
    ## table       Table identifier as returned by 'new_[sub]table'
    ## column      Number of the column the given information applies to.
    ## margin1     Space between a column and the ruler to its right (in pixel);
    ##             default is 0
    ## rule_width  Width of the ruler in pixel; defaults to 0
    ## margin2     Space between ruler and the column to its right (in pixel);
    ##             default is 0
    ## rule_colour Colour of rulers; defaults to black
    ## alignment   Placement of cell items in the column: 'left', 'center', or
    ##             'right'; defaults to 'left'
    ##
    upvar \#0 table_col$table col_info
    set col_info($i,m1) $m1
    set col_info($i,m2) $m2
    set col_info($i,rule) $rule
    set col_info($i,fill) $fill
    set col_info($i,just) $just
}

proc table_row_info {table i {m1 0} {rule 0} {m2 0} {fill black} {just top}} {
    ##
    ## Usage: table_col_defaults table row
    ##                      [margin1 [rule_width [margin2 [rule_colour [alignment]]]]]
    ## 
    ## Specifies row information for a specific row. This overrides the
    ## default row information. Ruler information is for row AFTER 'column'. Use 
    ## 0 to specify topmost ruler.
    ##
    ## table       Table identifier as returned by 'new_[sub]table'
    ## row         Number of the row the given information applies to.
    ## margin1     Space between a row and the ruler below it (in pixel);
    ##             default is 0
    ## rule_width  Width of the ruler in pixel; defaults to 0
    ## margin2     Space between ruler and the row below it (in pixel);
    ##             default is 0
    ## rule_colour Colour of rulers; defaults to black
    ## alignment   Placement of cell items in the row: 'top', 'center', or
    ##             'bottom'; defaults to 'top'
    ##
    upvar \#0 table_row$table row_info
    set row_info($i,m1) $m1
    set row_info($i,m2) $m2
    set row_info($i,rule) $rule
    set row_info($i,fill) $fill
    set row_info($i,just) $just
}

proc table_make_regions {table regions} {
    ##
    ## Usage: table_make_regions table regions
    ## 
    ## After a table has been layed out and the rulers have been drawn, this
    ## command might be used to specifiy regions. A region is a rectangular area 
    ## in the table over which a frame is drawn. The content of the leftmost,
    ## uppermost cell in the region is re-justified in this frame. This allows
    ## for cells to span multiple rows and/or columns.
    ##
    ## table       Table identifier as returned by 'new_[sub]table'
    ## regions     A list of region specifiers. Each element is a list of the
    ##             following format:
    ##             {rowNW colNW rowSE colSE [hor_justify [ver_justify]]}
    ##
    ##             rowNW,colNW    Upper left corner of region (incl.)
    ##             rowSE,colSE    Lower right corner of region (incl.)
    ##             hor_justify    Horizontal alignment of content of cell
    ##                            (rowNW,colNW); values: left, center, right;
    ##                            defaults to center
    ##             ver_justify    Vertical alignment; values: top, center,
    ##                            bottom; defaults to center
    ##

    if { [table_is_subtable $table] } {
	error "table_make_regions: not yet implemented for subtables!"
    }

    ## Set array names for column and row informations

    set row_info "table_row$table"
    set col_info "table_col$table"
    set row_defaults "table_row_def$table"
    set col_defaults "table_col_def$table"

    ## Make cell properties (computed by 'table_layout') and item array
    ## available 

    upvar \#0 table_cell_props$table cellp
    upvar \#0 tables$table a

    ## Get canvas of 'table'

    set cv [table_canvas $table]

    ## Make all regions

    foreach r $regions {

	## Get region information

	set rowNW [lindex $r 0]
	set colNW [lindex $r 1]
	set rowSE [lindex $r 2]
	set colSE [lindex $r 3]
	set hor_just [lindex $r 4]
	if { $hor_just == "" } { set hor_just center }
	set ver_just [lindex $r 5]
	if { $ver_just == "" } { set ver_just center }

	## Compute coords of region without margins
	
	set xNW $cellp($rowNW,$colNW,x)
	set yNW $cellp($rowNW,$colNW,y)
	set xSE [expr $cellp($rowSE,$colSE,x) + $cellp($rowSE,$colSE,w)]
	set ySE [expr $cellp($rowSE,$colSE,y) + $cellp($rowSE,$colSE,h)]

	## Compute coords WITH margins

	set xNWm [expr $xNW - [table_margin2 [expr $colNW - 1] $col_info $col_defaults]]
	set yNWm [expr $yNW - [table_margin2 [expr $rowNW - 1] $row_info $row_defaults]]
	set xSEm [expr $xSE + [table_margin1 $colSE $col_info $col_defaults] -1]
	set ySEm [expr $ySE + [table_margin1 $rowSE $row_info $row_defaults] -1]

	## Make rectangle the size of the region INCLUDING margins and place it
	## over region

	set bgcolor [$cv cget -background]
	$cv create rectangle $xNWm $yNWm $xSEm $ySEm -tag region \
	    -fill $bgcolor -outline $bgcolor
	
	## Rejustify item in upper left corner according to parameters (take
	## margins into account!)

	set item $a($rowNW,$colNW)
	set bbox [$cv bbox $item]
	set itemw [expr [lindex $bbox 2] - [lindex $bbox 0] + 1]
	set itemh [expr [lindex $bbox 3] - [lindex $bbox 1] + 1]

	set cellw [expr $xSE - $xNW + 1]
	set cellh [expr $ySE - $yNW + 1]

	switch $hor_just {
	    left    { set x $xNW }
	    right   { set x [expr $xSE - $itemw] }
	    center  { set x [expr $xNW + ($cellw - $itemw) / 2.0] }
	    default { error "table_make_regions: invalid parameter for horizontal offset!" }
	}
	switch $ver_just {
	    top     { set y $yNW }
	    bottom  { set y [expr $ySE - $itemh] }
	    center  { set y [expr $yNW + ($cellh - $itemh) / 2.0] }
	    default { error "table_make_regions: invalid parameter for vertical offset!" }
	}
	
	$cv move $item [expr $x - [lindex $bbox 0]] [expr $y - [lindex $bbox 1]]

	## Raise item

	$cv raise $item
    }
}

proc table_layout {table} {
    ##
    ## Usage: table_layout table
    ## 
    ## Moves the objects in the canvas so that rows and columns accomodate the
    ## largest elements, aligning the rest according to the alignment
    ## information given for the table.
    ##
    ## table       Table identifier as returned by 'new_[sub]table'
    ##
    table_layout_2 $table 1
}

proc table_fix_scroll {table} {
    ##
    ## Usage: table_fix_scroll table
    ## 
    ## Changes the scroll region of the canvas to accomodate all objects on the
    ## canvas. If the table has been created with the option -noscroll, the size 
    ## of the canvas is re-adjusted instead.
    ##
    ## table       Table identifier as returned by 'new_[sub]table'
    ##
    set cv [table_canvas $table]
    $cv configure -scrollregion [$cv bbox all]
    global table_SB
    if {!$table_SB} {
	set box [$cv bbox all]
	if {$box == ""} {puts stderr "bbox error in table_fix_scroll" }
	$cv configure -height [expr [lindex $box 3] - [lindex $box 1]]
	$cv configure -width [expr [lindex $box 2] - [lindex $box 0]]
	}
}

###
### Auxiliary commands
### ------------------
###

# compute sizes of rows and columns
# rn, rowvector, cn, colvector are reference parameters

proc table_sizes {table rn rowvector cn colvector} {
    upvar \#0 tables$table a
    upvar $rowvector row_h $rn rows
    upvar $colvector col_w $cn cols
    set rows 0
    set cols 0
    set cv [table_canvas $table]
    foreach key [array names a] {
	set l [split $key ,]
	set x [lindex $l 0]
	if {$x >= $rows} {set rows [expr $x + 1]}
	set y [lindex $l 1]
	if {$y >= $cols} {set cols [expr $y + 1]}
	set box [$cv bbox $a($key)]
	if {$box == ""} {continue}
	set width [expr [lindex $box 2] - [lindex $box 0]]
	set height [expr [lindex $box 3] - [lindex $box 1]]
	if [info exists row_h($x)] {
	    if {$row_h($x) < $height} {
		set row_h($x) $height
	    }
	} else {
	    set row_h($x) $height
	}
	if [info exists col_w($y)] {
	    if {$col_w($y) < $width} {
		set col_w($y) $width
	    }
	} else {
	    set col_w($y) $width
	}
    }
}

proc table_is_subtable {tbl} {
    upvar \#0 table_props$tbl props
    if {[info exists props(subtablep)]} {
	return $props(subtablep)
    } else {
	return 0
    }
}

proc table_subtable_canvas {tbl} {
    upvar \#0 table_props$tbl props
    if {[info exists props(canvas)]} {
	return $props(canvas)
    } else {
	return {}
    }
}

# note that layout changes the origins of subtables so no
# use in setting them yourself

proc table_origin {tbl args} {
    upvar \#0 table_props$tbl props

    if {$args == {}} {
	set xo 0
	set yo 0
	if {[info exists props(origin,x)]} { set xo $props(origin,x) }
	if {[info exists props(origin,y)]} { set yo $props(origin,y) }
	return [list $xo $yo]
    } else {
	set com [lindex $args 0]
	set len [llength $args]
	if {$len == 2} {
	    if {$com == "x"} {
		set props(origin,x) [lindex $args 1]
	    } elseif {$com == "y"} {
		set props(origin,y) [lindex $args 1]
	    } else {
		set props(origin,x) [lindex $args 0]
		set props(origin,y) [lindex $args 1]
		return [list $props(origin,x) $props(origin,y)]
	    }
	} elseif {$len == 1} {
	    if {$com == "x"} {
		if {[info exists props(origin,x)]} {
		    return $props(origin,x)
		} else {
		    return 0
		}
	    } elseif {$com == "y"} {
		if {[info exists props(origin,y)]} {
		    return $props(origin,y)
		} else {
		    return 0
		}
	    } else {
		error "Bad arg to table_origin"
	    }
	} else {
	    error "Bad arg to table_origin"
	}
    }
}

# if args, add them, else return subtables if any

proc table_subtables {tbl args} {
    upvar \#0 table_props$tbl props
    if {$args == {}} {
	if {[info exists props(subtables)]} {
	    return $props(subtables)
	} else {
	    return {}
	}
    } else {
	set com "lappend props(subtables) $args"
	eval $com
    }
}

proc table_margin1 {i infov defv} {
    upvar \#0 $infov info
    if {$i == 0} {
	return 0
    }
    if [info exists info($i,m1)] {
	return $info($i,m1)
    }
    upvar \#0 $defv info
    if [info exists info(m1)] {
	return $info(m1)
    }
    return 0
}

proc table_margin2 {i infov defv} {
    upvar \#0 $infov info
    if [info exists info($i,m2)] {
	return $info($i,m2)
    }
    upvar \#0 $defv info
    if [info exists info(m2)] {
	return $info(m2)
    }
    return 0
}

proc table_rule {i infov defv} {
    upvar \#0 $infov info
    if [info exists info($i,rule)] {
	return $info($i,rule)
    }
    upvar \#0 $defv info
    if [info exists info(rule)] {
	return $info(rule)
    }
    return 0
}

proc table_fill {i infov defv} {
    upvar \#0 $infov info
    if [info exists info($i,fill)] {
	return $info($i,fill)
    }
    upvar \#0 $defv info
    if [info exists info(fill)] {
	return $info(fill)
    }
    return black
}

proc table_just {i infov defv} {
    upvar \#0 $infov info
    if [info exists info($i,just)] {
	return $info($i,just)
    }
    upvar \#0 $defv info
    if [info exists info(just)] {
	return $info(just)
    }
    return left
}

proc table_margin_width {i info def} {
    return [expr [table_margin1 $i $info $def] + [table_margin2 $i $info $def] + [table_rule $i $info $def]]
}

proc table_draw_rules_1 {table maxx maxy rows rowv cols colv row_info row_defaults col_info col_defaults} {
    upvar $rowv row_h $colv col_w
    set cv [table_canvas $table]

    set xorigin [table_origin $table x]
    set yorigin [table_origin $table y]

    set maxx [expr $xorigin + $maxx - [table_margin2 [expr $rows - 1] $row_info $row_defaults] -1]
    set maxy [expr $yorigin + $maxy - [table_margin2 [expr $cols - 1] $col_info $col_defaults] -1]

    set x $xorigin
    for {set j 0} {$j < $cols} {} {
	set x [expr $x + [table_margin1 $j $col_info $col_defaults]]
	set w [table_rule $j $col_info $col_defaults]
	if {$w == 1} {
	    set fill [table_fill $j $col_info $col_defaults]
	    $cv create line $x $yorigin $x $maxy -tags table_rules -fill $fill -width $w
	} elseif {$w > 0} {
	    set fill [table_fill $j $col_info $col_defaults]
	    $cv create rectangle $x $yorigin [expr $x + $w - 1] $maxy -tags table_rules -fill $fill -outline $fill
	}
	set x [expr $x + $w + [table_margin2 $j $col_info $col_defaults]]
	incr j
	if [info exists col_w($j)] {
	    set x [expr $x + $col_w($j)]
	}
    }
    set y $yorigin
    for {set j 0} {$j < $rows} {} {
	set y [expr $y + [table_margin1 $j $row_info $row_defaults]]
	set h [table_rule $j $row_info $row_defaults]
	if {$h == 1} {
	    set fill [table_fill $j $row_info $row_defaults]
	    $cv create line $xorigin $y $maxx $y -tags table_rules -fill $fill -width $h

	} elseif {$h > 0} {
	    set fill [table_fill $j $row_info $row_defaults]
	    $cv create rectangle $xorigin $y $maxx [expr $y + $h - 1] -tags table_rules -fill $fill -outline $fill
	}
	set y [expr $y + $h + [table_margin2 $j $row_info $row_defaults]]
	incr j
	if [info exists row_h($j)] {
	    set y [expr $y + $row_h($j)]
	}
    }
}

proc table_size_value {index vector} {
    upvar $vector a
    if {[info exists a($index)]} {
	return $a($index)
    } else {
	return 0
    }
}

proc table_draw_rules_subs {master} {
    set k 0
    foreach table [table_subtables $master] {
	incr k
	set row_info "table_row$table"
	set col_info "table_col$table"
	set row_defaults "table_row_def$table"
	set col_defaults "table_col_def$table"
	set row_h "row_h$k"
	set col_w "col_w$k"
	table_sizes $table rows $row_h cols $col_w

	set maxx 0
	for {set j 0} {$j < $cols} {} {
	    set maxx [expr $maxx + [table_margin_width $j $col_info $col_defaults]]
	    incr j
	    set maxx [expr $maxx + [table_size_value $j $col_w]]
	}
	set maxy 0
	for {set j 0} {$j < $rows} {} {
	    set maxy [expr $maxy + [table_margin_width $j $row_info $row_defaults]]
	    incr j
	    set maxy [expr $maxy + [table_size_value $j $row_h]]
	}
	table_draw_rules_1 $table $maxx $maxy $rows $row_h $cols $col_w $row_info \
	    $row_defaults $col_info $col_defaults

	table_draw_rules_subs $table
    }
}

proc table_draw_rules {table maxx maxy rows rowv cols colv} {
    set cv [table_canvas $table]
    $cv delete table_rules
    table_draw_rules_subs $table
    set row_info "table_row$table"
    set col_info "table_col$table"
    set row_defaults "table_row_def$table"
    set col_defaults "table_col_def$table"
    upvar $rowv row_h $colv col_w
    table_draw_rules_1 $table $maxx $maxy $rows row_h $cols col_w $row_info \
	$row_defaults $col_info $col_defaults
}

proc table_just_x {box j colw col_info col_defaults} {
    set just [table_just $j $col_info $col_defaults]
    if {$just == "left"} {
	return 0
    } elseif {$just == "right"} {
	set w [expr [lindex $box 2] - [lindex $box 0]]
	# must be greater than 0 by construction of column widths
	return [expr $colw - $w]
    } else {
	set w [expr [lindex $box 2] - [lindex $box 0]]
	return [expr ($colw -$w) / 2.0]
    }
}

proc table_just_y {box j rowh row_info row_defaults} {
    set just [table_just $j $row_info $row_defaults]
    if {$just == "top" || $just == "left"} {
	return 0
    } elseif {$just == "bottom" || $just == "right"} {
	set w [expr [lindex $box 3] - [lindex $box 1]]
	# must be greater than 0 by construction of row widths
	return [expr $rowh - $w]
    } else {
	set w [expr [lindex $box 3] - [lindex $box 1]]
	return [expr ($rowh -$w) / 2.0]
    }
}

proc table_layout_1  {table master} {
    upvar \#0 tables$table a
    upvar \#0 table_cell_props$table cell
    set row_info "table_row$table"
    set col_info "table_col$table"
    set row_defaults "table_row_def$table"
    set col_defaults "table_col_def$table"

    table_sizes $table rows row_h cols col_w
    set cv [table_canvas $table]

    set xorigin [table_origin $table x]
    set yorigin [table_origin $table y]

    set x [expr $xorigin + [table_margin_width 0 $col_info $col_defaults]]

    for {set j 1} {$j < $cols} {incr j} {
	set y [expr $yorigin + [table_margin_width 0 $row_info $row_defaults]]
	if [info exists col_w($j)] {
	    set w $col_w($j)
	} else {
	    set w 0
	}

	for {set i 1} {$i < $rows} {incr i} {
	    if [info exists a($i,$j)] {
		set item $a($i,$j)
		set h $row_h($i)
		set cell($i,$j,x) $x
		set cell($i,$j,y) $y
		set cell($i,$j,w) $w
		set cell($i,$j,h) $h
		set box [$cv bbox $item]
		set xx [expr $x + [table_just_x $box $j $w $col_info $col_defaults]]
		set yy [expr $y + [table_just_y $box $i $h $row_info $row_defaults]]
		if {[table_is_subtable $item]} {
		    table_move $cv $item $xx $yy
		} else {
		    $cv move $item [expr $xx - [lindex $box 0]] [expr $yy - [lindex $box 1]]
		}
	    }
	    if [info exists row_h($i)] {
		set y [expr $y + $row_h($i)]
	    }
	    set y [expr $y + [table_margin_width $i $row_info $row_defaults]]
	}

	if [info exists col_w($j)] {
	    set x [expr $x + $col_w($j)]
	}
	set x [expr $x + [table_margin_width $j $col_info $col_defaults]]
    }
    if {$master} {
	table_draw_rules $table $x $y $rows row_h $cols col_w
    }
}

proc table_layout_2 {table master} {
    foreach sub [table_subtables $table] {
	table_layout_2 $sub 0
    }
    table_layout_1 $table $master
}

proc table_move {cv table x y} {
    upvar \#0 tables$table a

    table_sizes $table rows row_h cols col_w
    table_origin $table $x $y
    set box1 [$cv bbox $table]
    set xoffset [expr $x - [lindex $box1 0]]
    set yoffset [expr $y - [lindex $box1 1]]

    for {set j 1} {$j < $cols} {incr j} {
	for {set i 1} {$i < $rows} {incr i} {
	    if [info exists a($i,$j)] {
		set item $a($i,$j)
		if {[table_is_subtable $item]} {
		    set box [$cv bbox $item]
		    set xx [expr $x + [lindex $box 0] - [lindex $box1 0]]
		    set yy [expr $y + [lindex $box 1] - [lindex $box1 1]]
		    table_move $cv $item $xx $yy
		} else {
		    $cv move $item $xoffset $yoffset
		}
	    }
	}
    }
}


##
## Description of global variables:
## --------------------------------
## 
## table_num                 Number of next table that will be created; starts
##                           with 101. The table identifier created by
##                           'new_table' is '.table$table_num' or
##                           '$parent.table$table_num' if 'parent' is specified
##                           
## table_SB                  Flag whether the last table was created with option
##                           -noscroll. This should be an ARRAY with table
##                           identifier as index instead of a global flag. This
##                           way only the -noscroll option of the last table
##                           is stored.
##
## tables$tableid()          Array containing the item/tag/subtableID
##                           information for the cells of 'tableid'; index is
##                           'row,col'
##                           
## table_props$tableid()     Array containing information about tables; indices:
##                           subcount       Counter for number of subtables
##                           subtables      List of identifiers of subtables
##                                          contained in 'tableid'
##                           subtablep      Flag if 'tableid' is subtable
##                           canvas         Canvas on which 'tableid' is displayed
##                           origin,x       Coordinates of upper left corner
##                           origin,y       of 'tableid'
##
## table_col_def$tableid()   Array containing information about the default
##                           layout of columns; indices:
##                           m1    Margin to the left of ruler (in pixel)
##                           m2    Margin to the right of ruler (in pixel)
##                           rule  Width of the ruler (in pixel)
##                           fill  Colour of ruler
##                           just  Alignment of columns (left, center, right)
##                           
## table_row_def$tableid()   Array containing information about the default
##                           layout of rows; indices:
##                           m1    Margin above the ruler (in pixel)
##                           m2    Margin below the ruler (in pixel)
##                           rule  Width of the ruler (in pixel)
##                           fill  Colour of ruler
##                           just  Alignment of rows (top, center, bottom)
##                           
## table_col$tableid()       Array containing information about column layout;
##                           indices:
##                           $colno,m1
##                           $colno,m2
##                           $colno,rule
##                           $colno,fill
##                           $colno,just   Same as above, but for column 'colno'
##
## table_row$tableid()       Array containing information about row layout;
##                           indices:
##                           $rowno,m1
##                           $rowno,m2
##                           $rowno,rule
##                           $rowno,fill
##                           $rowno,just   Same as above, but for row 'rowno'
##
## table_cell_props$table()  Array containing cell informations; indices:
##                           row,col,x  X coord of upper left corner of cell (in canvas coords)
##                           row,col,y  Y coord -"-
##                           row,col,w  Width of cell
##                           row,col h  Height of cell
## 

#Local Variables:
#mode: tcl
#End:
