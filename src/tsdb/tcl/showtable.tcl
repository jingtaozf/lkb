#
## =========================
##   A Simple Table Viewer
## =========================
##
## File:    showtable.tcl
##
## Author:  Oliver Plaehn (plaehn@coli.uni-sb.de)
##
## Purpose: Defines procedures for 'showtable'.
##

proc showtable {filename {container ".table"} {database "unknown"} {title ""}} {

## Usage:   showtable <tablefilename>
##          
## Input:   <tablefilename>
##
##              Contains the description of the table to be displayed. The
##              format is as follows (see 'test.tbl' for an example).
##
##              Lines are separated by newline characters. Empty lines
##              (ie. lines consisting exclusively of spaces and tabs or a single
##              newline character) and lines starting with a '#' (comments) are
##              ignored. All other lines must start with one of the commands
##              described below, followed by its obligatory arguments (if any)
##              and zero or more option/value pairs (eg. '-font times'). Order
##              of the arguments is relevant, order of the option/value pairs is
##              not. The file might contain commands (ie. lines) in any order.
##
##              Commands:
##
##                format <name> [options]
##
##                Specifies a new format '<name>' to be used in the 'cell' and
##                'region' commands. Possible options are those applicable to
##                Tk canvas text items, eg. '-font', '-fill', '-justify', etc.
##
##                layout {col | row} {def | <num>} [options]
##
##                Specifies row (first argument is 'row') or column (first
##                argument is 'col') layout. If the second argument is 'def',
##                the default row/column layout is specfied by the succeeding
##                options. Otherwise the second argument must contain a number
##                of a row/column, for which the layout is specified by the
##                options.
##                Valid options are: '-m1', '-m2', '-r', '-c', and '-j'.
##                '-m1' and '-m2' specify the margin to the left and right 
##                of the ruler of the column in pixels (or, in the case of rows,
##                the margin above and below the ruler). The ruler of a column
##                is the one to the right of it, the ruler of a row is the one
##                below it. To specify the topmost/leftmost ruler, use 'layout
##                row 0 ...' or 'layout col 0 ...', resp.
##                '-r' specifies the width of the ruler in pixels, '-f' its
##                color. '-j' specifies the justification of cell elements in
##                the row/column. Valid values are 'left', 'right', and 'center'
##                for columns, 'top', 'bottom', and 'center' for rows.
##                
##                cell <rowno> <colno> [options]
##
##                Specifies the cell in row '<rowno>' and column '<colno>'.
##                Numbering of rows and columns starts with 1. Valid options are
##                '-contents', '-format', and all options applicable for canvas
##                text items. '-contents' specifies the text to be displayed in
##                the cell. If the text contains spaces and/or tabs, it should
##                be quoted (eg. '-contents "cell text"'). Line breaks can be
##                inserted as '\n' and special characters as '\<octnum>', where
##                <octnum> is an octal number with 3 digits.
##                (Eg. '-contents "total\n\330"'.)
##                '-format' must be followed by a name of a format that is
##                specified elsewhere in the file with the 'format' command (see
##                above). Any other options are passed to the canvas text item
##                and override the corresponding options in the chosen
##                format. (Eg. 'cell 1 1 -fill red -format title' displays the
##                upper left cell with red text color, even if the format
##                'title' specifies the color as, say, blue.)
##
##                region <rowNW> <colNW> <rowSE> <colSE> [options]
##
##                Specifies a region in the table, ie. a cell that spans more
##                than one row and/or column. <rowNW> and <colNW> are the row
##                and column number of the upper left corner, <rowSE> and
##                <colSE> the row and column number of the lower right corner of
##                the region. Valid options are those of the 'cell' command plus
##                '-hor_justify' and '-ver_justify'. '-hor_justify' specifies
##                the horizontal justification of the text (as defined by the
##                '-contents' option) in the defined region; valid values are
##                'left', 'center', or 'right'. '-ver_justify' accordingly
##                specifies the vertical justification as either 'top',
##                'center', or 'bottom'.
##
## Caveats: The format of the input file is supposed to be correct and is only
##          partially checked. An incorrect file format might therefore result
##          in unpredictable program behavior.
##

  global globals;

  ## Constants

  set minWindowWidth 100; 		# Minimum width of toplevel window
  set minWindowHeight 100;		# Minimum height of toplevel window

  #
  # compute window title if not passed as `title' argument
  #

  if {$title == ""} {
      set title [format "tsdb(1) analysis: `%s'" $database]
  }

  ## Open file

  if { [catch {set file [open $filename r]}] != 0 } {
      error "Couldn't open table file '$filename'!"
  }

  ## Make toplevel window

  set toplevel [toplevel $container]
  set t [new_table $toplevel]
  set c [table_canvas $t]
  set maxWindowWidth [expr int([winfo screenwidth $toplevel] * 0.95)];
  set maxWindowHeight [expr int([winfo screenheight $toplevel] * 0.95)];

  button $toplevel.quit -text "Close" \
          -command [list "tsdb_close" $toplevel]
  button $toplevel.latex -text "LaTeX" \
          -command [list "tsdb_latex" $toplevel]
  button $toplevel.print -text "PostScript" \
          -command [list make_postscript $c $database]

  table $toplevel \
      0,0 $t -cspan 3 -fill both -padx 5 -pady 5\
      1,0 $toplevel.quit -pady {5 10} \
      1,1 $toplevel.latex -pady {5 10} \
      1,2 $toplevel.print -pady {5 10}
  table configure $toplevel r1 -resize none
  $c configure -relief flat -bg white -bd 3


  wm title $toplevel $title
  wm iconname $toplevel $title

  ## Read table from file and display it

  read_table_file $file
  make_table $t
  table_layout $t
  make_regions $t

  #
  # add copyright caption
  #
  set bbox [$c bbox all];
  set x [expr [lindex $bbox 2] - 1];
  set y [expr [lindex $bbox 3] + 1];
  set caption "(generated by $globals(name) at [current-time 1] - \
               (c) oe@coli.uni-sb.de)";
  $c create text $x $y -anchor ne -text $caption \
    -font $globals(copyright_font);

  table_fix_scroll $t
  $c xview moveto 0
  $c yview moveto 0

  ## Display toplevel

  compute_requested_geometry $t width height
  if { $width < $minWindowWidth } {
      set width $minWindowWidth
  } elseif { $width > $maxWindowWidth } {
      set width $maxWindowWidth
  }
  if { $height < $minWindowHeight } {
      set height $minWindowHeight
  } elseif { $height > $maxWindowHeight } {
      set height $maxWindowHeight
  }
  wm geometry $toplevel "${width}x${height}"
#  wm minsize $toplevel $minWindowWidth $minWindowHeight
#  wm maxsize $toplevel $maxWindowWidth $maxWindowHeight
  tkwait visibility $toplevel
  update idletasks
#  catch {file delete -force $filename};
  close $file;

  #
  # establish familiar key bindings for scrolling
  #
  #
  # for some weird reason the bindings have to go with the toplevel ??
  #
  catch {
    bind $toplevel <Prior> [list $c yview scroll -1 page];
    bind $toplevel <Next> [list $c yview scroll 1 page];
    bind $toplevel <BackSpace> [list $c yview scroll -1 page];
    bind $toplevel <Delete> [list $c yview scroll -1 page];
    bind $toplevel <space> [list $c yview scroll 1 page];
    bind $toplevel <Control-v> [list $c yview scroll 1 page];
    bind $toplevel <Control-V> [bind $toplevel <Control-v>];
    bind $toplevel <Alt-v> [list $c yview scroll -1 pages];
    bind $toplevel <Alt-V> [bind $toplevel <Alt-v>];
    bind $toplevel <Meta-v> [list $c yview scroll -1 pages];
    bind $toplevel <Meta-V> [bind $toplevel <Meta-v>];
    bind $toplevel {<Escape> <v>} [list $c yview scroll -1 pages];
    bind $toplevel <Return> [list $c yview scroll 1 units];
    bind $toplevel <Down> [list $c yview scroll 1 units];
    bind $toplevel <Control-n> [list $c yview scroll 1 units];
    bind $toplevel <Control-N> [bind $toplevel  <Control-n>];
    bind $toplevel <Up> [list $c yview scroll -1 units];
    bind $toplevel <Control-p> [list $c yview scroll -1 units];
    bind $toplevel <Control-P> [bind $toplevel  <Control-p>];
    bind $toplevel <Shift-Prior> [list $c yview moveto 0];
    bind $toplevel <Shift-Next> [list $c yview moveto 1];
    bind $toplevel <Control-Prior> [list $c yview moveto 0];
    bind $toplevel <Control-Next> [list $c yview moveto 1];
    bind $toplevel <Alt-Prior> [list $c yview moveto 0];
    bind $toplevel <Alt-Next> [list $c yview moveto 1];
    bind $toplevel <Meta-Prior> [list $c yview moveto 0];
    bind $toplevel <Meta-Next> [list $c yview moveto 1];

    bind $toplevel <Left> [list $c xview scroll -1 units];
    bind $toplevel <Right> [list $c xview scroll 1 units];
    bind $toplevel <Control-b> [list $c xview scroll -1 units];
    bind $toplevel <Control-B> [bind $toplevel  <Control-b>];
    bind $toplevel <Control-f> [list $c xview scroll 1 units];
    bind $toplevel <Control-F> [bind $toplevel  <Control-f>];
    bind $toplevel <Shift-Left> [list $c xview scroll -1 pages];
    bind $toplevel <Shift-Right> [list $c xview scroll 1 pages];
    bind $toplevel <Control-Left> [list $c xview scroll -1 pages];
    bind $toplevel <Control-Right> [list $c xview scroll 1 pages];
    bind $toplevel <Alt-Left> [list $c xview scroll -1 pages];
    bind $toplevel <Alt-Right> [list $c xview scroll 1 pages];
    bind $toplevel <Meta-Left> [list $c xview scroll -1 pages];
    bind $toplevel <Meta-Right> [list $c xview scroll 1 pages];
    bind $toplevel <Home> [list $c xview moveto 0];
    bind $toplevel <End> [list $c xview moveto 1];
    bind $toplevel <Control-a> [list $c xview moveto 0];
    bind $toplevel <Control-A> [bind $toplevel  <Control-a>];
    bind $toplevel <Control-e> [list $c xview moveto 1];
    bind $toplevel <Control-E> [bind $toplevel  <Control-e>];
    bind $toplevel <q> [list "tsdb_close" $toplevel];
    bind $toplevel <Q> [bind $toplevel <q>];
  }; # catch

  bind $c <Button> [list canvas_select $c %x %y %b];
  bind $c <Enter> {
    balloon post "<Button-1> selects cell element; \
                  <Double-Button-1> processes test item interactively";
  }; # bind
    bind $c <Leave> {balloon unpost};
  return [format "(:table (:toplevel . \"%s\") (:table . \"%s\")\
                          (:canvas . \"%s\"))" \
                 $toplevel $t $c]

}; # showtable()

proc canvas_select {canvas x y button} {
  set x [$canvas canvasx $x];
  set y [$canvas canvasy $y];
  set item [lindex [$canvas find closest $x $y] 0];
  if {$item != "" && [$canvas type $item] == "text"} {
    if {$button == 1} {
      $canvas select clear;
    }; # if
    $canvas select from $item 0;
    $canvas select to $item end;
  } else {
    tsdb_beep;
  }; # else
}; # canvas_select()

proc compute_requested_geometry {table width height} {
    ##
    ## Usage: compute_requested_geometry table width height
    ## 
    ## Computes the requested width and height of the toplevel window depending
    ## on the size of 'table'. These values are returned via the reference
    ## parameters 'width' and 'height'.
    ##

    upvar width w
    upvar height h

    ## Compute requested width and height of the canvas

    set cv [table_canvas $table]
    set bbox [$cv bbox all]
    set w [expr [lindex $bbox 2] - [lindex $bbox 0]]
    set h [expr [lindex $bbox 3] - [lindex $bbox 1]]

    ## Add space needed by other widgets contained in the toplevel

    incr w 42
    incr h 88
}

proc make_postscript {canvas name} {
    ##
    ## Usage: make_postscript canvas file
    ## 
    ## Writes 'canvas' as postscript to 'file'. Page format is landscape.
    ##

    ## Set regions' background to white

    status "generating PostScript output ...";

    regsub -all "/" $name "." file
    set file "/tmp/$file.ps"

    set bg [$canvas cget -background]
    $canvas itemconfig region -fill white -outline white

    ## Compute size of canvas

    set bb [$canvas bbox all]
    set x [lindex $bb 0]
    set y [lindex $bb 1]
    set w [expr [lindex $bb 2] - $x + 20]
    set h [expr [lindex $bb 3] - $y + 20]

    ## Write postscript file
    $canvas postscript -x $x -y $y -height $h -width $w \
	-file $file -rotate 1
    
    ## Set regions' background back to canvas background

    $canvas itemconfig region -fill $bg -outline $bg
    run_meter 500;
    status "wrote `$file'" 10;

}

proc read_table_file {file} {
    ##
    ## Usage: read_table_file file canvas regions
    ## 
    ## Reads the description of a table from 'file' and stores it in global
    ## variables.
    ##
    global format layout region cell

    catch {unset format}
    catch {unset layout}
    catch {unset region}
    catch {unset cell}

    while { ![eof $file] } {
	
	## Read next line

	set line [gets $file]

	## Ignore empty lines and comments (ie. lines starting with '#')

	if { $line == "" || [string index $line 0] == "#" } {
	    continue
	}

	## Process command in line

	set command [lindex $line 0]
	switch $command {
	    format  { set format([lindex $line 1]) [lrange $line 2 end] }
	    layout  { set layout([lindex $line 1],[lindex $line 2]) [lrange $line 3 end] }
	    region  { set region([lindex $line 1],[lindex $line 2],[lindex $line 3],[lindex $line 4]) \
			  [lrange $line 5 end] }
	    cell    { set cell([lindex $line 1],[lindex $line 2]) [lrange $line 3 end] }
	    default { error "showtable: unknown command '$command'!" }
	}
    }
}

proc make_table {table} {
    ##
    ## Usage: make_table table
    ## 
    ## Uses the information in the global variables to fill 'table'.
    ##
    global format layout region cell
    
    ## Process layout information

    foreach index [array names layout] {
	set idx_list [split $index ,]
	set rowcol [lindex $idx_list 0]
	set what [lindex $idx_list 1]

	set cmd "table_${rowcol}_"
	if {[string match "$what*" "defaults"] && $what != "" } {
	    set cmd "${cmd}defaults $table"
	} else {
	    set cmd "${cmd}info $table $what"
	}

	process_options $layout($index) opts
	
	set cmd "${cmd} $opts(m1) $opts(r) $opts(m2)"
	if { [info exists opts(c)] } {
	    set cmd "$cmd $opts(c)"
	} else {
	    set cmd "$cmd black"
	}
	if { [info exists opts(j)] } {
	    set cmd "$cmd $opts(j)"
	}

	eval $cmd
    }

    ## Insert cells into table

    foreach index [array names cell] {
	set idx_list [split $index ,]
	set row [lindex $idx_list 0]
	set col [lindex $idx_list 1]

	process_options $cell($index) opts

	set newitem [make_new_item [table_canvas $table] $row $col opts \
                     "cell$row,$col"]

	table_add $table $newitem $row $col
    }

    ## For each region: insert a "W" into each empty cell of the region, so that
    ## some minimum space is requested for the cell; if a cell already contains
    ## some text, leave it unchanged

    foreach index [array names region] {
	set idx_list [split $index ,]
	set rowNW [lindex $idx_list 0]
	set colNW [lindex $idx_list 1]
	set rowSE [lindex $idx_list 2]
	set colSE [lindex $idx_list 3]

	process_options $region($index) opts
	forI row $rowNW $rowSE {
	    forI col $colNW $colSE {
		if { [info exists cell($row,$col)] } {
		    set opts(contents) \
                        [[table_canvas $table] itemcget "cell$row,$col" -text]
		} else {
		    set opts(contents) "W"
		}
		set newitem [make_new_item [table_canvas $table] $row $col \
				 opts "region$row,$col"]
		table_add $table $newitem $row $col
	    }
	}
    }
}

proc make_new_item {canvas row col optionarrayname {tags ""} } {
    ##
    ## Usage: make_new_item canvas optionarrayname
    ## 
    ## Creates a new item on 'canvas' according to the options stored in the
    ## array with name 'optionarrayname'.
    ##
    global format

    upvar $optionarrayname o

    ## Process contents

    set c "$o(contents)"
    
    set item [eval $canvas create text -100 -100 -text \$c \
		  $format($o(format)) -tag \"$o(format) $tags\" \
		  -anchor w]
    if {[info exists o(key)] && [info exists o(source)]} {
      $canvas bind $item <Double-Button> \
        [list tsdb_process selection $o(source) $o(key)];
    }; # if
    return $item
}

proc make_regions {table} {
    ##
    ## Usage: make_regions table
    ## 
    ## Creates all regions via the proc 'table_make_regions'.
    ##
    global region
    
    set cv [table_canvas $table]

    ## Process all regions

    set regions ""
    foreach index [array names region] {
	set idx_list [split $index ,]
	set rowNW [lindex $idx_list 0]
	set colNW [lindex $idx_list 1]
	set rowSE [lindex $idx_list 2]
	set colSE [lindex $idx_list 3]

	process_options $region($index) opts

	## Delete all items in the region EXCEPT for the one in the upper left
	## cell; this one is reconfigured with the full text of the region

	forI row $rowNW $rowSE {
	    forI col $colNW $colSE {
		$cv delete "cell$row,$col"
		if { $row == $rowNW && $col == $colNW } {
		    $cv itemconfigure "region$row,$col" -text $opts(contents)
		} else {
		    $cv delete "region$row,$col"
		}
	    }
	}

	## Append new list element to 'regions'

	set elem "$rowNW $colNW $rowSE $colSE "
	if { [info exists opts(hor_justify)] } {
	    lappend elem $opts(hor_justify)
	} else {
	    lappend elem "center"
	}
	if { [info exists opts(ver_justify)] } {
	    lappend elem $opts(ver_justify)
	}
	lappend regions $elem
    }
    
    ## Call 'table_make_regions' with list
    
    table_make_regions $table $regions
}

proc process_options {optionlist optionarrayname} {
    ##
    ## Usage: process_options optionlist optionarrayname
    ## 
    ## Processes a list of options and stores them in an array. Eg. if
    ## 'optionlist' is "-text Table -format title" and 'optionarrayname' is
    ## "opts", then, after calling this procedure, 'opts(format)' will contain
    ## the string "title" and 'opts(text)' the string "table".
    ##
    upvar $optionarrayname o
    catch { unset o }

    while { $optionlist != "" } {
	set optionname [lindex $optionlist 0]
	set optionvalue [lindex $optionlist 1]
	set optionlist [lrange $optionlist 2 end]

	set o([string range $optionname 1 end]) $optionvalue
    }
}