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

## disable flawed default bindings of 'Table' widget
bind TkTable <Control-Left>  {}
bind TkTable <Control-Right> {}


proc showtable {filename {container ".table"} 
                         {database "unknown"} 
                         {title ""}
                         {cells -1}} {

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
##                viewer {fast | nice}
##
##                Specifies the viewer used to display the table; 'nice' uses a
##                viewer that has more layout options (e.g. regions, rulers,
##                copyright caption), whereas the 'fast' viewer has less layout
##                possibilities, but displays the table much faster and allows
##                interactive column resizing as well as fixed title columns and 
##                rows.  Defaults to 'nice'.  Postscript output of the table is
##                not supported by the fast viewer, either.
##
##                titlerows <num>
##
##                Specifies the number of title rows that stay fixed while
##                scrolling.  Defaults to 1.  The "nice" viewer ignores this
##                command.
##                
##                titlecols <num>
##
##                Specifies the number of title columns that stay fixed while
##                scrolling.  Defaults to 0.  The "nice" viewer ignores this
##                command.
##                
##                format <name> [options]
##
##                Specifies a new format '<name>' to be used in the 'cell' and
##                'region' commands.  Possible options (if the "nice" viewer is
##                used) are those applicable to Tk canvas text items,
##                e.g. '-font', '-fill', '-justify', etc.  If the "fast" viewer
##                is used, the possible options are '-font', '-fill', '-justify' 
##                (only for columns), and '-background' ('-bg' for short).
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
##                The "fast" viewer uses only the justification option for
##                columns and ignores all other options.
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
##                With the "fast" viewer, the same constraints as described
##                above for the "cell" command apply here.  Furthermore,
##                multiple line entrys are not supported properly.
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
##                The "region" command is ignored if the "fast" viewer was
##                selected.
##
## Caveats: The format of the input file is supposed to be correct and is only
##          partially checked. An incorrect file format might therefore result
##          in unpredictable program behavior.
##

  global globals viewer cell layout noOfCols noOfRows colWidth \
      colAnchor colDefaultAnchor  titlerows titlecols
    
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

  ## Read table from file

  if {$cells == -1} {
    set cells [read_table_file $file];
    if {[expr {$globals(critical_cell_threshold) > 0
               && $cells > $globals(critical_cell_threshold)}]} {
      tsdb_beep;
      if {[yes-or-no-p \
          "table layout may be slow ($cells  cells); continue"] != 1} {
        return "nil";
      }; # if
    }; # if
  } else {
    read_table_file $file;
    if {[expr {$globals(critical_cell_threshold) > 0
               && $cells > $globals(critical_cell_threshold)}]} {
      tsdb_beep;
      if {[yes-or-no-p \
          "table layout may be slow ($cells cells); continue"] != 1} {
        return "nil";
      }; # if
    }; # if
  }; # else

  ## Make toplevel window

  set toplevel [toplevel $container]
  set maxWindowWidth [expr int([winfo screenwidth $toplevel] * 0.95)];
  set maxWindowHeight [expr int([winfo screenheight $toplevel] * 0.95)];
  if { $viewer == "nice" } {
      set t [new_table $toplevel]
      set c [table_canvas $t]
  } else {
      ## Use fast viewer
      set t ${toplevel}.frm
      frame $t
      fast_viewer_preprocess_cells ${t}.tbl
      tktable ${t}.tbl -cache 1 -colorigin 1 -roworigin 1 \
	  -rows $noOfRows -cols $noOfCols \
	  -maxheight [expr int( $maxWindowWidth  * 0.90)] \
	  -maxwidth  [expr int( $maxWindowHeight * 0.90)] \
	  -drawmode fast -resizeborders col -state disabled \
	  -titlerows $titlerows -titlecols $titlecols \
	  -coltagcommand colTagProc -relief ridge -bd 1 \
	  -variable cell${t}.tbl \
	  -yscrollcommand "${t}.sy set" -xscrollcommand "${t}.sx set"
      scrollbar ${t}.sy -command [list ${t}.tbl yview]
      scrollbar ${t}.sx -command [list ${t}.tbl xview] -orient horizontal
      fast_viewer_postprocess_cells ${t}.tbl
      table $t \
	  0,0 ${t}.tbl -fill both \
	  0,1 ${t}.sy -fill y \
	  1,0 ${t}.sx -fill x
      table configure $t r1 c1 -resize none
      set c ${t}.tbl
  }

  button $toplevel.quit -text "Close" \
      -command [list "tsdb_close" $toplevel]
  button $toplevel.latex -text "LaTeX" \
      -command [list "tsdb_latex" $toplevel]
  if { $viewer == "nice" } {
      button $toplevel.print -text "PostScript" \
	  -command [list make_postscript $c $database]
  
      table $toplevel \
	  0,0 $t -cspan 3 -fill both -padx 5 -pady 5\
	  1,0 $toplevel.quit -pady {5 10} \
	  1,1 $toplevel.latex -pady {5 10} \
	  1,2 $toplevel.print -pady {5 10}
  } else {
      table $toplevel \
	  0,0 $t -cspan 2 -fill both -padx 5 -pady 5\
	  1,0 $toplevel.quit -pady {5 10} \
	  1,1 $toplevel.latex -pady {5 10} \
  }
  
  table configure $toplevel r1 -resize none
  if { $viewer == "nice" } {
      $c configure -relief flat -bg white -bd 3
  } else {
      $c tag configure title -relief ridge
  }
  wm title $toplevel $title
  wm iconname $toplevel $title

  if { $viewer == "nice" } {
      ## Display table
      
      make_table $t
      table_layout $t
      make_regions $t
      
      #
      # add copyright caption
      #
      set bbox [$c bbox all];
      set x [expr [lindex $bbox 2] - 1];
      set y [expr [lindex $bbox 3] + 1];
      set caption "(generated by $globals(name) at [current-time 1])";
      $c create text $x $y -anchor ne -text $caption \
	  -font $globals(copyright_font);
      
      table_fix_scroll $t
      $c xview moveto 0
      $c yview moveto 0
  }

  ## Display toplevel

  if { $viewer == "nice" } {
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
  }
  tkwait visibility $toplevel
  update idletasks
  #  catch {file delete -force $filename};
  close $file

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

  if { $viewer == "nice" } {
      bind $c <Button> [list canvas_select $c %x %y %b]
  } else {
      bind $c <Double-Button> {
	  set t %W
	  set index [$t index @%x,%y]
	  if {[info exists key${t}($index)] 
              && [info exists source${t}($index)]} {
	    [list tsdb_process selection [set source${t}($index)] [set key${t}($index)]]
	  }
      }
  }
  bind $c <Enter> {
      balloon post "<Button-1> selects cell element; \
                    <Double-Button-1> processes test item interactively";
  }
  bind $c <Leave> {balloon unpost}
  
  if { $viewer == "nice" } {
      return [format "(:table (:toplevel . \"%s\") (:table . \"%s\")\
                          (:canvas . \"%s\"))" \
		  $toplevel $t $c]
  } else {
      return [format "(:table (:toplevel . \"%s\") (:table . \"%s\")\
                          (:canvas . \"%s\"))" \
		  $toplevel $c ""]
  }
}; # showtable()

proc colTagProc {col} {
    return col$col
}

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

    global globals;

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
	-file $file -rotate $globals(landscape);
    
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
    global format layout region cell viewer titlerows titlecols

    catch {unset format}
    catch {unset layout}
    catch {unset region}
    catch {unset cell}
    set viewer "nice";			# default value
    set ncells 0;
    set titlecols 0;			# default value
    set titlerows 1;			# default value

    catch {fconfigure $file -encoding euc-jp};
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
	    cell    { 
              set cell([lindex $line 1],[lindex $line 2]) \
                  [lrange $line 3 end] 
              incr ncells;
            }
	    viewer  { set viewer [lindex $line 1] }
	    titlerows { set titlerows [lindex $line 1] }
	    titlecols { set titlecols [lindex $line 1] }
	    default { error "showtable: unknown command '$command'!" }
	}
    }
  return $ncells;
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
  global format;

  upvar $optionarrayname o;

  ## Process contents

  set c "$o(contents)";

  set item [eval $canvas create text -100 -100 -text \$c \
                         $format($o(format)) -tag \"$o(format) $tags\" \
                         -anchor w];

  if {[info exists o(key)] && [info exists o(source)]} {
    $canvas bind $item <Double-Button> \
      [list tsdb_process selection $o(source) $o(key)];
    $canvas bind $item <Control-Double-Button> \
      [list tsdb_browse trees "i-id == $o(key)" 0 $o(source)];

    if {[info exists o(action)] && [info exists o(stag)]} {
      $canvas itemconfigure $item -fill tomato;
    } else {
      $canvas itemconfigure $item -fill red;
    }; # else
  }; # if

  if {[info exists o(action)] && [info exists o(stag)]} {
    $canvas itemconfigure $item -fill red;
    $canvas bind $item <Shift-Double-Button> \
      [list tsdb_execute $o(action) $o(stag)];
  }; # if

  if {[info exists o(action)] && [info exists o(tag)]} {
    $canvas itemconfigure $item -fill red;
    $canvas bind $item <Double-Button> \
      [list tsdb_execute $o(action) $o(tag)];
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


#==[ Fast viewer ]==============================================================

proc fast_viewer_preprocess_cells {Tbl} {
    ##
    ## Usage: fast_viewer_preprocess_cells
    ## 
    ## After reading the table file, this procedure is used to set the values of 
    ## an array named 'cell$Tbl' to the values of the '-content' options.  This
    ## array is then used with the '-variable' option of the table widget.
    ## The anchor of all columns and the default anchor is set according to the
    ## "layout" commands in the input.
    ## The number of rows and columns of the table as well as the widths of the
    ## columns are determined.
    ## 

    global cell noOfCols noOfRows colWidth \
	layout colAnchor colDefaultAnchor

    upvar \#0 cell$Tbl c
    upvar \#0 source$Tbl source
    upvar \#0 key$Tbl key
    
    catch {unset c}
    catch {unset colWidth}
    set noOfCols 0
    set noOfRows 0

    ## Process "cell" information
    foreach index [array names cell] {
	set idx_list [split $index ,]
	set row [lindex $idx_list 0]
	set col [lindex $idx_list 1]

	process_options $cell($index) opts
	set c($index) $opts(contents)
	if { [catch {set k $opts(key)}] == 0 } {
	    set key(${row},${col}) $k
	}
	if { [catch {set s $opts(source)}] == 0 } {
	    set source(${row},${col}) $s
	}

	if { $row > $noOfRows } { set noOfRows $row }
	if { $col > $noOfCols } { set noOfCols $col }
	
	set w 0
	catch { set w $colWidth($col) }
	set colWidth($col) [max $w [string length $c($index)]]
    }

    ## Process "layout" information
    foreach l [array names layout] {
	set idx_list [split $l ,]
	set what [lindex $idx_list 0]
	set idx  [lindex $idx_list 1]
	
	if { $what == "col" } {	# rows are ignored for the time being
	    process_options $layout($l) opts
	    if { [catch { set just $opts(j) }] == 0 } {
		switch $just {
		    left    { set anchor w }
		    center  { set anchor center }
		    right   { set anchor e }
		    default { set anchor "" }
		}
		if { $anchor != "" } {
		    if { $idx == "def" } {
			set colDefaultAnchor $anchor
		    } else {
			set colAnchor($idx) $anchor
		    }
		}
	    }
	}
    }
}


proc fast_viewer_postprocess_cells {Tbl} {
    ##
    ## Usage: fast_viewer_postprocess_cells
    ## 
    ## Set widths of table columns to values determined in
    ## '..._preprocess_cells'.  Layout table according to options in input
    ## file.
    ## 
     
    global noOfCols colWidth format cell colAnchor colDefaultAnchor

    ## Set column widths and process anchor information
    catch { $Tbl configure -anchor $colDefaultAnchor }
    forI c 1 $noOfCols {
	set w 0
	catch { set w $colWidth($c) }
	$Tbl width $c $w
	catch { $Tbl tag configure col$c -anchor $colAnchor($c) }
    }

    ## Process layout info of cells
    foreach index [array names cell] {
	set idx_list [split $index ,]
	set row [lindex $idx_list 0]
	set col [lindex $idx_list 1]
	
	process_options $cell($index) opts

	## Process 'format' layout first, if specified for current cell
	if { [catch { set f $opts(format) }] == 0 } {
	    process_options $format($f) fopts
	    fast_viewer_process_cell_options $Tbl c${row},${col} fopts
	}

	## Process remaining options
	fast_viewer_process_cell_options $Tbl c${row},${col} opts

	## Set tag
	$Tbl tag cell c${row},${col} ${row},${col}
    }
}

proc fast_viewer_process_cell_options {Tbl TagName OptsVarName} {
    ##
    ## Usage: fast_viewer_process_cell_options Tbl TagName OptsVarName
    ## 
    ## Layouts all cells with tag 'TagName' of table 'Tbl' according to layout
    ## requirements specified in the option array 'OptsVarName'.  Since the
    ## "fast" viewer is not based on a canvas widget, not all layout options of
    ## the "nice" viewer are applicable here. (See comment header of this file.)
    ## 

    upvar $OptsVarName opts

    foreach option [array names opts] {
	switch $option {
	    fill        { $Tbl tag configure $TagName -fg $opts(fill) }
	    font        { $Tbl tag configure $TagName -font $opts(font) }
	    bg          -
	    background  { $Tbl tag configure $TagName -background $opts($option) }
	    justify     {  
		switch $opts(justify) {
		    left    { set anchor "w" }
		    center  { set anchor "center" }
		    right   { set anchor "e" }
		    default { set anchor "" }
		}
		if { $anchor != "" } {
		    $Tbl tag configure $TagName -anchor $anchor
		}
	    }
	    default  {}
	}
    }
}

