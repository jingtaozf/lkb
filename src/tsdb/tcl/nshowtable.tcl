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


proc showtable {filename {container ".table"} {database "unknown"} {title ""}} {

## Usage:   showtable <tablefilename>
##          
## Input:   <tablefilename>
##
##              Contains the description of the table to be displayed.  The
##              format is as follows (see 'test.tbl' for an example).
##              
##              The information in the input file is roughly divided into three
##              parts.  The first part contains lines specifying which viewer is 
##              to be used, how many rows and columns are contained in the
##              table, and how many of these are title columns/rows.  (The
##              latter applies only to the fast viewer.)  Then follows the
##              contents of the cells, every cell on a single line, in the order 
##              (row1, col1), (row1, col2), ..., (row1, colM), (row2, col1),
##              ..., (row2, colM), ..., (rowN, col1), ..., (rowN, colM), that
##              is, from left to right and from top to bottom.  (N is the number
##              of rows, M the number of columns, as specified in the first part
##              of the file).  The third and last part of the input file
##              specifies the layout of columns and rows, and the format of
##              cells.
##
##              Each of these three parts is now described in more detail.
##
##              First part:
##              -----------
##              The first line of the file specifies which viewer should be used
##              to display the table.  It thus starts with the keyword "viewer"
##              in the first column, followed by either "nice" or "fast" in the
##              second column.  (Lines usually consists of several columns,
##              which are separated by one or more whitespace(s), that is,
##              blanks or tabs.)  The "nice" viewer offers more layout options
##              than the "fast" viewer (e.g. regions), and allows postscript
##              output of the table.  The "fast" viewer is, as the name
##              suggests, faster than the "nice" viewer, and additionally
##              facilitates dynamic resizing of columns and rows, and title
##              columns and rows, which remain fixed when scrolling the table
##              contents.  The differences between the two viewers are described 
##              in more detail below.
##
##              The second line must look like "noofrows <num>", where "<num>"
##              is the number of rows that the table contains.  The third line
##              specifies the number of columns as "noofcols <num>".
##
##              The fourth and fifth line specify the number of title rows and
##              title columns as "titlerows <num>" and "titlecols <num>". (This
##              applies only, if the fast viewer has been chosen, otherwise
##              these line are to be left out!)
##
##
##              Second part:
##              ------------
##              The second part contains the contents of the table cells, the
##              text for each cell on a single line.  The first line in the
##              second part of the input file specifies the text to be displayed
##              in the upper left table cell, the last line in this part the
##              text to be displayed in the lower right cell (see remarks
##              above). 
##
##              If the text for a cell contains whitespaces, it should be quoted
##              with ".  Line breaks can be inserted with '\n', special
##              characters with '\<octnum>', where <octnum> is a 3-digit octal
##              number.  For example, the line "total\n\330" makes the
##              corresponding cell contain two lines of text, the first
##              being "total" (without the quotes, of course), the second the
##              sign for average (a circle with a diagonal line crossing it).
##
##
##              Third part:
##              -----------
##              The third part begins with an arbitrary number (including zero)
##              of lines that specify the layout of columns and rows.  The
##              available layout options differ for the nice and the fast
##              viewer, so we describe both separately.
##              
##              For the nice viewer, each line has the format
##              "layout [col | row] [def | <num>] <m1> <r> <m2> <color> <just>".
##              The second column specifies whether the following layout options
##              apply to a column or a row.  If the third column contains "def",
##              the default layout for all columns/rows is specified, otherwise
##              the layout of the <num>-th column/row.
##              <m1> is the width (height) of the margin to the left of the
##              column (above the row), <m2> the width (height) of the margin to
##              the right of the column (below the row), <r> is the width of the
##              ruler.  All distances are measured in pixels.  <color> specifies
##              the color of the ruler between this column and its right
##              neighbor (between this row and the one below it).  The layout of 
##              the leftmost (topmost) ruler is specified, if <num> is set to
##              0.  <just> determines the justification of cells in this
##              row/column.  Valid values are "left", "right", or "center" for
##              columns, and "top", "bottom", or "center" for rows.
##
##              For the fast viewer, each line looks like "layout col <num>
##              <just>", that is, only the justification of cells in columns can 
##              be specfied, all other options mentioned in the previous
##              paragraph are not applicable for the fast viewer.  <num> is the
##              column number ("def" for default is NOT possible), <just> the
##              justification, with possible values "n", "w", "s", "e" (for
##              north, west, south or east, resp.), mixed forms like "nw",
##              "se", etc. (for northwest and southeast, resp.), or "center".
##
##              After the specification of row/column layout, a blank line is
##              needed to separate this block from the next one, that specifies
##              the format of cells.
##
##              The first line of this block looks like "default <options>" and
##              specifies the default format of all cells.  For the fast viewer,
##              the next line specifies the format of the title rows/columns:
##              "title <options>".  If the nice viewer is used, leave this line
##              out.  The next zero, one or more lines specify the format of
##              particular columns/rows: "[col | row] <num> <options>".  After
##              another blank line, individual cells can be formatted with lines 
##              like "cell <row> <col> <options>".
##
##              Available options again differ depending on the chosen viewer.
##              Options for the nice viewer are those usable with Tk canvas text
##              items, for instance:
##              -fill <color>         Text color
##              -justify <j>          Justification of multi-line text entries
##                                    within given space (<j> is "left",
##                                    "center", or "right")
##              -font <fontname>      Specification of font used for displaying
##                                    the text
##
##              Available options for the fast viewer are those usable with the
##              TkTable widget, e.g.:
##              -fg <color>           Foreground color
##              -bg <color>           Background color
##              -font <fontname>      as above
##              -justify <j>          as above
##
##              The input of <options> in the above commands is directly passed
##              to the text item (nice viewer) or to the TkTable widget (fast
##              viewer), without any error checking.
##
##              After a separating blank line, one ore more regions may be
##              specified for the nice viewer.  A region spans more than one row 
##              and/or column.  The line format is "region <rowNW> <colNW>
##              <rowSE> <colSE> <hor_just> <ver_just>".  (<rowNW>,<colNW>) is
##              the upper left corner of the region, (<rowSE>,<colSE>) the lower 
##              right corner.  The contents of the region (and its format) is
##              copied from the upper left cell (as specfied in the second part
##              of the input file and with the "cell" command, resp.)  All other 
##              cells within the region are overwritten.
##              <hor_just> is the horizontal justification of the cell within
##              the region.  Possible values are "left", "right", or "center".
##              The latter is the default.  <ver_just> is the vertical
##              justification of the cell within the region; possible values:
##              "top", "bottom, "center", the last being the default.
##
## Caveats: The format of the input file is supposed to be correct and is only
##          partially checked.  An incorrect file format might therefore result
##          in unpredictable program behavior.
##

  global globals viewer cell layout noOfCols noOfRows colWidth \
      colAnchor colDefaultAnchor  titlerows titlecols regions
    
  ## Constants

  set minWindowWidth 100; 		# Minimum width of toplevel window
  set minWindowHeight 100;		# Minimum height of toplevel window

  ## Compute window title if not passed as `title' argument

  if {$title == ""} {
      set title [format "tsdb(1) analysis: `%s'" $database]
  }

  ## Open file

  if { [catch {set file [open $filename r]}] != 0 } {
      error "Couldn't open table file '$filename'!"
  }

  ## Make toplevel window

  set toplevel [toplevel $container]
  set maxWindowWidth [expr int([winfo screenwidth $toplevel] * 0.95)];
  set maxWindowHeight [expr int([winfo screenheight $toplevel] * 0.95)];

  ## Determine viewer to be used, number of rows, number of columns, and number
  ## of title rows and cols for fast viewer
  
  set viewer [lindex [gets $file] 1]
  set noOfRows [lindex [gets $file] 1]
  set noOfCols [lindex [gets $file] 1]
  if { $viewer == "fast" } {
      set noOfTitleRows [lindex [gets $file] 1]
      set noOfTitleCols [lindex [gets $file] 1]
  }

  ## Nice viewer: create table

  if { $viewer == "nice" } {
      ## Use nice viewer
      set t [new_table $toplevel]
      set tbl $t
      set c [table_canvas $t]
  } else {
      set t ""
      set tbl ${toplevel}.frm.tbl
      set c ""
  }

  ## Read contents of cells

  read_file_cell_contents $file $viewer $tbl $c

  ## Create new table if fast viewer

  if { $viewer == "fast" } {
      set t ${toplevel}.frm
      frame $t
      tktable ${t}.tbl -cache 1 -colorigin 1 -roworigin 1 \
	  -rows $noOfRows -cols $noOfCols \
	  -maxheight [expr int( $maxWindowWidth  * 0.90)] \
	  -maxwidth  [expr int( $maxWindowHeight * 0.90)] \
	  -drawmode fast -resizeborders col -state disabled \
	  -titlerows $noOfTitleRows -titlecols $noOfTitleCols \
	  -coltagcommand colTagProc \
	  -rowtagcommand rowTagProc \
	  -relief ridge -bd 1 \
	  -variable cell${t}.tbl \
	  -yscrollcommand "${t}.sy set" -xscrollcommand "${t}.sx set"
      scrollbar ${t}.sy -command [list ${t}.tbl yview]
      scrollbar ${t}.sx -command [list ${t}.tbl xview] -orient horizontal
      table $t \
	  0,0 ${t}.tbl -fill both \
	  0,1 ${t}.sy -fill y \
	  1,0 ${t}.sx -fill x
      table configure $t r1 c1 -resize none
      set c ${t}.tbl
  }

  ## Read remaining info in input file
  
  read_file_layout $file $viewer $tbl
  read_file_cell_layout $file $viewer $tbl

  ## Create widgets in toplevel

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
      
      read_file_regions $file
      preprocess_regions $t
      table_layout $t
      postprocess_regions $t
      
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

proc rowTagProc {row} {
    return row$row
}

proc canvas_select {canvas x y button} {
  set x [$canvas canvasx $x];
  set y [$canvas canvasy $y];
  set item [lindex [$canvas find closest $x $y] 0];
  if {$item != "" && [$canvas type $item] == "text"} {
    if {$button == 1} {
      $canvas select clear;
    }; # if
    ##[ OP: Test 'wfsl'! ]##################################################
    set charpos [$canvas index $item @$x,$y]
    set list    [$canvas itemcget $item -text]
    puts [wfsl $list $charpos]
    ########################################################################
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
    ## Usage: make_postscript canvas name
    ## 
    ## Writes 'canvas' as postscript to file 'name'. Page format is landscape.
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

proc read_file_cell_contents {file viewer tbl canvas} {
    ##
    ## Usage: read_file_cell_contents file viewer tbl canvas
    ## 
    ## Reads the contents of all cells from 'file'.  'viewer' specifies whether
    ## the "fast" or the "nice" viewer should be used.  For the fast viewer, the 
    ## contents are stored in a global array 'cell$tbl' and the widths of all
    ## columns are computed and stored in 'colWidth'.  For the nice viewer, a
    ## canvas text item is created for every cell with the contents of the cell
    ## as text, and then added to the table with 'table_add'.
    ##
    global noOfCols noOfRows colWidth

    if { $viewer == "fast" } {
	upvar \#0 cell${tbl} c
	catch { unset c }
	catch { unset colWidth }
    }

    forI row 1 $noOfRows {
	forI col 1 $noOfCols {
	    eval set contents [gets $file]
	    if { $viewer=="fast" } {
		set c($row,$col) "$contents"
		## Determine max col width
		set w 0
		catch { set w $colWidth($col) }
		set colWidth($col) [max $w [string length $contents]]
	    } else {
		set newitem [eval $canvas create text -100 -100 -text \"$contents\" \
				 -tag \"cell$row,$col col$col row$row allcells\" \
				 -anchor w]
		table_add $tbl $newitem $row $col
	    }
	}
    }
}

proc read_file_layout {file viewer tbl} {
    ##
    ## Usage: read_file_layout file viewer tbl
    ## 
    ## Reads layout of columns and rows from 'file'.  For "fast" viewer, only
    ## the justification of columns can be set.  Plus setting the widths of
    ## columns according to 'colWidth' (set in 'read_file_cellcontents').
    ##
    global colWidth noOfCols

    if { $viewer == "fast" } {
	forI c 1 $noOfCols {
	    set w 0
	    catch { set w $colWidth($c) }
	    $tbl width $c $w
	}
    }

    while { ![eof $file] } {
	set line [gets $file]
	if { $line == "" } {
	    break
	}

	if { [lindex $line 0] != "layout" } {
	    error "showtable: incorrect file format!"
	}

	if { $viewer == "fast" } {
	    set what [lindex $line 1]
	    set col  [lindex $line 2]
	    set just [lindex $line 3]
	    if { $what != "col" } {
		error "showtable: incorrect file format!"
	    }
	    $tbl tag configure col$col -anchor $just
	} else {
	    set rowcol [lindex $line 1]
	    set what   [lindex $line 2]
	    set opts   [lrange $line 3 end]
	    set cmd "table_${rowcol}_"
	    if { $what == "def" } {
		set cmd "${cmd}defaults $tbl"
	    } else {
		set cmd "${cmd}info $tbl $what"
	    }
	    eval $cmd $opts
	}
    }
}

proc read_file_cell_layout {file viewer tbl} {
    ##
    ## Usage: read_file_cell_layout file viewer tbl
    ## 
    ## Reads layout of cells (default, per col/row, per cell) from 'file'.
    ##
    global noOfCols

    if { $viewer == "nice" } {
	set c [table_canvas $tbl]
    }

    ## Read and set default cell layout
    set line [gets $file]
    set opts [lrange $line 1 end]
    if { $viewer == "fast" } {
	forI i 1 $noOfCols {
	    eval $tbl tag configure col$i $opts
	}
    } else {
	eval $c itemconfigure allcells $opts
    }

    ## Read and set title layout (only for fast viewer)
    if { $viewer == "fast" } {
	set line [gets $file]
	set opts [lrange $line 1 end]
	eval $tbl tag configure title $opts
    }

    ## Read and set cell layout for columns/rows
    while { ![eof $file] } {
	set line [gets $file]
	if { $line == "" } {
	    break
	}

	set rowcol [lindex $line 0]
	set rowcolnum [lindex $line 1]
	set opts [lrange $line 2 end]
	if { $viewer == "fast" } {
	    eval $tbl tag configure ${rowcol}$rowcolnum $opts
	} else {
	    eval $c itemconfigure ${rowcol}$rowcolnum $opts
	}
    }

    ## Read and set cell layout for single cells
    while { ![eof $file] } {
	set line [gets $file]
	if { $line == "" } {
	    break
	}

	set row [lindex $line 1]
	set col [lindex $line 2]
	set opts [lrange $line 3 end]
	if { $viewer == "fast" } {
	    eval $tbl tag configure cell$row,$col $opts
	    eval $tbl tag cell cell$row,$col $row,$col
	} else {
	    eval $c itemconfigure cell$row,$col $opts
	}
    }
}


proc read_file_regions {file} {
    ##
    ## Usage: read_file_regions file
    ## 
    ## Reads region information into array "region".
    ##
    global region

    ## Reset 'region' array 
    catch { unset region }

    ## Read region information until end-of-file or empty line
    while { ![eof $file] } {
	set line [gets $file]
	if { $line == "" } {
	    break
	}

	set rowNW [lindex $line 1]
	set colNW [lindex $line 2]
	set rowSE [lindex $line 3]
	set colSE [lindex $line 4]
	set region($rowNW,$colNW,$rowSE,$colSE) [lrange $line 5 end]
    }
}

proc preprocess_regions {table} {
    ##
    ## Usage: preprocess_regions table
    ## 
    ## Uses the information in the global array "region" to preprocess all
    ## regions.  For each region, all non-empty cells are filled with some
    ## minimum space.
    ##
    global region

    ## For each region: insert a "W" into each empty cell of the region, so that
    ## some minimum space is requested for the cell; if a cell already contains
    ## some text, leave it unchanged

    set cv [table_canvas $table]

    foreach index [array names region] {
	set idx_list [split $index ,]
	set rowNW [lindex $idx_list 0]
	set colNW [lindex $idx_list 1]
	set rowSE [lindex $idx_list 2]
	set colSE [lindex $idx_list 3]
	
	forI row $rowNW $rowSE {
	    forI col $colNW $colSE {
		if { $row != $rowNW || $col != $colNW } {
		    ## If cell doesn't exist, create it
		    if { [$cv gettags cell$row,$col] == "" } {
			set newitem [$cv create text -100 -100 -text "W" \
					 -tag "cell$row,$col" -anchor w]
			table_add $table $newitem $row $col
		    }
		}
	    }
	}
    }
}

proc postprocess_regions {table} {
    ##
    ## Usage: postprocess_regions table
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

	## Delete all items in the region EXCEPT for the one in the upper left
	## cell; this one is reconfigured with the full text of the region

	forI row $rowNW $rowSE {
	    forI col $colNW $colSE {
		if { $row != $rowNW || $col != $colNW } {
		    $cv delete "cell$row,$col"
		}
	    }
	}

	## Append new list element to 'regions'

	set elem "$idx_list $region($index)"
	lappend regions $elem
    }
    
    ## Call 'table_make_regions' with list
    
    table_make_regions $table $regions
}
