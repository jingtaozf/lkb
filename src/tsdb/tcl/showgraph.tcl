##
## ========================
##   A Simple Graph Viewer
## ========================
##
## File:    showgraph.tcl
##
## Author:  Oliver Plaehn (plaehn@coli.uni-sb.de)
##
## Purpose: Reads a graph description from a file and displays it in a
##          canvas (procedure 'showgraph', see below).
## 
## File format:
##
##          The file format of graph descriptions as expected by 'showgraph'
##          is as follows (see 'test.gph' for an example).
##
##          Lines are separated by newline characters. If the last character of
##          a line is a \ (backslash) the next line is interpreted as a
##          continuation of the current line.  Empty lines (ie. lines consisting
##          exclusively of spaces and tabs or a single newline character) and
##          lines starting with a '#' (comments) are ignored.  All other lines
##          must start with one of the commands described below, followed by its
##          obligatory arguments (if any) and zero or more option/value pairs
##          (eg. '-font times').  Order of the arguments is relevant, order of
##          the option/value pairs is not.  The file might contain commands
##          (ie. lines) in any order.
##
##          Commands:
##
##            {barchart | graph} [options]
##
##            Specifies the type of the graph to be displayed.  Only one such
##            command (either 'barchart' or 'graph') should be present in an
##            input file.  If there are more than one 'graph' or 'barchart'
##            commands the last one overrides all others.
##            The options are directly passed to the newly created BLT barchart
##            or graph widget that
##
## Caveats: The format of the input file is supposed to be correct and is only
##          partially checked.  An incorrect file format might therefore result
##          in unpredictable program behavior.
##
## 
## Todo:
##
## - describe file format
## - add more commands for pens, grids, etc.
## - allow for Blt_ZoomStack, Blt_Crosshairs, Blt_ActiveLegend, Blt_ClosestPoint commands
## - re-compile BLT with bug in 'bltGrAxis.c' fixed
## - email bug report
## 

## Constants and global variables

set windowTitle "Graph Viewer";		# Default toplevel title
set noOfGraphWindows 0;			# Counter for number of toplevel windows


## ------------------
##   Main procedure
## ------------------

proc showgraph {FileName {Toplevel ""} {output "unknown"} {Title ""} {scatterp 0}} {

    ## Displays a new graph that is read from 'FileName'.  A new toplevel window
    ## is created with title 'Title'.  If 'Title' is empty, "Graph Viewer"
    ## followed by the filename is used as title. The name of the new toplevel is
    ## 'Toplevel'. If 'Toplevel' is empty, a new name is created.  The name of
    ## the new toplevel is returned.

  global globals;

  set windowTitle "Graph Viewer";		# Default toplevel title

    ## Open file

    if { [catch {set file [open $FileName r]}] != 0 } {
	error "Couldn't open table file '$FileName'!"
    }

    ## Set names of toplevel and title

    if { $Toplevel != "" } {
	set t $Toplevel
    } else {
	set t ".graph[gensym]"
    }

    set g $t.graph

    if { $Title != "" } {
	set title $Title
    } else {
      set title [format "tsdb(1) `%s' bar chart view" $FileName]
    }
	
    ## Make toplevel window

    toplevel $t
    button $t.quit  -text "Close" -command [list "tsdb_close" $t]
    button $t.latex -text "LaTeX" -command [list "tsdb_latex" $t];
    button $t.print -text "PostScript" \
        -command [list graph_postscript $g $output]

    #
    # add balloon-type label for inspection of individual data points
    #
    if {$scatterp} {
      label $t.balloon -relief flat -bd 0 \
        -font $globals(balloon_font) -anchor c \
    }; # if

    wm title $t $title
    wm iconname $t $title

    ## Read graph from file, create and configure it

    read_graph_file $file $g
    make_graph $g

    ## Arrange widgets in toplevel

    if {$scatterp} {
      table $t \
          0,0 $t.balloon -cspan 3 -fill both -padx 5 -pady {5 0} \
          1,0 $g -cspan 3 -fill both -padx 5 -pady 5\
          2,0 $t.quit -pady {5 10} \
          2,1 $t.latex -pady {5 10} \
          2,2 $t.print -pady {5 10};
    } else {
      table $t \
          0,0 $g -cspan 3 -fill both -padx 5 -pady 5\
          1,0 $t.quit -pady {5 10} \
          1,1 $t.latex -pady {5 10} \
          1,2 $t.print -pady {5 10};
    }; # else
    table configure $t r1 -resize none


    #
    # establish bindings for interactive zooming and balloon help
    #
    bind $g <Motion> "find_graph_point post %W %b %x %y $t.balloon"
    bind $g <Shift-Button> "find_graph_point browse %W %b %x %y $t.balloon"
    bind $g <Button> {adjust_graph_view %W %b %x %y}
    bind $g <Enter> {
      balloon post "click <Button-1> or <Button-3> on the plot area to adjust axis limits; <Button-2> to reset"
    }; # bind
    bind $g <Leave> {balloon unpost};
    bind $t <q> [list "tsdb_close" $t];
    bind $t <Q> [bind $t <q>];

    #
    # activate legend entries: pop-up menu for interactive colour selection
    #
    set colours {white yellow orange red blue green black};
    foreach item [$g element names] {
      set menu [menu $g.$item -font {Helvetica 9} -tearoff no];
      foreach colour $colours {
        $menu add command -label $colour -foreground $colour \
          -command [list $g element config $item -color $colour]
      }; # foreach
      $g legend bind $item <Button> [list tk_popup $menu %X %Y];
    }; # foreach


    ## Display toplevel

    tkwait visibility $t
    update idletasks

    ## Return toplevel name
    return [format "(:graph (:toplevel . \"%s\") (:graph . \"%s\"))" \
            $t $g];

}

## --------------
##   Procedures
## --------------

proc graph_postscript {graph name} {
    ##
    ## Usage: make_postscript graph file
    ## 
    ## Writes 'graph' as postscript to 'file'.  Page format is landscape.
    ##

  global globals;

    ## Write postscript file

  status "generating PostScript output ...";

  regsub -all "/" $name "." name
  set file "/tmp/$name.ps"

  $graph postscript output $file -landscape $globals(landscape) \
     -paperheight 29.7c -paperwidth 21c -decoration no
   run_meter 500;
  status "wrote `$file'" 10;
}

proc read_graph_file {file graph} {
    ##
    ## Usage: read_graph_file file canvas regions
    ## 
    ## Reads the description of a graph from 'file' and stores it in global
    ## variables.
    ##
    global graphtype graphoptions data axis element legend

    ## Reset old values in globals

    catch {unset graphtype}
    catch {unset graphoptions}
    catch {unset data}
    catch {unset function}
    catch {unset axis}
    catch {unset element}
    catch {unset legend}

    ## Read input file line by line

    while { ![eof $file] } {
	
	## Read next line; backslash at end of line continues line

	set line ""
	do {
	    set line "$line[gets $file]"
	} while {[regexp {^(.*)\\$} $line dummy line]}

	## Ignore empty lines and comments (ie. lines starting with '#')

	if { $line == "" || [string index $line 0] == "#" } {
	    continue
	}

	## Process command in line
        regsub -all "\\." $graph "_" prefix
        set prefix "v$prefix"

	set command [lindex $line 0]
	switch $command {
	    barchart -
	    graph    { 
              set graphtype $command; 
              set graphoptions [lrange $line 1 end] 
            }
	    data     { set data([lindex $line 1]) [lindex $line 2] }
	    axis     { set axis([lindex $line 1]) [lrange $line 2 end] }
	    element  { 
              set e [lindex $line 1];
              set args [lrange $line 2 end];
              set xdata [expr [lsearch $args "-xdata"] + 1];
              set ydata [expr [lsearch $args "-ydata"] + 1];
              set xname [lindex $args $xdata];
              set yname [lindex $args $ydata];
              set args [lreplace $args $xdata $xdata "${prefix}_$xname"];
              set args [lreplace $args $ydata $ydata "${prefix}_$yname"];
              set element($e) $args;
            }
	    legend   { set legend [lrange $line 1 end] }
	    default  { error "showgraph: unknown command '$command'!" }
	}
    }
}

proc axislabel {Labels Graph Tick} {
    return [lindex $Labels [expr $Tick - 1]]
}

proc make_graph {graph} {
    ##
    ## Usage: make_graph graph
    ## 
    ## Uses the information in the global variables to make either a graph or a
    ## barchart.  'graph' is the name to be used for the graph/barchart.
    ##
    global globals graphtype graphoptions data axis element labels legend

    ## Create BLT "graph" or "barchart" according to 'graphtype'

    eval $graphtype $graph $graphoptions

    ## Make data vectors

    foreach index [array names data] {
      regsub -all "\\." $graph "_" vname
      set vname "v${vname}_$index"
      uplevel #0 "vector $vname";
      $vname set $data($index)
    }

    ## Create new elements in 'graph'

    foreach index [array names element] {
	eval $graph element create $index $element($index)
    }

    ## Configure legend

    if { [info exists legend ] } {
	eval $graph legend configure $legend
    }

    ## Configure axis

    foreach index [array names axis] {
	
	## Process options not recognised by BLT graph/barchart

	process_options $axis($index) o

	## Option '-labels'
	
	if { [info exists o(labels)] } {
	    $graph axis configure $index -command [list axislabel $o(labels)]
	    unset o(labels)
	}
	    
	## Configure axis with remaining options
	
	foreach option [array names o] {
	    eval $graph axis configure $index -$option \$o($option)
	}
    }

  #
  # kludge (2-nov-98): adjust y axis to leave extra space at the bottom and
  # position copyright notice; needs improvement ...
  #
  $graph axis config y -min {} -max {};
  set dimension [expr {[$graph cget -invertxy] ? "x" : "y"}];
  set padding [expr {[$graph cget -invertxy] ? 0.025 : 0.05}];
  set foo [$graph axis limits $dimension];
  set xmin [lindex $foo 0];
  set xmax [lindex $foo 1];
  if {[$graph axis cget $dimension -logscale]} {
    set pad 0;
  } else {
    set pad [expr ($xmax - $xmin) * $padding];
  }; # else
  $graph axis config $dimension -min [expr $xmin - $pad] -max $xmax;
  set caption "(generated by $globals(name) at [current-time 1]";
  set copyright [label $graph.copyright -bg white -fg black];
  $copyright config -relief flat -bd 2 \
    -text $caption -font $globals(copyright_font);
#  if {[$graph cget -invertxy]} {
#   $graph marker create window -coords {-Inf Inf} -anchor e -window $copyright
#  } else {
#    $graph marker create window -coords {Inf -Inf} -anchor e -window $copyright
#  }; # else
  #
  # now that text markers are supposedly fixed, they come out in dark grey
  # when generating postscript |:-(.
  #
  $graph marker create text -coords {Inf -Inf} -anchor e \
    -text $caption -font $globals(copyright_font) \
    -fill white -fg black
}    

proc process_options {optionlist optionarrayname} {
    ##
    ## Usage: process_options optionlist optionarrayname
    ## 
    ## Processes a list of options and stores them in an array. Eg. if
    ## 'optionlist' is "-text Graph -format title" and 'optionarrayname' is
    ## "opts", then, after calling this procedure, 'opts(format)' will contain
    ## the string "title" and 'opts(text)' the string "graph".
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


proc local_balloon {balloon action {text ""}} {

  global globals;

  if {$action == "post"} {
    $balloon config -text $text;
  } else {
    $balloon config -text $text;
  }; # else
}; # local_balloon()


proc find_graph_point {action graph button x y balloon} {
  if {[$graph element closest $x $y point -halo 5]} {
    set element [string range $point(name) 2 end];
    regsub -all "\\." $graph "_" name
    set ids "v${name}_ids$element"
    upvar #0 $ids vector
    if {[info exists vector]} {
      if {$action == "post"} {
        set x [format "%.2f" $point(x)];
        set y [format "%.2f" $point(y)];
        set id [expr round($vector($point(index)))];
        local_balloon $balloon post \
          "data point ($x  $y) corresponds to item \# $id"
      } elseif {$action == "browse"} {
        tsdb_browse parses "i-id = [expr round($vector($point(index)))]" 0;
      }; # if
      return;
    }; # if
  }; # if
  if {$action == "post"} {
    local_balloon $balloon unpost;
  }; # if

}; # find_graph_point()

proc adjust_graph_view {graph button x y} {

  if {[$graph legend get @$x,$y] != ""} {
    return;
  }; # if

  set x [$graph axis invtransform x $x];
  set y [$graph axis invtransform y $y];
  set foo [$graph axis limits x];
  set xmin [lindex $foo 0];
  set xmax [lindex $foo 1];
  set foo [$graph axis limits y];
  set ymin [lindex $foo 0];
  set ymax [lindex $foo 1];

  if {$button == 1} {
    if {$x > $xmin} {
      $graph axis config x -min $x;
    } else {
      $graph axis config x -min {};
    }; # else
    if {$y > $ymin} {
      $graph axis config y -min $y;
    } else {
      $graph axis config y -min {};
    }; # else
  } elseif {$button == 2} {
    $graph axis config x -min {} -max {};
    $graph axis config y -min {} -max {};
    #
    # kludge (2-nov-98): adjust y axis to leave extra space at the bottom and
    # position copyright notice; needs improvement ...
    #
    $graph axis config y -min {} -max {};
    set foo [$graph axis limits y];
    set xmin [lindex $foo 0];
    set xmax [lindex $foo 1];
    if {[$graph axis cget y -logscale]} {
      set pad 0;
    } else {
      set pad [expr ($xmax - $xmin) * 0.05];
    }; # else
    $graph axis config y -min [expr $xmin - $pad] -max $xmax;
  } elseif {$button == 3} {
    if {$x < $xmax} {
      $graph axis config x -max $x;
    } else {
      $graph axis config x -max {};
    }; # else
    if {$y < $ymax} {
      $graph axis config y -max $y;
    } else {
      $graph axis config y -max {};
    }; # else
  }; # if

}; # adjust_graph_view()
