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

  global globals graphtype;

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
    make_graph $g $scatterp;

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
    if {$scatterp} {
      bind $g <Motion> "find_graph_point post %W %b %x %y $t.balloon"
      bind $g <Shift-Button> "find_graph_point browse %W %b %x %y $t.balloon"
    }; # if
    bind $g <Button> {adjust_graph_view %W %b %x %y}
    bind $g <Enter> {
      balloon post "click <Button-1> or <Button-3> on the plot area to adjust axis limits; <Button-2> to reset"
    }; # bind
    bind $g <Leave> {balloon unpost};
    bind $t <q> [list "tsdb_close" $t];
    bind $t <Q> [bind $t <q>];

    #
    # activate legend entries: pop-up menues for interactive configuration
    #
    set graph $g;
    set colours {white yellow orange red blue green black};
    set symbols {square circle diamond plus cross splus scross triangle void};
    foreach item [$graph element names] {

      set cmenu [menu ${graph}.${item}_colour -font {Helvetica 9} -tearoff no];
      foreach colour $colours {
        $cmenu add command -label $colour -foreground $colour \
          -command [list $graph element config $item -color $colour]
      }; # foreach

      set smenu [menu ${graph}.${item}_symbol -font {Helvetica 9} -tearoff no];
      foreach symbol $symbols {
        $smenu add command -label $symbol \
          -command [list $graph element config $item -symbol $symbol]
      }; # foreach

      $graph legend bind $item <Button-1> [list tk_popup $cmenu %X %Y];
      $graph legend bind $item <Button-2> [list tk_popup $smenu %X %Y];

      if {$item == "overlay" && $graphtype == "graph"} {
        set fmenu \
          [menu ${graph}.${item}_function -font {Helvetica 9} -tearoff no];
        set fcascade \
          [menu ${graph}.${item}_function.grade -font {Helvetica 9} -tearoff no]
        $fmenu add command -label custom \
          -command [list overlay_input $graph];
        $fmenu add separator;
        $fmenu add command -label hyperbolic -state disabled \
          -command [list overlay_regression $graph hyperbolic];
        $fmenu add command -label logarithmic -state disabled \
          -command [list overlay_regression $graph logarithmic];
        $fmenu add command -label linear \
          -command [list overlay_regression $graph linear];
        $fmenu add cascade -label polynomial \
          -menu $fcascade;
        $fcascade add command -label "2nd grade" \
          -command [list overlay_regression $graph 2];
        $fcascade add command -label "3rd grade" \
          -command [list overlay_regression $graph 3];
        for {set i 4} {$i <= 6} {incr i} {
          $fcascade add command -label "${i}th grade" \
            -command [list overlay_regression $graph $i];
        }; # for
        $fcascade add separator;
        $fcascade add command -label "automatic" \
          -command [list overlay_iterate_polynomial $graph];
        $fmenu add command -label exponential \
          -command [list overlay_regression $graph exponential];
        $graph legend bind $item <Button-3> [list tk_popup $fmenu %X %Y];
      }; # if
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

proc make_graph {graph scatterp} {
    ##
    ## Usage: make_graph graph
    ## 
    ## Uses the information in the global variables to make either a graph or a
    ## barchart.  'graph' is the name to be used for the graph/barchart.
    ##
    global globals graphtype graphoptions data axis element labels legend

    ## Create BLT "graph" or "barchart" according to 'graphtype'

    eval $graphtype $graph $graphoptions -bufferelements 0

    ## Make data vectors

    foreach index [array names data] {
      regsub -all "\\." $graph "_" vname
      set vname "v${vname}_$index"
      uplevel #0 "vector $vname";
      $vname set $data($index)
    }

    if {$graphtype == "graph" && $scatterp} {
      regsub -all "\\." $graph "_" name
      set xname "v${name}_x";
      set overlay "v${name}_o";
      uplevel #0 vector create ${overlay};
      $graph element create overlay -xdata $xname -ydata $overlay \
        -label "curve fit" -symbol diamond -pixel 3 -linewidth 1 \
        -color grey -fill defcolor -outline defcolor;
    }; # if

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
  set caption "(generated by $globals(name) at [current-time 1])";
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


proc overlay_input {graph {input ""} {entry ""} {marker ""} {focus ""}} {

  global globals;

  if {$input == ""} {
    set bg [$graph cget -bg];
    set input [tixLabelEntry $graph.input \
                 -relief ridge -bd 2 -label "f(x) = " \
                 -options [list label.font $globals(status_font) \
                           label.padx 0 label.pady 0 label.bg $bg\
                           entry.font $globals(input_font) \
                           entry.relief flat entry.highlightThickness 0 \
                           entry.width 15 entry.bg $bg]];
    [$input subwidget entry] config \
       -bg [[$input subwidget label] cget -bg];

    $graph legend config -hide yes;
    set marker [$graph marker create window -coords {-Inf Inf} -anchor nw \
                  -window $input -width 5c];
    set entry [$input subwidget entry];

    if {[info exists globals($graph,overlay)]} {
      set function $globals($graph,overlay);
      $entry insert 0 $function;
      $entry icursor end;
      $entry xview end;
    }; # if

    bind $entry <Return> { 
      set globals(errno) 0;
    }; # bind
    bind $entry <Control-g> {
      set globals(errno) 1;
    }; # bind
    bind $entry <Control-G> [bind $entry <Control-g>];
    bind $entry <Up> [list entry_history $entry overlay 1];
    bind $entry <Down> [list entry_history $entry overlay -1];
    history_move overlay end 1;

    set focus [focus -displayof .]
  }; # if 

  focus $entry
  grab set $entry
  tkwait variable globals(errno);
  grab release $entry
  focus $focus

  if {$globals(errno)} {
    destroy $input;
    $graph marker delete $marker;
    $graph legend config -hide no;
    plot_overlay $graph "";
    return;
  }; # if

  set function [$entry get];
  busy hold $graph;
  busy config $graph -cursor $globals(busy_cursor);
  update idletasks;
  set error [plot_overlay $graph $function "custom fit"];
  busy release $graph;
  if {$error < 0} {
    tsdb_beep;
    set globals(errno) 0;
    overlay_input $graph $input $entry $marker $focus;
  } else {
    destroy $input;
    $graph marker delete $marker;
    $graph legend config -hide no;
 
    history_add overlay $function;
  }; # else

}; # overlay_input()


proc overlay_iterate_polynomial {graph {lower 2} {upper 42}} {

  global globals;

  busy hold $graph;
  busy config $graph -cursor $globals(busy_cursor);
  update idletasks;

  set epsilon 1e-16;
  set best(grade) 0;
  for {set n $lower; set kaerb 0} {$n <= $upper} {incr n} {
    set solution [overlay_regression $graph $n return];
    if {$solution != ""} {
      set function "[lindex $solution 0] + [lindex $solution 1] * x";
      for {set i 2} {$i <= $n} {incr i} {
        append function " + [lindex $solution $i] * pow(x,$i)";
        if {[expr {abs([lindex $solution $i])}] < $epsilon} { 
          set kaerb 1;
        }; #if
      }; # for
      regsub -all "x" $function {$x} function;
      set pre [compute_pre $graph $function];
      if {!$best(grade) || $best(pre) < $pre} {
        set best(grade) $n;
        set best(pre) $pre;
        set best(function) $function;
      }; # if
      if {$kaerb} { break; }
      busy config $graph -cursor "plus";
      update idletasks;
      after 200;
      busy config $graph -cursor $globals(busy_cursor);
      update idletasks;
    }; # if
  }; # for

  if {$best(grade)} {
    regsub -all {\$x} $best(function) {x} function;
    plot_overlay $graph $function "polynomial ($best(grade))";
  }; # if
  busy release $graph;
  update idletasks;

}; # overlay_iterate_polynomial()


proc overlay_regression {graph {mode linear} {action "plot"} {xx ""} {yy ""}} {

  global globals;

  if {$action == "plot"} {
    busy hold $graph;
    busy config $graph -cursor $globals(busy_cursor);
    update idletasks;
  }; # if

  regsub -all "\\." $graph "_" name;
  if {$xx == ""} {
    set xx "v${name}_xx0";
  }; # if
  if {$yy == ""} {
    set yy "v${name}_yy0";
  }; # if
  if {[$xx length] != [$yy length]} {
    return "";
  }; # if
  set length [$xx length];

  if {$mode == "linear"} {
    vector create mean(1);
    mean expr mean($xx);
    set xmean $mean(0);
    mean expr mean($yy);
    set ymean $mean(0);

    vector create foo($length) bar($length) baz($length);

    foo expr {$xx - $xmean};
    bar expr {$yy - $ymean};
    baz expr {foo * bar};
    baz expr {sum(baz)};
    set xysum $baz(0);
    baz expr {foo * foo}; 
    baz expr {sum(baz)};
    set xxsum $baz(0);
    set b [expr {$xysum / $xxsum}];
    set a [expr {$ymean - $b * $xmean}];
    if {$action == "plot"} {
      plot_overlay $graph "$a + $b * x" "linear";
    } else {
      return [list $a $b];
    }; # else

  } elseif {[regexp {[1-9]} $mode]} {

    vector create foo(1) bar($length);
    set width [expr {$mode + 1}];
    set cells [expr {2 * $mode}];

    
    set matrix(1,1) $length;
    foo expr {sum($yy)};
    set matrix(1,[expr {$width + 1}]) $foo(0);

    set bar(:) 1.0;
    for {set i 1} {$i <= $mode} {incr i} {
      bar expr {bar * $xx};
      foo expr {sum(bar)};
      set matrix(1,[expr {$i + 1}]) $foo(0);
      for {set m 2; set n $i} {$n >= 1} {incr m; incr n -1} {
        set matrix($m,$n) $foo(0);
      }; # for
      foo expr {sum($yy * bar)};
      set matrix([expr {$i + 1}],[expr {$width + 1}]) $foo(0);
    }; # for
    for {set i $width; set j 2} {$i <= $cells} {incr i; incr j} {
      bar expr {bar * $xx};
      foo expr {sum(bar)};
      set matrix($j,$width) $foo(0);
      for {set m [expr {$j + 1}]; set n $mode} \
          {$m <= $width} \
          {incr m; incr n -1} {
        set matrix($m,$n) $foo(0);
      }; # for
    }; # for

    if {![catch {gauss_eliminate matrix x $width}]} {
      if {$action == "plot"} {
        set function "$x(1) + $x(2) * x";
        for {set i 3} {$i <= $width} {incr i} {
          append function " + $x($i) * pow(x,[expr {$i - 1}])";
        }; # for
        plot_overlay $graph $function "polynomial ($mode)";
      } else {
        for {set i 1} {$i <= $width} {incr i} {
          lappend result $x($i);
        }; # for
        return $result;
      }; # else
    }; # if
  } elseif {$mode == "exponential"} {

    $xx variable xxs;
    $yy variable yys;
    vector create xxx($length) yyy($length);
    for {set i 0} {$i < $length} {incr i} {
      if {$yys($i) > 0} {
        set xxx($i) $xxs($i);
        set yyy($i) [expr {log10($yys($i))}];
      }; # if
    }; # for
    xxx length $i;
    yyy length $i;
    set solution [overlay_regression $graph linear return xxx yyy];
    if {$solution != ""} {
      set a [expr {pow(10,[lindex $solution 0])}];
      set b [expr {pow(10,[lindex $solution 1])}];
      if {$action == "plot"} {
        plot_overlay $graph "$a + pow($b,x)" \
          "exponential" xxx yyy;
      } else {
        return {$a $b};
      }; # else
    }; # if
  }; # elseif

  if {$action == "plot"} {  
    busy release $graph;
    update idletasks;
  }; # if

  return "";

}; # overlay_regression()


proc compute_pre {graph function {xx ""} {yy ""}} {

  regsub -all "\\." $graph "_" name;
  if {$xx == ""} { set xx "v${name}_xx0"; };
  if {$yy == ""} { set yy "v${name}_yy0"; };
  $xx variable xxs;
  $yy variable yys;

  if {[$xx length] != [$yy length]} {
    return "";
  }; # if

  set length [$xx length];
  vector create foo(1);
  foo expr mean($yy);
  set ymean $foo(0);

  vector create error1($length);
  vector create error2($length);

  error1 expr {($yy - $ymean) * ($yy - $ymean)};
  foo expr {sum(error1)};
  set e1 $foo(0);

  for {set i 0} {$i < $length} {incr i} {
    set x $xxs($i);
    if {[catch {expr $function} y]} {
      return "";
    }; # if
    set error2($i) [expr {pow($yys($i) - $y,2)}];
  }; # for
  
  foo expr {sum(error2)};
  set e2 $foo(0);

  return [expr {($e1 -$e2) / $e1}];

}; # compute_pre()


proc gauss_eliminate {input output n} {

  upvar 1 $input matrix;
  upvar 1 $output x;

#  for {set i 1} {$i <= $n} {incr i} {
#    for {set k $i} {$k <= [expr {$n + 1}]} {incr k} {
#      puts "($i,$k) $matrix($i,$k)"
#    }; # for
#  }; # for
#  return "";
  for {set i 1} {$i <= $n} {incr i} {
    set max $i;
    for {set j [expr {$i + 1}]} {$j <= $n} {incr j} {
      if {abs($matrix($j,$i)) > abs($matrix($max,$i))} {
        set max $j;
      }; # if
    }; # for
    for {set k $i} {$k <= [expr {$n + 1}]} {incr k} {
      set t $matrix($i,$k);
      set matrix($i,$k) $matrix($max,$k);
      set matrix($max,$k) $t;
    }; # for
    for {set j [expr {$i + 1}]} {$j <= $n} {incr j} {
      for {set k [expr {$n + 1}]} {$k >= $i} {incr k -1} {
        set t [expr {$matrix($i,$k) * $matrix($j,$i) / $matrix($i,$i)}];
        set matrix($j,$k) [expr $matrix($j,$k) - $t];
      }; # for
    }; # for
  }; # for

  for {set i $n} {$i >= 1} {incr i -1} {
    set t 0.0;
    for {set j [expr {$i + 1}]} {$j <= $n} {incr j} {
      set t [expr {$t + ($matrix($i,$j) * $x($j))}];
    }; # for
    set x($i) [expr {($matrix($i,[expr {$n + 1}]) - $t) / $matrix($i,$i)}];
  }; # for
}; # gauss_eliminate()


proc plot_overlay {graph {function {pow($x,2)}} {legend ""} {xx ""} {yy ""}} {

  global globals;

  regsub -all "\\." $graph "_" name;
  set xname "v${name}_x";
  $xname variable xs;
  if {![catch {$xname length} length] && $length > 0} {
    if {$legend == ""} { set legend $function; }
    set readable $function;
    regsub -all "exp" $function "_exp" function;
    regsub -all "x" $function {$x} function;
    regsub -all "_exp_" $function "exp" function;
    set yname "v${name}_o";
    $yname variable ys;
    $yname length $length
    if {$function != ""} {
      for {set i 0} {$i < $length} {incr i} {
        set x $xs($i);
        if {[catch {expr $function} y]} {
          return -2;
        }; # if
        set ys($i) $y;
      }; # for
      set globals($graph,overlay) $readable;
      set pre [compute_pre $graph $function $xx $yy];
      if {$pre != ""} {
        set label [format {%s  [%.2f]} $legend $pre];
      } else {
        set label $legend;
      }; # else
    } else {
      $yname set "";
      set label "curve fit";
    }; # else
    $graph element config overlay -xdata $xname -ydata $yname \
      -label $label \
      -color [expr {$function == "" ? "gray" : "red"}];
    adjust_graph_view $graph 2;
    logger "(status: $graph `$readable' overlay)";
    update idletasks;
    after 500;
    return 0;
  } else {
    return -1;
  }; 
}; # plot_overlay()


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
    set i [string range $point(name) 2 end];
    regsub -all "\\." $graph "_" name
    set ids "v${name}_ids$i"
    if {![catch {$ids variable vector}]} {
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

proc adjust_graph_view {graph button {x ""} {y ""}} {

  global globals;

  if {$button == 2} {
    $graph axis config x -min {} -max {};
    $graph axis config y -min {} -max {};
    #
    # kludge (2-nov-98): adjust y axis to leave extra space at the bottom and
    # position copyright notice; needs improvement ...
    #
    set foo [$graph axis limits y];
    set xmin [lindex $foo 0];
    set xmax [lindex $foo 1];
    if {[$graph axis cget y -logscale]} {
      set pad 0;
    } else {
      set pad [expr ($xmax - $xmin) * 0.05];
    }; # else
    $graph axis config y -min [expr $xmin - $pad] -max $xmax;
  } else {
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
  }; # else
}; # adjust_graph_view()
