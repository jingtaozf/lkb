proc yy_initialize {} {

  global globals;

#  .menu.file.menu entryconfig "Export" \
#    -state normal -command yy_export_results;

  #
  # create entry pane for (YY) result filter parameters
  #
  set grey lightgrey;
  frame .status.filter -bd 0 -bg $grey;
  frame .status.filter.fill0 -bd 0 -bg $grey;
  frame .status.filter.fill1 -bd 0 -bg $grey;
  frame .status.filter.fill2 -bd 0 -bg $grey;
  frame .status.filter.fill3 -bd 0 -bg $grey;

  set loptions [list -font $globals(status_font) -padx 0 -pady 0 -bg $grey];
  set eoptions [list -justify right -font $globals(input_font) \
                     -bd 1 -relief solid -width 3 -highlightthickness 0 \
                     -bg white];
  eval label .status.filter.lk2y -text {{K2Y threshold }} $loptions;
  eval entry .status.filter.ek2y $eoptions;
  eval label .status.filter.lrts -text {{RTs threshold }} $loptions;
  eval entry .status.filter.erts $eoptions;
  eval label .status.filter.lratio -text {{K2Y to RTs ratio }} $loptions;
  eval entry .status.filter.eratio $eoptions;

  place .status.filter -in .status.dummy -relwidth 1 -relheight 1;
  pack .status.filter.fill0 -side left -fill both -expand yes;
  pack .status.filter.lk2y -side left;
  pack .status.filter.ek2y -side left;
  pack .status.filter.fill1 -side left -fill both -expand yes;
  pack .status.filter.lrts -side left;
  pack .status.filter.erts -side left;
  pack .status.filter.fill2 -side left -fill both -expand yes;
  pack .status.filter.lratio -side left;
  pack .status.filter.eratio -side left;
  pack .status.filter.fill3 -side left -fill both -expand yes;
  lower .status.filter;

  #
  # add result filter command into `Options' menu
  #
  set last [.menu.options.menu index "Switches"];
  .menu.options.menu insert $last command -label "Filter Parameters" \
    -command "yy_filter_input";
  .menu.options.menu insert [incr last] separator;

}; # yy_initialize()


proc yy_export_results {} {

  global globals test_suites;

  if {[verify_ts_selection]} {return 1};
  set skeleton [lindex [file split $globals(data)] 2];
  if {![input "directory:" "/var/www/html/rte/lib/$skeleton" "" directory]} {
    set target $globals(input);
    
    if {[catch {file mkdir $target}]} {
      tsdb_beep;
      status "error creating target directory `$target'" 10;
      return;
    }; # if
    if {![file writable $target]} {
      tsdb_beep;
      status "target directory `$target' not writable" 10;
      return;
    }; # if
    set index [file join $target "Index"];
    if {[file exists $index]
        && [yes-or-no-p "overwrite non-empty `$target'"] != 1} {
      return;
    }; # if
    history_add directory $target;
    set command "(export :yy \"$globals(data)\" \"$target\")";
    send_to_lisp :event $command;
  }; # if

}; # yy_export_results()


set globals(k2y_relation_font) {Helvetica 14 bold};
set globals(k2y_bracket_font) {Helvetica 16};
set globals(k2y_attribute_font) {Helvetica 14};
set globals(k2y_value_font) {Helvetica 14 italic};

proc showk2y {{file "/tmp/foo.k2y"} {container ".k2y"} {title ""}} {

  global globals relations contents;

  if {$title == ""} {
    set title [format "untitled K2Y view"]
  }; # if

  if {[read_k2y $file]} {
    set t [toplevel $container];
    set f [frame $t.frame];
    set c [canvas $f.canvas];
    scrollbar $f.scrollx -command "$c xview" -orient horiz;
    scrollbar $f.scrolly -command "$c yview";
    $c configure -yscrollcommand "$f.scrolly set";
    $c configure -xscrollcommand "$f.scrollx set";
    bind $f.scrolly <FocusIn> {tk_focusContinue %W};
    bind $f.scrollx <FocusIn> {tk_focusContinue %W};
    table $f 0,0 $c -fill both
    table $f \
      0,1 $f.scrolly -fill y \
      1,0 $f.scrollx -fill x
    table configure $f r1 c1 -resize none

    $c bind all <Enter> "k2y_highlight_by_clause $c"
    $c bind all <Leave> "k2y_unhighlight $c"

    set x 0;
    set y 0;
    foreach i [array names relations] {
      set id "";
      if {[set foo [lsearch $contents($i) "ID"]] >= 0} {
        set id "id_[lindex $contents($i) [incr foo]]";
      }; # if
      if {[set foo [lsearch $contents($i) "CLAUSE"]] >= 0} {
        set clause "clause_[lindex $contents($i) [incr foo]]";
      }; # if
      set item [$c create text $x $y -anchor ne \
                          -tag "relation $id $clause" \
                          -text "$relations($i)" \
                          -font $globals(k2y_relation_font)];
      set first $item;
      set bbox [$c bbox $item];
      set by [expr {$y + ([lindex $bbox 3] - [lindex $bbox 1]) / 2 - 1}];
      set x [compute_x $c $item 1];
      set item [$c create text $x $by -anchor w \
                          -tag "sbracket $id $clause" \
                          -text {[} \
                          -font $globals(k2y_bracket_font)];

      set firstp true;
      foreach {attribute value} $contents($i) {
        if {$firstp} {
          set x [compute_x $c $item 1];
        } else {
          set x [compute_x $c $item 1];
          set item [$c create text $x $y -anchor nw \
                              -tag "abracket $id $clause" \
                              -text {;} \
                              -font $globals(k2y_relation_font)];
          set x [compute_x $c $item 6];
        }; # else
        set item [$c create text $x $y -anchor nw \
                            -tag "attribute $id $clause" \
                            -text $attribute \
                            -font $globals(k2y_attribute_font)];
        if {$attribute == "ID"} {
          $c bind $item <Enter> "k2y_highlight_by_id $c";
        }; # if
        set x [compute_x $c $item 2];
        if {$attribute == "RA"} {
          set item [$c create text $x $by -anchor w \
                              -tag "abracket $id $clause" \
                              -text {<} \
                              -font $globals(k2y_attribute_font)];
          set x [compute_x $c $item 1];
        }; # if
        set item [$c create text $x $y -anchor nw \
                            -tag "value $attribute $id $clause" \
                            -text $value \
                            -font $globals(k2y_value_font)];
        if {$attribute == "ID"} {
          $c bind $item <Enter> "k2y_highlight_by_id $c";
        }; # if
        if {$attribute == "RA"} {
          set x [compute_x $c $item 1];
          set item [$c create text $x $by -anchor w \
                              -tag "abracket $id $clause" \
                              -text {>} \
                              -font $globals(k2y_attribute_font)];
        }; # if
        set firstp false;
      }; # foreach

      set x [compute_x $c $item 1];
      set item [$c create text $x $by -anchor w \
                                      -tag "sbracket $id $clause" \
                          -text {]} \
                          -font $globals(k2y_bracket_font)];
      set x 0;
      set bbox [$c bbox $first $item];
      set y [expr {[lindex $bbox 3] + \
                   ([lindex $bbox 3] - [lindex $bbox 1]) * 0.4}];

    }; # foreach
  }; # if

  set bbox [$c bbox all];
  set x [expr [lindex $bbox 2] - 10];
  set y [expr [lindex $bbox 3] + 4];
  set caption "(generated by $globals(name) at [current-time 1])";
  $c create text $x $y -anchor ne -text $caption \
            -font $globals(copyright_font);

  set bbox [$c bbox all];
  $c configure -scrollregion $bbox;
  $c configure -height [expr [lindex $bbox 3] - [lindex $bbox 1]];
  $c configure -width [expr [lindex $bbox 2] - [lindex $bbox 0]];
  $c xview moveto 0
  $c yview moveto 0
  $c configure -relief solid -bg white -bd 1

  button $t.quit -text "Close" -command [list "tsdb_close" $t];
  button $t.latex -text "LaTeX" -command [list "tsdb_latex" $t];
  button $t.print -text "PostScript" \
    -command [list make_postscript $c "k2y"];
  table $t \
    0,0 $f -cspan 3 -fill both -padx 5 -pady 5\
    1,0 $t.quit -pady {5 10} \
    1,1 $t.latex -pady {5 10} \
    1,2 $t.print -pady {5 10}

  catch {
      bind $t <Prior> [list $c yview scroll -1 page];
      bind $t <Next> [list $c yview scroll 1 page];
      bind $t <BackSpace> [list $c yview scroll -1 page];
      bind $t <Delete> [list $c yview scroll -1 page];
      bind $t <space> [list $c yview scroll 1 page];
      bind $t <Control-v> [list $c yview scroll 1 page];
      bind $t <Control-V> [bind $t <Control-v>];
      bind $t <Alt-v> [list $c yview scroll -1 pages];
      bind $t <Alt-V> [bind $t <Alt-v>];
      bind $t <Meta-v> [list $c yview scroll -1 pages];
      bind $t <Meta-V> [bind $t <Meta-v>];
      bind $t {<Escape> <v>} [list $c yview scroll -1 pages];
      bind $t <Return> [list $c yview scroll 1 units];
      bind $t <Down> [list $c yview scroll 1 units];
      bind $t <Control-n> [list $c yview scroll 1 units];
      bind $t <Control-N> [bind $t  <Control-n>];
      bind $t <Up> [list $c yview scroll -1 units];
      bind $t <Control-p> [list $c yview scroll -1 units];
      bind $t <Control-P> [bind $t  <Control-p>];
      bind $t <Shift-Prior> [list $c yview moveto 0];
      bind $t <Shift-Next> [list $c yview moveto 1];
      bind $t <Control-Prior> [list $c yview moveto 0];
      bind $t <Control-Next> [list $c yview moveto 1];
      bind $t <Alt-Prior> [list $c yview moveto 0];
      bind $t <Alt-Next> [list $c yview moveto 1];
      bind $t <Meta-Prior> [list $c yview moveto 0];
      bind $t <Meta-Next> [list $c yview moveto 1];

      bind $t <Left> [list $c xview scroll -1 units];
      bind $t <Right> [list $c xview scroll 1 units];
      bind $t <Control-b> [list $c xview scroll -1 units];
      bind $t <Control-B> [bind $t  <Control-b>];
      bind $t <Control-f> [list $c xview scroll 1 units];
      bind $t <Control-F> [bind $t  <Control-f>];
      bind $t <Shift-Left> [list $c xview scroll -1 pages];
      bind $t <Shift-Right> [list $c xview scroll 1 pages];
      bind $t <Control-Left> [list $c xview scroll -1 pages];
      bind $t <Control-Right> [list $c xview scroll 1 pages];
      bind $t <Alt-Left> [list $c xview scroll -1 pages];
      bind $t <Alt-Right> [list $c xview scroll 1 pages];
      bind $t <Meta-Left> [list $c xview scroll -1 pages];
      bind $t <Meta-Right> [list $c xview scroll 1 pages];
      bind $t <Home> [list $c xview moveto 0];
      bind $t <End> [list $c xview moveto 1];
      bind $t <Control-a> [list $c xview moveto 0];
      bind $t <Control-A> [bind $t  <Control-a>];
      bind $t <Control-e> [list $c xview moveto 1];
      bind $t <Control-E> [bind $t  <Control-e>];
      bind $t <q> [list "tsdb_close" $t];
      bind $t <Q> [bind $toplevel <q>];
  }; # catch

  set maxwidth [expr int([winfo screenwidth $t] * 0.95)];
  set maxheight [expr int([winfo screenheight $t] * 0.95)];
  set width [expr {[lindex $bbox 2] - [lindex $bbox 0] + 42}];
  set height [expr {[lindex $bbox 3] - [lindex $bbox 1] + 88}];
  if {$width > $maxwidth} {
    set width $maxwidth;
  }; # if
  if {$height > $maxheight} {
    set height $maxheight;
  }; # if
  wm geometry $t "${width}x${height}"
  wm title $t $title
  wm iconname $t $title
  tkwait visibility $t
  update idletasks

  return [format "(:k2y (:toplevel . \"%s\") (:canvas . \"%s\"))" $t $c];

}; # showk2y()


proc read_k2y {file} {

  global relations contents;

  catch {unset relations};
  catch {unset contents};

  if {[catch {set stream [open $file r]}] != 0} {
    error "read_k2y(): unable to read '$file'";
    return 0;
  }; # if

  while {![eof $stream]} {
    set line [gets $stream];
    if {$line == "" || [string index $line 0] == "#"} {
      continue;
    }; # if
    set command [lindex $line 0]
    switch $command {
      relation {
        set index [lindex $line 1];
        set relations($index) [lindex $line 2];
        set contents($index) [lindex $line 3];
      }
    }; # switch
  }; # while
  close $stream;
  return 1;

}; # read_k2y()


proc compute_x {canvas item offset} {
  set bbox [$canvas bbox $item];
  set x [expr {[lindex $bbox 2] + $offset}];
  return $x;
}; # compute_x()


proc k2y_highlight_by_clause {canvas} {

  set item [$canvas find withtag current];
  set tags [$canvas itemcget $item -tag];
  if {[regexp -- {clause_([a-zA-Z0-9]+)} $tags foo id] == 1} {
    $canvas itemconfig id_$id -fill red;
  }; # if

}; # k2y_highlight_by_id()


proc k2y_highlight_by_id {canvas} {

  set item [$canvas find withtag current];
  set tags [$canvas itemcget $item -tag];
  if {[regexp -- {id_([a-zA-Z0-9]+)} $tags foo id] == 1} {
    $canvas itemconfig clause_$id -fill blue;
    if {[$canvas find withtag clause_$id] != ""} {
      $canvas itemconfig id_$id -fill orange;
    }; # if
  }; # if

}; # k2y_highlight_by_id()


proc k2y_unhighlight {canvas} {
  $canvas itemconfig all -fill black;
}; # k2y_unhighlight()


set globals(rt_role_font) {Helvetica 14 bold};
set globals(rt_bracket_font) {Helvetica 16};
set globals(rt_modifier_font) {Helvetica 14 italic};
set globals(rt_value_font) {Helvetica 14 italic};

proc showrt {{file "/tmp/test.rt"} {container ".rt"} {title ""}} {

  global globals rts roles;

  if {$title == ""} {
    set title [format "untitled Role Table view"]
  }; # if

  if {[read_rt $file]} {
    set t [toplevel $container];
    set f [frame $t.frame];
    set c [canvas $f.canvas];
    scrollbar $f.scrollx -command "$c xview" -orient horiz;
    scrollbar $f.scrolly -command "$c yview";
    $c configure -yscrollcommand "$f.scrolly set";
    $c configure -xscrollcommand "$f.scrollx set";
    bind $f.scrolly <FocusIn> {tk_focusContinue %W};
    bind $f.scrollx <FocusIn> {tk_focusContinue %W};
    table $f 0,0 $c -fill both
    table $f \
      0,1 $f.scrolly -fill y \
      1,0 $f.scrollx -fill x
    table configure $f r1 c1 -resize none

    set x 0;
    set y 0;
    foreach i [lsort -integer [array names rts]] {
      foreach j [lsort -dictionary [array names roles]] {
        if {[string first "$i," "$j"]} continue;
        set role $roles($j);
        set item [$c create text $x $y -anchor ne \
                            -tag [list "role" "($j)"] \
                            -text [lindex $role 0] \
                            -font $globals(rt_role_font)];
        set first $item;
        set bbox [$c bbox $item];
        set x [compute_x $c $item 5];
        set by [expr {$y + ([lindex $bbox 3] - [lindex $bbox 1]) / 2 - 1}];
        set item [$c create text $x $by -anchor w \
                  -tag [list "abracket" "($j)"] \
                  -text {[} \
                  -font $globals(rt_bracket_font)];
        set firstp true;
        foreach {modifier} [lindex $role 1] {
          if {$firstp} {
            set x [compute_x $c $item 1];
          } else {
            set x [compute_x $c $item 6];
          }; # else
          set item [$c create text $x $y -anchor nw \
                              -tag [list "role" "($j)"] \
                              -text "$modifier" \
                              -font $globals(rt_modifier_font)];
          set firstp false;
        }; # foreach

        set x [compute_x $c $item 1];
        set item [$c create text $x $by -anchor w \
                  -tag [list "abracket" "($j)"] \
                  -text {]} \
                  -font $globals(rt_bracket_font)];

        set x [compute_x $c $item 5];
        set item [$c create text $x $by -anchor w \
            -tag [list "abracket" "($j)"] \
            -text "[lindex $role 2]" \
            -font $globals(rt_value_font)];
        set x 0;
        set bbox [$c bbox $first $item];
        set y [expr {[lindex $bbox 3] + \
                     ([lindex $bbox 3] - [lindex $bbox 1]) * 0.2}];

     }; # foreach
     set y [expr {$y + ([lindex $bbox 3] - [lindex $bbox 1]) * 0.6}];

    }; # foreach
  }; # if

  set bbox [$c bbox all];
  set x [expr [lindex $bbox 2] - 10];
  set y [expr [lindex $bbox 3] + 4];
  set caption "(generated by $globals(name) at [current-time 1])";
  $c create text $x $y -anchor ne -text $caption \
            -font $globals(copyright_font);

  set bbox [$c bbox all];
  $c configure -scrollregion $bbox;
  $c configure -height [expr [lindex $bbox 3] - [lindex $bbox 1]];
  $c configure -width [expr [lindex $bbox 2] - [lindex $bbox 0]];
  $c xview moveto 0
  $c yview moveto 0
  $c configure -relief solid -bg white -bd 1

  button $t.quit -text "Close" -command [list "tsdb_close" $t];
  button $t.latex -text "LaTeX" -command [list "tsdb_latex" $t];
  button $t.print -text "PostScript" \
    -command [list make_postscript $c "rt"];
  table $t \
    0,0 $f -cspan 3 -fill both -padx 5 -pady 5\
    1,0 $t.quit -pady {5 10} \
    1,1 $t.latex -pady {5 10} \
    1,2 $t.print -pady {5 10}

  catch {
      bind $t <Prior> [list $c yview scroll -1 page];
      bind $t <Next> [list $c yview scroll 1 page];
      bind $t <BackSpace> [list $c yview scroll -1 page];
      bind $t <Delete> [list $c yview scroll -1 page];
      bind $t <space> [list $c yview scroll 1 page];
      bind $t <Control-v> [list $c yview scroll 1 page];
      bind $t <Control-V> [bind $t <Control-v>];
      bind $t <Alt-v> [list $c yview scroll -1 pages];
      bind $t <Alt-V> [bind $t <Alt-v>];
      bind $t <Meta-v> [list $c yview scroll -1 pages];
      bind $t <Meta-V> [bind $t <Meta-v>];
      bind $t {<Escape> <v>} [list $c yview scroll -1 pages];
      bind $t <Return> [list $c yview scroll 1 units];
      bind $t <Down> [list $c yview scroll 1 units];
      bind $t <Control-n> [list $c yview scroll 1 units];
      bind $t <Control-N> [bind $t  <Control-n>];
      bind $t <Up> [list $c yview scroll -1 units];
      bind $t <Control-p> [list $c yview scroll -1 units];
      bind $t <Control-P> [bind $t  <Control-p>];
      bind $t <Shift-Prior> [list $c yview moveto 0];
      bind $t <Shift-Next> [list $c yview moveto 1];
      bind $t <Control-Prior> [list $c yview moveto 0];
      bind $t <Control-Next> [list $c yview moveto 1];
      bind $t <Alt-Prior> [list $c yview moveto 0];
      bind $t <Alt-Next> [list $c yview moveto 1];
      bind $t <Meta-Prior> [list $c yview moveto 0];
      bind $t <Meta-Next> [list $c yview moveto 1];

      bind $t <Left> [list $c xview scroll -1 units];
      bind $t <Right> [list $c xview scroll 1 units];
      bind $t <Control-b> [list $c xview scroll -1 units];
      bind $t <Control-B> [bind $t  <Control-b>];
      bind $t <Control-f> [list $c xview scroll 1 units];
      bind $t <Control-F> [bind $t  <Control-f>];
      bind $t <Shift-Left> [list $c xview scroll -1 pages];
      bind $t <Shift-Right> [list $c xview scroll 1 pages];
      bind $t <Control-Left> [list $c xview scroll -1 pages];
      bind $t <Control-Right> [list $c xview scroll 1 pages];
      bind $t <Alt-Left> [list $c xview scroll -1 pages];
      bind $t <Alt-Right> [list $c xview scroll 1 pages];
      bind $t <Meta-Left> [list $c xview scroll -1 pages];
      bind $t <Meta-Right> [list $c xview scroll 1 pages];
      bind $t <Home> [list $c xview moveto 0];
      bind $t <End> [list $c xview moveto 1];
      bind $t <Control-a> [list $c xview moveto 0];
      bind $t <Control-A> [bind $t  <Control-a>];
      bind $t <Control-e> [list $c xview moveto 1];
      bind $t <Control-E> [bind $t  <Control-e>];
      bind $t <q> [list "tsdb_close" $t];
      bind $t <Q> [bind $toplevel <q>];
  }; # catch

  set maxwidth [expr int([winfo screenwidth $t] * 0.95)];
  set maxheight [expr int([winfo screenheight $t] * 0.95)];
  set width [expr {[lindex $bbox 2] - [lindex $bbox 0] + 42}];
  set height [expr {[lindex $bbox 3] - [lindex $bbox 1] + 88}];
  if {$width > $maxwidth} {
    set width $maxwidth;
  }; # if
  if {$height > $maxheight} {
    set height $maxheight;
  }; # if
  wm geometry $t "${width}x${height}"
  wm title $t $title
  wm iconname $t $title
  tkwait visibility $t
  update idletasks

  return [format "(:rt (:toplevel . \"%s\") (:canvas . \"%s\"))" $t $c];

}; # showrt()


proc read_rt {file} {

  global rts roles;

  catch {unset rts};
  catch {unset roles};

  if {[catch {set stream [open $file r]}] != 0} {
    error "read_rt(): unable to read '$file'";
    return 0;
  }; # if

  set ntables 0;
  while {![eof $stream]} {
    set line [gets $stream];
    if {$line == "" || [string index $line 0] == "#"} {
      continue;
    }; # if
    set command [lindex $line 0]
    switch $command {
      table {
        set rts([incr ntables]) true;
      }
      role {
        set index [lindex $line 1];
        set roles($ntables,$index) [lindex $line 2];
      }
    }; # switch
  }; # while
  close $stream;
  return 1;

}; # read_rt()


set globals(yy_filter_k2y) "";
set globals(yy_filter_rts) "";
set globals(yy_filter_ratio) "";

proc yy_filter_input {{set 1}} {

  global globals;

  .status.filter.ek2y delete 0 end;
  .status.filter.ek2y insert 0 $globals(yy_filter_k2y);
  .status.filter.erts delete 0 end;
  .status.filter.erts insert 0 $globals(yy_filter_rts);
  .status.filter.eratio delete 0 end;
  .status.filter.eratio insert 0 $globals(yy_filter_ratio);

  #
  # <Tab> and shifted variants cycle (both directions) through entry fields
  #
  bind .status.filter.ek2y <Tab> {
    if {[entry_validate .status.filter.ek2y 1 "" "K2Y threshold"]} {
      focus .status.filter.erts;
    }; # if
  }; # bind
  bind .status.filter.erts <Tab> {
    if {[entry_validate .status.filter.erts 1 "" "RTs threshold"]} {
      focus .status.filter.eratio;
    }; # if
  }; # bind
  bind .status.filter.eratio <Tab> {
    if {[entry_validate .status.filter.eratio 0 "" "K2Y to RTs ratio"]} {
      focus .status.filter.ek2y;
    }; # if
  }; # bind

  bind .status.filter.ek2y <Control-Tab> {
    if {[entry_validate .status.filter.ek2y 1 "" "K2Y threshold"]} {
      focus .status.filter.eratio;
    }; # if
  }; # bind
  bind .status.filter.ek2y <Alt-Tab> \
    [bind .status.filter.ek2y <Control-Tab>];
  bind .status.filter.ek2y <Shift-Tab> \
    [bind .status.filter.ek2y <Control-Tab>];
  catch {
    bind .status.filter.ek2y <ISO_Left_Tab> \
      [bind .status.filter.ek2y <Control-Tab>];
  }; # catch
  bind .status.filter.erts <Control-Tab> {
    if {[entry_validate .status.filter.erts 0 "" "RTs threshold"]} {
      focus .status.filter.ek2y;
    }; # if
  }; # bind
  bind .status.filter.erts <Alt-Tab> \
    [bind .status.filter.erts <Control-Tab>];
  bind .status.filter.erts <Alt-Tab> \
    [bind .status.filter.erts <Control-Tab>];
  bind .status.filter.erts <Shift-Tab> \
    [bind .status.filter.erts <Control-Tab>];
  catch {
    bind .status.filter.erts <ISO_Left_Tab> \
     [bind .status.filter.erts <Control-Tab>];
  }; # catch

  bind .status.filter.eratio <Control-Tab> {
    if {[entry_validate .status.filter.eratio 0 "" "K2Y to RTs ratio"]} {
      focus .status.filter.erts;
    }; # if
  }; # bind
  bind .status.filter.eratio <Alt-Tab> \
    [bind .status.filter.eratio <Control-Tab>];
  bind .status.filter.eratio <Shift-Tab> \
    [bind .status.filter.eratio <Control-Tab>];
  catch {
    bind .status.filter.eratio <ISO_Left_Tab> \
      [bind .status.filter.eratio <Control-Tab>];
  }; # catch

  #
  # cursor <Up> and <Down> increment and decrement values; shifted variants
  # give steps of 10
  #
  bind .status.filter.ek2y <Up> {
    entry_incr .status.filter.ek2y 1 1 "" "K2Y threshold";
    raise .status.filter;
  }; # bind
  bind .status.filter.ek2y <Down> {
    entry_incr .status.filter.ek2y -1 1 "" "K2Y threshold";
    raise .status.filter;
  }; # bind
  bind .status.filter.ek2y <Shift-Up> {
    entry_incr .status.filter.ek2y 10 1 "" "K2Y threshold";
    raise .status.filter;
  }; # bind
  bind .status.filter.ek2y <Shift-Down> {
    entry_incr .status.filter.ek2y -10 1 "" "K2Y threshold";
    raise .status.filter;
  }; # bind
  bind .status.filter.ek2y <Control-Down> \
    [bind .status.filter.ek2y <Shift-Down>];
  bind .status.filter.ek2y <Control-Up> \
    [bind .status.filter.ek2y <Shift-Up>];
  bind .status.filter.ek2y <Alt-Down> \
    [bind .status.filter.ek2y <Shift-Down>];
  bind .status.filter.ek2y <Alt-Up> \
    [bind .status.filter.ek2y <Shift-Up>];

  bind .status.filter.erts <Up> {
    entry_incr .status.filter.erts 1 1 "" "RTs threshold";
    raise .status.filter;
  }; # bind
  bind .status.filter.erts <Down> {
    entry_incr .status.filter.erts -1 1 "" "RTs threshold";
    raise .status.filter;
  }; # bind
  bind .status.filter.erts <Shift-Up> {
    entry_incr .status.filter.erts 10 1 "" "RTs threshold";
    raise .status.filter;
  }; # bind
  bind .status.filter.erts <Shift-Down> {
    entry_incr .status.filter.erts -10 1 "" "RTs threshold";
    raise .status.filter;
  }; # bind
  bind .status.filter.erts <Control-Down> \
    [bind .status.filter.erts <Shift-Down>];
  bind .status.filter.erts <Control-Up> \
    [bind .status.filter.erts <Shift-Up>];
  bind .status.filter.erts <Alt-Down> \
    [bind .status.filter.erts <Shift-Down>];
  bind .status.filter.erts <Alt-Up> \
    [bind .status.filter.erts <Shift-Up>];

  bind .status.filter.eratio <Up> {
    entry_incr .status.filter.eratio 1 0 "" "K2Y to RTs ratio";
    raise .status.filter;
  }; # bind
  bind .status.filter.eratio <Down> {
    entry_incr .status.filter.eratio -1 0 "" "K2Y to RTs ratio";
    raise .status.filter;
  }; # bind
  bind .status.filter.eratio <Shift-Up> {
    entry_incr .status.filter.eratio 10 1 "" "K2Y to RTs ratio";
    raise .status.filter;
  }; # bind
  bind .status.filter.eratio <Shift-Down> {
    entry_incr .status.filter.eratio -10 1 "" "K2Y to RTs ratio";
    raise .status.filter;
  }; # bind
  bind .status.filter.eratio <Control-Down> \
    [bind .status.filter.eratio <Shift-Down>];
  bind .status.filter.eratio <Control-Up> \
    [bind .status.filter.eratio <Shift-Up>];
  bind .status.filter.eratio <Alt-Down> \
    [bind .status.filter.eratio <Shift-Down>];
  bind .status.filter.eratio <Alt-Up> \
    [bind .status.filter.eratio <Shift-Up>];

  #
  # attach additional tag to all filter input widgets; makes life easier
  #
  foreach widget {lk2y ek2y lrts erts lratio eratio} {
    bindtags .status.filter.$widget \
      [concat [bindtags .status.filter.$widget] yy_filter_input];
  }; # foreach

  #
  # <Return> terminates filter parameter input: read out values from entry
  # fields, validate, and store into global variables. <Control-G> aborts.
  #
  bind yy_filter_input <Return> { 
    if {[entry_validate .status.filter.ek2y 1 "" "K2Y threshold"]
        && [entry_validate .status.filter.erts 1 "" "RTs threshold"]
        && [entry_validate .status.filter.eratio 0 "" "K2Y to RTs ratio"]} {
      set globals(yy_filter_k2y) [.status.filter.ek2y get];
      set globals(yy_filter_rts) [.status.filter.erts get];
      set globals(yy_filter_ratio) [.status.filter.eratio get];
      set globals(errno) 0;
    }; # if
    break;
  }; # bind

  bind yy_filter_input <Control-g> {
    set globals(errno) 1;
  }; # bind
  bind yy_filter_input <Control-G> [bind yy_filter_input <Control-g>];

  #
  # produce reminder of input modality if necessary
  #
  bind .status.filter <ButtonPress> {
    if {[winfo containing %X %Y] != ".status.filter.ek2y"
        && [winfo containing %X %Y] != ".status.filter.erts"
        && [winfo containing %X %Y] != ".status.filter.eratio"
        && [winfo containing %X %Y] != ".status.filter.eupper"} {
      tsdb_beep;
      status [format "%%s %%s" \
              "<Return> completes parameter input;" \
              "<Control-G> aborts"];
      after 1500;
      status "";
      raise .status.filter;
      update idletasks;
    }; # if
  }; # bind
  bind .status.filter.fill0 <ButtonPress> \
    [bind .status.filter <ButtonPress>];
  bind .status.filter.lk2y <ButtonPress> \
    [bind .status.filter <ButtonPress>];
  bind .status.filter.fill1 <ButtonPress> \
    [bind .status.filter <ButtonPress>];
  bind .status.filter.lrts <ButtonPress> \
    [bind .status.filter <ButtonPress>];
  bind .status.filter.fill2 <ButtonPress> \
    [bind .status.filter <ButtonPress>];
  bind .status.filter.lratio <ButtonPress> \
    [bind .status.filter <ButtonPress>];
  bind .status.filter.fill3 <ButtonPress> \
    [bind .status.filter <ButtonPress>];

  #
  # activate filter parameter input pane
  #
  set focus [focus -displayof .];
  focus .status.filter.ek2y;
  grab set .status.filter;
  raise .status.filter;
  tkwait variable globals(errno);

  #
  # restore previous state
  #
  grab release .status.filter;
  focus $focus;
  raise .status.label;
  if {!$globals(errno) && $set} {
    tsdb_set "*yy-k2y-ra-threshold*" $globals(yy_filter_k2y);
    tsdb_set "*yy-rts-ra-threshold*" $globals(yy_filter_rts);
    tsdb_set "*yy-k2y-rts-ra-ratio*" $globals(yy_filter_ratio);
  }; # if
  return $globals(errno);

}; # yy_filter_input()
