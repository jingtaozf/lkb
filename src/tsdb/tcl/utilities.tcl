proc update_skeleton_list {} {

  global globals skeletons;

  set menu .menu.file.menu.create 
  $menu delete 0 end;

  foreach i [lsort [array names skeletons]] {
    set item $skeletons($i);
    set label "[lindex $item 1] \[[lindex $item 2] items\]";
    $menu add command -label $label -command [list tsdb_file create $i]
  }; # foreach

}; # update_skeleton_list()


proc update_ts_list {{action update} {name all} {arg_one yes} {arg_two yes}} {

  global globals test_suites compare_in_detail;

  set list [.list subwidget hlist];
  set wleft [tixDisplayStyle text -bg white -anchor w -padx 5]
  set wcenter [tixDisplayStyle text -bg white -anchor c]
  set gleft [tixDisplayStyle text -bg gold -anchor w -padx 5]
  set gcenter [tixDisplayStyle text -bg gold -anchor c]

  if {$action == "update"} {
    if {$name == "all"} {
      $list delete all
      .menu.detail.menu.compare delete 0 end;
      foreach i [lsort [array names test_suites]] {
        set item $test_suites($i);
        set left $wleft;
        set center $wcenter;

        set selection $globals(selection);

        if {[lindex $item 0] == $compare_in_detail(source)} {
          set left $gleft;
          set center $gcenter;
          if {$selection == -1} {
            set selection $i;
          }; # if
        }; # if
        if {[lindex $item 0] == $globals(data)} {
          set active $i;
          if {$globals(selection) == -1} {
            set selection $i;
          }; # if
        }; # if
        $list add $i -at $i -text [lindex $item 0] -style $left;
        $list item create $i 1  -text [lindex $item 1] -style $center;
        $list item create $i 2  -text [lindex $item 2] -style $center;
        $list item create $i 3  -text [lindex $item 3] -style $center;
        if {[lindex $item 4]} {
          $list item create $i 4  -text yes -style $center;
        } else {
          $list item create $i 4  -text no -style $center;
        }; # else
        .menu.detail.menu.compare add radiobutton -label "[lindex $item 0]" \
          -selectcolor gold \
          -variable compare_in_detail(source) \
          -command "update_source_database $i"
      }; # foreach
      if {[info exists active]} {
        $list selection clear;
        $list selection set $active;
      }; # if
      if {[info exists selection] && $selection != -1} {
        $list see $selection;
      }; # if
      set globals(selection) -1;
    } else {
    if {[regexp {^[0-9][0-9]*$} $name] && $test_suites($name) != ""} {
      set index $name;
    } else {
      for {set i 0} {$i < [array size test_suites]} {incr i} {
        if {![string compare $name [lindex $test_suites($i) 0]]} {
          set index $i;
          break;
        }; # if
      }; # for
    }; # else
      if {[info exists index]} {
        set command "(list \"[lindex $test_suites($index) 0]\" $index)";
        send_to_lisp :event $command;
      }; # if
    }; # else
  } elseif {$action == "rename"} {
    if {[regexp {^[0-9][0-9]*$} $name] && $test_suites($name) != ""} {
      set index $name;
    } else {
      for {set i 0} {$i < [array size test_suites]} {incr i} {
        if {![string compare $name [lindex $test_suites($i) 0]]} {
          set index $i;
          break;
        }; # if
      }; # for
    }; # else
    set new [lreplace $test_suites($index) 0 0 $arg_one];
    update_ts_list delete $index;
    update_ts_list add $new yes;
  } elseif {$action == "add"} {
    set n [array size test_suites];
    set new [lindex $name 0];
    for {set i 0} {$i < $n} {incr i} {
      if {[string compare $new [lindex $test_suites($i) 0]] <= 0} {
        break;
      }; # if
    }; # for
    set index $i;
    for {set i [expr $n - 1]} {$i >= $index} {incr i -1} {
      set test_suites([expr $i + 1]) $test_suites($i);
    }; # for
    set test_suites($index) $name;
    update_ts_list;
    if {$arg_one == "yes"} {
      $list selection clear;
      $list selection set $index;
      $list see $index;
      set globals(data) $new;
      set globals(relations) {};
      set globals(attributes) {};
      tsdb_set "*tsdb-data*" "\"$new\"";
    }; # if
  } elseif {$action == "delete"} {
    if {[regexp {^[0-9][0-9]*$} $name] && $test_suites($name) != ""} {
      set index $name;
      set name [lindex $test_suites($index) 0];
    } else {
      for {set i 0} {$i < [array size test_suites]} {incr i} {
        if {![string compare $name [lindex $test_suites($i) 0]]} {
          set index $i;
          break;
        }; # if
      }; # for
    }; # else
    if {[info exists index]} {
      for {set i $index} {$i < [expr [array size test_suites] - 1]} {incr i} {
        set test_suites($i) $test_suites([expr $i + 1]);
      }; # for
      unset test_suites($i);
      update_ts_list;
      if {$globals(data) == $name} {
        set globals(data) "";
        set globals(relations) {};
        set globals(attributes) {};
        tsdb_set "*tsdb-data*" nil;
      }; # if
      if {$compare_in_detail(source) == $name} {
        set compare_in_detail(source) "";
      }; # if
    }; # if
  }; # elseif

}; # update_ts_list()


proc update_phenomena_list {} {

  global phenomena;

  set browse_cascades {
    .menu.browse.menu.items
    .menu.browse.menu.phenomena
    .menu.browse.menu.parses
    .menu.browse.menu.results
    .menu.browse.menu.errors
  }; # browse_cascades

  if {[info exists phenomena]} {
    if {[.menu.detail.menu.phenomena index end]} {
      #
      # somehow, deleting from position 1 (as it should be because of the
      # static `All Phenomena' entry at position 0) produces a miss-by-one
      # error.  possibly a TCL 8.0 bug.             (3-aug-98  -  oe@csli)
      #
      .menu.detail.menu.phenomena delete 0 end;
    }; # if

    foreach cascade $browse_cascades {
      if {[$cascade index end] != "none" && [$cascade index end]} {
        $cascade delete 1 end;
      }; # if
    }; # foreach

    .menu.detail.menu.phenomena add separator
    foreach i [lsort -integer [array names phenomena]] {
      .menu.detail.menu.phenomena add checkbutton -label "$phenomena($i)" \
        -variable compare_in_detail(phenomena,$phenomena($i));
    }; # foreach

    foreach cascade $browse_cascades {
      $cascade add separator;
      foreach i [lsort -integer [array names phenomena]] {
        set condition "p-name ~ `$phenomena($i)'";
        set action [string range $cascade 18 end];
        $cascade add command -label "$phenomena($i)" \
          -command [list tsdb_browse $action $condition];
      }; # foreach
    }; # foreach

  }; # if

}; # update_phenomena_list()


proc update_source_database {index} {

  global globals;

  set globals(selection) $index;
  update_ts_list;

}; # update_source_database()


proc update_condition_cascade {menu class} {

  global globals conditions;

  if {[$menu index end] != "none"} {
    $menu delete 0 end;
  }; # if
  $menu add command 

}; # update_condition_cascade()


proc tsdb_set {variable {value ""}} {

  global globals;

  if {$value == ""} {
    switch $variable {
      exhaustive_p {
        set variable "*tsdb-exhaustive-p*"; 
        set value [lispify_truth_value $globals(exhaustive_p)]
      }
      write_run_p {
        set variable "*tsdb-write-run-p*"; 
        set value [lispify_truth_value $globals(write_run_p)]
      }
      write_parse_p {
        set variable "*tsdb-write-parse-p*"; 
        set value [lispify_truth_value $globals(write_parse_p)]
      }
      write_result_p {
        set variable "*tsdb-write-result-p*"; 
        set value [lispify_truth_value $globals(write_result_p)]
      }
      write_output_p {
        set variable "*tsdb-write-output-p*"; 
        set value [lispify_truth_value $globals(write_output_p)]
      }
      write_syntax_chart_p {
        set variable "*tsdb-write-syntax-chart-p*"; 
        set value [lispify_truth_value $globals(write_syntax_chart_p)]
      }
      write_lexicon_chart_p {
        set variable "*tsdb-write-lexicon-chart-p*"; 
        set value [lispify_truth_value $globals(write_lexicon_chart_p)]
      }
      gc_p {
        set variable "*tsdb-gc-p*";
        set value $globals(gc_p);
      }
      tenure_p {
        set variable "*tsdb-tenure-p*";
        set value [lispify_truth_value $globals(tenure_p)];
      }
    }; # switch
  }; # if

  set command [format "(set %s %s)" $variable $value];
  send_to_lisp :event $command;

}; # tsdb_set()


proc verify_ts_selection {{code ""}} {

  global globals compare_in_detail;

  if {$globals(data) == ""} {
    tsdb_beep;
    status "no test suite database active; make up your mind ... |:-\}" 5;
    return 1;
  }; # if
  if {$code != ""
      && (![info exists compare_in_detail(source)] 
          || $compare_in_detail(source) == "")} {
    tsdb_beep;
    set message "no (gold-standard) source database active";
    status "$message; make up your mind ... |:-\}" 5;
    return 1;
  }; # if
  return 0;

}; # verify_ts_selection()


proc read_database_schema {data} {
  
  send_to_lisp :event "(schema \"$data\")";
  tkwait variable globals(relations);

}; # read_database_schema()

proc lispify_truth_value {value} {

  if {$value} {
    return t;
  } else {
    return nil;
  }; # else

}; # lispify_truth_value()


proc string_strip {prefix string} {

  return [string range $string [string length $prefix] end]

}; # string_strip()


proc tsdb_beep {} {

  beep 100;

}; # tsdb_beep()


proc isbusy {window} {
  return "\"[busy isbusy $window]\"";
}; # isbusy()

proc show_text {file {container ""} {title ""} {width 80} {height 25}} {

  global globals;

  if {$container == ""} { set container ".relations[gensym]" };
  if {$title == ""} { set title [format "tsdb(1) `%s' text view" $file] };

  if {[catch {set in [open $file r]}]} { 
    error "show_text(): unable to open `$file'."
  };# if

  set toplevel [toplevel $container];

  button $toplevel.close -text "Close" -command [list "tsdb_close" $toplevel];
  button $toplevel.search -text "Find" -state disabled;
  button $toplevel.print -text "Print" -state disabled;

  set stext [tixScrolledText $toplevel.stext \
                             -options { text.font {courier 12}
                                        text.scrollbar auto }];
  set text [$stext subwidget text]
  $text config -width $width
  $text config -height $height

  table $toplevel \
    0,0 $toplevel.stext -cspan 3 -fill both -padx 5 -pady 5 \
    1,0 $toplevel.close -pady {5 10} \
    1,1 $toplevel.search -pady {5 10} \
    1,2 $toplevel.print -pady {5 10}
  table configure $toplevel r1 -resize none

  wm title $toplevel $title
  wm iconname $toplevel $title

  while {![eof $in]} { $text insert end [read $in 512] };
  close $in;

  tkwait visibility $toplevel
  update idletasks

  return [format "(:text (:toplevel . \"%s\") (:stext . \"%s\")\
                          (:text . \"%s\"))" \
                 $toplevel $stext $text]

}; # show_text()

proc show_chart {file {container ""} {title ""}} {

  global globals;

  if {$container == ""} { set container ".relations[gensym]" };
  if {$title == ""} { set title [format "tsdb(1) `%s' bar chart view" $file] };

  if {[catch {set in [open $file r]}]} { 
    error "show_chart(): unable to open `$file'."
  };# if

  set toplevel [toplevel $container];

  button $toplevel.close -text "Close" -command [list "tsdb_close" $toplevel];
  button $toplevel.latex -text "LaTeX";
  button $toplevel.postscript -text "PostScript";

  set chart [barchart $toplevel.stext -invertxy 1]

  table $toplevel \
    0,0 $toplevel.chart -cspan 3 -fill both -padx 5 -pady 5 \
    1,0 $toplevel.close -pady {5 10} \
    1,1 $toplevel.latex -pady {5 10} \
    1,2 $toplevel.postscript -pady {5 10}
  table configure $toplevel r1 -resize none

  wm title $toplevel $title
  wm iconname $toplevel $title

  tkwait visibility $toplevel
  update idletasks

  return [format "(:text (:toplevel . \"%s\") (:stext . \"%s\")\
                          (:text . \"%s\"))" \
                 $toplevel $stext $text]

}; # show_chart()

proc send_to_lisp {code string {lispify 0}} {

  if {$lispify} {
    set string [lispify_string $string];
  }; # if
  set command [format "(%s %s)" $code $string];
  puts $command;
  flush stdout;
  logger $command;

}; # send_to_lisp()


proc lispify_string {string} {

  regsub -all {(\\)|(")} $string {\\\0} string;
  return $string;

}; # lispify-string()


proc initialize_meter {{toplevel ".meter"} {label ""}} {

  toplevel $toplevel;
  wm title $label;
  
}; # initialize_meter()

#
# set of utility functions to maintain (input) history for various entry types
#
proc history_add {class item} {

  global history;

  if {$item != ""} {
    if {[info exists history($class)]} {
      set history($class) [linsert $history($class) 0 $item]
      incr history($class,size);
    } else {
      set history($class) [list $item];
      set history($class,size) 1;
    }; # else
    set history($class,position) -1;
  }; # if

}; # history_add()

proc history_move {class offset} {

  global history;

  if {![info exists history($class)] 
      || [set position [expr $history($class,position) + $offset]] < -1
      || $position > [expr $history($class,size) - 1]} {
    tsdb_beep;
    set history(errno) -1;
    return "";
  }; # if
  
  incr history($class,position) $offset;
  set history(errno) 0;
  if {$position == -1} {
    return "";
  } else {
    return [lindex $history($class) $history($class,position)];
  }; # else

}; # history_move()


proc entry_history {entry class offset} {

  global history;

  set item [history_move $class $offset];
  if {!$history(errno)} {
    $entry delete 0 end;
    $entry insert 0 $item;
    $entry selection clear;
    $entry icursor end;
  }; # if

}; # entry_history()

