proc update_skeleton_list {} {

  global globals skeletons;

  set menu .menu.file.menu.create 
  $menu delete 0 end;

  foreach i [lsort -integer [array names skeletons]] {
    set item $skeletons($i);
    set label "[lindex $item 1] \[[lindex $item 2] items\]";
    $menu add command -label $label -command [list tsdb_file create $i]
  }; # foreach

  copyleft hide;

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
      foreach i [lsort -integer [array names test_suites]] {
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
      } else {
        set globals(data) "";
      }; # else
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

  #
  # as this is one of the more frequently called functions, place the safenet
  # for the registrar background process here; just to make sure that the poor
  # zombie can eventually free its resources ...
  #
  oe reap;

}; # update_ts_list()


proc update_source_database {index} {

  global globals;

  set globals(selection) $index;
  update_ts_list;

}; # update_source_database()


proc update_condition_cascade {{active ""}} {

  global globals phenomena;

  if {$active == "phenomena"} {
    set globals(condition,null) $globals(phenomena,all);
  } elseif {[regexp {^[0-9]+$} $active]} {
    set globals(condition,null) 0;
  } elseif {$active == "wellformed"} {
    set globals(condition,null) 0;
    set globals(condition,illformed) 0;
  } elseif {$active == "illformed"} {
    set globals(condition,null) 0;
    set globals(condition,wellformed) 0;
  } elseif {$active == "analyzed"} {
    set globals(condition,null) 0;
    set globals(condition,unanalyzed) 0;
  } elseif {$active == "unanalyzed"} {
    set globals(condition,null) 0;
    set globals(condition,analyzed) 0;
  } elseif {$active == "unproblematic"} {
    set globals(condition,null) 0;
  } elseif {$active == "null" || $globals(condition,null)} {
    set globals(condition,null) 1;
    foreach i {wellformed illformed analyzed unanalyzed unproblematic} {
      set globals(condition,$i) 0;
    }; # foreach
    for {set i 0} {$i < $globals(condition,size)} {incr i} {
      set globals(condition,$i) 0;
    }; # for
    update_phenomena_cascade all;
  }; # if
  
  if {$active != "phenomena"} {
    #
    # construct cascade of checkbuttons; bottom is .n. (5) most recent history
    # entries for the condition class.
    #
    if {[.menu.options.menu.condition index end] != "none"} {
      .menu.options.menu.condition delete 0 end;
    }; # if
    .menu.options.menu.condition add checkbutton \
      -label "No Condition" \
      -variable globals(condition,null) \
      -command {update_condition_cascade null};
    .menu.options.menu.condition add checkbutton \
      -label "Phenomena Selection" \
      -offvalue 1 -onvalue 0 -state disabled \
      -variable globals(phenomena,all) \
      -command {update_phenomena_cascade};
    .menu.options.menu.condition add separator

    .menu.options.menu.condition add checkbutton \
      -label "Wellformed (`i-wf = 1')" \
      -variable globals(condition,wellformed) \
      -command {update_condition_cascade wellformed};
    .menu.options.menu.condition add checkbutton \
      -label "Illformed (`i-wf = 0')" \
      -variable globals(condition,illformed) \
      -command {update_condition_cascade illformed};
    .menu.options.menu.condition add checkbutton \
      -label "Analyzed (`readings > 0')" \
      -variable globals(condition,analyzed) \
      -command {update_condition_cascade analyzed};
    .menu.options.menu.condition add checkbutton \
      -label "Unanalyzed (`readings = 0')" \
      -variable globals(condition,unanalyzed) \
      -command {update_condition_cascade unanalyzed};
    .menu.options.menu.condition add checkbutton \
      -label "Unproblematic (`readings != -1')" \
      -variable globals(condition,unproblematic) \
      -command {update_condition_cascade unproblematic};

    history_move condition end 1;
    for {set i 0} {$i < 10} {incr i} {
      if {[set condition [history_move condition 1 1]] == ""} {
        break;
      }; # if
      if {!$i} {
        .menu.options.menu.condition add separator
      }; # if
      .menu.options.menu.condition add checkbutton \
        -label "`$condition'" -variable globals(condition,$i) \
        -command [list update_condition_cascade $i];
    }; # for
    set globals(condition,size) $i;
    history_move condition end 1;
  }; # if

  #
  # determine current TSQL condition: conjunctively concatenate all active
  # restrictions.
  #
  set globals(condition) "";
  if {!$globals(phenomena,all)} {
    foreach i [lsort -integer [array names phenomena]] {
      if {$globals(phenomena,$i)} {
        set condition [lispify_string "(p-name ~ `$phenomena($i)')"];
        if {$globals(condition) == ""} {
          set globals(condition) $condition;
        } else {
          set globals(condition) \
            "$globals(condition) or $condition";
        }; # else
      }; # if
    }; # foreach
  }; # if
  if {!$globals(condition,null)} {

    if {$globals(condition,wellformed)} {
      if {$globals(condition) == ""} {
        set globals(condition) [lispify_string "(i-wf = 1)"];
      } else {
        set globals(condition) \
          "$globals(condition) and [lispify_string "(i-wf = 1)"]";
      }; # else
    }; # if

    if {$globals(condition,illformed)} {
      if {$globals(condition) == ""} {
        set globals(condition) [lispify_string "(i-wf = 0)"];
      } else {
        set globals(condition) \
          "$globals(condition) and [lispify_string "(i-wf = 0)"]";
      }; # else
    }; # if

    if {$globals(condition,analyzed)} {
      if {$globals(condition) == ""} {
        set globals(condition) [lispify_string "(readings > 0)"];
      } else {
        set globals(condition) \
          "$globals(condition) and [lispify_string "(readings > 0)"]";
      }; # else
    }; # if

    if {$globals(condition,unanalyzed)} {
      if {$globals(condition) == ""} {
        set globals(condition) [lispify_string "(readings = 0)"];
      } else {
        set globals(condition) \
          "$globals(condition) and [lispify_string "(readings = 0)"]";
      }; # else
    }; # if

    if {$globals(condition,unproblematic)} {
      if {$globals(condition) == ""} {
        set globals(condition) [lispify_string "(readings != -1)"];
      } else {
        set globals(condition) \
          "$globals(condition) and [lispify_string "(readings != -1)"]";
      }; # else
    }; # if

    history_move condition end 1;
    for {set i 0} {$i < $globals(condition,size)} {incr i} {

      set condition [history_move condition 1];
      if {$globals(condition,$i)} {
        if {$globals(condition) == ""} {
          set globals(condition) "([lispify_string $condition])";
        } else {
          set globals(condition) \
            "$globals(condition) and ([lispify_string $condition])";
        }; # else
      }; # if
    }; # for
  }; # if

  tsdb_set "*statistics-select-condition*" "\"$globals(condition)\"";
  history_move condition end 1;

}; # update_condition_cascade()


proc update_phenomena_cascade {{code ""}} {

  global globals phenomena;

  if {$code == "all" || $code == "reset" || $globals(phenomena,all)} {
    set globals(phenomena,all) 1;
    foreach i [lsort -integer [array names phenomena]] {
      set globals(phenomena,$i) 0;
    }; # foreach
  } elseif {$code != ""} {
    set globals(phenomena,all) 0;
  }; # if

  if {$code == "reset"} {
    #
    # construct cascade of checkbuttons, one per phenomenon.
    #
    if {[.menu.options.menu.phenomena index end] != "none"} {
      .menu.options.menu.phenomena delete 0 end;
    }; # if
    .menu.options.menu.phenomena add checkbutton \
      -label "All Phenomena" \
      -variable globals(phenomena,all) \
      -command {update_phenomena_cascade all};

    if {[array names phenomena] != ""} {
      .menu.options.menu.phenomena add separator;
    }; # if
    foreach i [lsort -integer [array names phenomena]] {
      .menu.options.menu.phenomena add checkbutton \
        -variable globals(phenomena,$i) \
        -label "$phenomena($i)" \
        -command "update_phenomena_cascade $i";
    }; # foreach
  }; # if

  update_condition_cascade phenomena;

}; # update_phenomena_cascade()


proc update_graph_cascade {code} {

  global globals;

  set fields {first total tcpu tgc 
              p-ftasks p-etasks p-stasks 
              aedges pedges raedges rpedges};
  if {$code == "tasks" || $code == "ptimes" || $code == "ttimes"} {
    set globals(graph,values) $code;
    foreach field $fields {
      set globals(graph,$field) 0;
    }; # foreach
    switch $code {
      tasks {
        set globals(graph,p-ftasks) 1;
        set globals(graph,p-etasks) 1;
        set globals(graph,p-stasks) 1;
      }
      ptimes {
        set globals(graph,first) 1;
        set globals(graph,total) 1;
      }
      ttimes {
        set globals(graph,tcpu) 1;
        set globals(graph,tgc) 1;
      }
    }; # switch
  } elseif {$code == "first" || $code == "total" 
            || $code == "tcpu" || $code == "tgc"} {
    foreach field {p-ftasks p-etasks p-stasks aedges pedges} {
      set globals(graph,$field) 0;
    }; # foreach
    if {$globals(graph,values) != "ptimes" 
        && $globals(graph,values) != "ttimes"} {
      set globals(graph,values) "";
    }; # if
  } elseif {$code == "p-ftasks" || $code == "p-etasks" 
            || $code == "p-stasks"} {
    foreach field {first total tcpu tgc aedges pedges} {
      set globals(graph,$field) 0;
    }; # foreach
    if {$globals(graph,values) != "tasks"} {
      set globals(graph,values) "";
    }; # if
  } elseif {$code == "aedges" || $code == "pedges"
            || $code == "raedges" || $code == "rpedges"} {
    foreach field {first total tcpu tgc p-ftasks p-etasks p-stasks} {
      set globals(graph,$field) 0;
    }; # foreach
    set globals(graph,values) "edges";
  }; # if

  set globals(graph_values) "(";
  foreach field $fields {
    if {$globals(graph,$field)} {
      set globals(graph_values) "$globals(graph_values) :$field";
    }; # if
  }; # foreach
  set globals(graph_values) "$globals(graph_values))";

}; # update_graph_cascade()


proc register_at_saabruecken {} {

  global globals;

  catch {
    set version \
      [expr {[info exists globals(version)] ? $globals(version) : "?"}];
    set client [info hostname];
    set socket [socket $globals(saarbruecken) 25];
    fconfigure $socket -blocking yes -buffering line;
    set status [gets $socket];
    if {[string range $status 0 2] != "220"} {
      return [after 600000 register_at_saabruecken];
    }; # if
    puts $socket "HELO $client";
    set status [gets $socket];
    if {[string range $status 0 2] != "250"} {
      return [after 600000 register_at_saabruecken];
    }; # if
    puts $socket "MAIL FROM:<$globals(user)@$client>";
    set status [gets $socket];
    if {[string range $status 0 2] != "250"} {
      return [after 600000 register_at_saabruecken];
    }; # if
    puts $socket "RCPT TO:<podium@coli.uni-sb.de>";
    set status [gets $socket];
    if {[string range $status 0 2] != "250"} {
      after 600000 register_at_saabruecken;
      return;
    }; # if
    puts $socket "DATA";
    set status [gets $socket];
    if {[string range $status 0 2] != "354"} {
      after 600000 register_at_saabruecken;
      return;
    }; # if
    puts $socket "To: podium@coli.uni-sb.de";
    puts $socket \
      "Subject: $globals(name) ($version) for `$globals(user)' on `$client'";
    puts $socket ".";
    set status [gets $socket];
    if {[string range $status 0 2] != "250"} {
      after 600000 register_at_saabruecken;
      return;
    }; # if
    puts $socket "QUIT";
    after 5000;
    close $socket;
  }; # catch

}; # register_at_saabruecken()


proc tsdb_set {variable {value ""}} {

  global globals;

  if {$value == ""} {
    switch $variable {
      aggregate_dimension {
        set variable "*statistics-aggregate-dimension*";
        set value $globals(aggregate_dimension);
      }
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
      write_rule_p {
        set variable "*tsdb-rule-statistics-p*"; 
        set value [lispify_truth_value $globals(write_rule_p)]
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

  if {$value == "" || $value == "\"\""} {
    switch -exact $variable {
      *statistics-select-condition* -
      *statistics-aggregate-size* -
      *statistics-aggregate-threshold* -
      *statistics-aggregate-lower* -
      *statistics-aggregate-upper* {
        set value nil;
      }
      default {
        set value "\"\"";
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


proc entry_incr {entry increment {lower ""} {upper ""} {label ""}} {

  if {[entry_validate $entry $lower $upper $label]} {
    set current [$entry get];
    if {$current == ""} {set current 0};
    incr current $increment;
    if {$lower != "" && $current < $lower} {set current $lower};
    if {$upper != "" && $current > $upper} {set current $upper};
    $entry delete 0 end;
    $entry insert 0 $current;
  }; # if

}; # entry_incr()


proc entry_validate {entry {lower ""} {upper ""} {label ""}} {

  set current [$entry get];
  if {[regexp {^[+-]?[0-9]+$} $current] || $current == ""} {
    if {$current == "" || $lower == "" || $current >= $lower} {
      if {$current == "" || $upper == "" || $current <= $upper} {
        return 1;
      } else {
        set error [format "invalid (too large) value in entry field%s (%s)" \
                   [expr {$label != "" ? " `$label'" : ""}] \
                   "should be less or equal $upper"];
      }; # else
    } else {
      set error [format "invalid (too small) value in entry field%s (%s)" \
                 [expr {$label != "" ? " `$label'" : ""}] \
                 "should be greater or equal $lower"];
    }; # else
  } else {
    set error [format "invalid (non-numeric) value in entry field%s" \
               [expr {$label != "" ? " `$label'" : ""}]];
  }; # else

  if {[info exists error]} {
    tsdb_beep;
    status $error;
    after 2500;
    status "";
    lower .status.label;
  }; # if
  return 0;

}; # entry_validate()


proc string_strip {prefix string} {

  return [string range $string [string length $prefix] end]

}; # string_strip()


proc tsdb_beep {} {

  beep 100;

}; # tsdb_beep()


proc tsdb_busy {{action "update"}} {

  global globals;

  switch $action {
    "freeze" {
      oe busy yes;
      tsdb_busy;
    }
    "release" {
      oe busy no;
      tsdb_busy;
    }
    "update" {
      if {[oe gc] == "yes"} {
        busy hold .;
        busy config . -cursor $globals(gc_cursor); 
      } elseif {[oe busy] == "yes"} {
        if {!$globals(kanji_p)} {
          busy hold .;
          busy config . -cursor $globals(busy_cursor);
        }; # if
      } else {
        if {[busy isbusy .] == "."} {
          busy release .;
        }; # if
      }; # else
    }
  }; # switch

}; # tsdb_busy()

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

  bind $toplevel <q> [list "tsdb_close" $toplevel];
  bind $toplevel <Q> [bind $toplevel <q>];

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

proc send_to_lisp {code string {lispify 0} {force 1}} {

  if {!$force && [oe busy] == "yes"} {
    tsdb_beep;
    return;
  }; # if

  if {$lispify} {
    set string [lispify_string $string];
  }; # if
  set command [format "(%s %s)\n\n" $code $string];
  puts $command;
  flush stdout;
  logger $command;

}; # send_to_lisp()


proc lispify_string {string} {

  regsub -all {(\\)|(")} $string {\\\0} string;
  return $string;

}; # lispify-string()


proc unlispify_string {string} {

  regsub -all {\\(\\|")} $string {\1} string;
  return $string;

}; # unlispify-string()


proc current-time {{long 0}} {

  set now [clock seconds];
  set day [string trim [clock format $now -format "%d"] {"0"}];
  set format [expr {$long ? "%b-%y (%H:%M)" : "%b-%y"}];
  set time "$day-[clock format $now -format $format]";
  string tolower $time;

}; # current-time()

#
# set of utility functions to maintain (input) history for various entry types
#
proc history_add {class item} {

  global history;

  if {$item != ""} {
    if {[info exists history($class)]} {
      if {[lsearch -exact $history($class) $item] < 0} {
        set history($class) [linsert $history($class) 0 $item]
        incr history($class,size);
      }; # if
    } else {
      set history($class) [list $item];
      set history($class,size) 1;
    }; # else
    set history($class,position) -1;
  }; # if

}; # history_add()

proc history_move {class offset {quiet 0}} {

  global history;

  if {![info exists history($class)]} {
    if {!$quiet} {
      tsdb_beep;
    }; # if
    set history(errno) -1;
    return "";
  }; # if

  if {$offset == "end"} { 
    set history($class,position) -1;
    return "";
  }; # if

  if {[set position [expr $history($class,position) + $offset]] < -1
      || $position > [expr $history($class,size) - 1]} {
    if {!$quiet} {
      tsdb_beep;
    }; # if
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

proc menu_button_down {window button} {

  global globals;

  set special 0;
  foreach menu $globals(special_menues) {
    if {$menu == $window} {
      set special 1;
      break;
    }; # if
  }; # foreach

  if {!$special || $button != 2} {
    tkMenuButtonDown $window;
  }; # if
  
}; # menu_button_down()


proc menu_button_up {window button} {

  global globals;

  set special 0;
  foreach menu $globals(special_menues) {
    if {$menu == $window} {
      set special 1;
      break;
    }; # if
  }; # foreach

  if {!$special || $button != 2} {
     tkMenuInvoke $window 1;
  } else {
    $window invoke active;
  }; # else

}; # menu_button_up()


proc install_interrupt_handler {file} {

  global globals;

  set globals(interrupt) $file;
  balloon post "<Control-C> or <Control-G> abort \
                processing (i.e. the current test run)"
  bind . <Control-C> tsdb_abort;
  bind . <Control-c> tsdb_abort;
  bind . <Control-G> tsdb_abort;
  bind . <Control-g> tsdb_abort;

}; # install_interrupt_handler()


proc delete_interrupt_handler {} {

  global globals;

  set globals(interrrupt) "";
  balloon unpost;

}; # delete_interrupt_handler()
