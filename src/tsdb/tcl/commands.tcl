proc tsdb_file {action {index -1}} {

  global globals skeletons;

  if {$action == "create"} {
    set skeleton $skeletons($index);
    if {$skeleton != ""} {
      set globals(skeleton) [lindex $skeleton 0];
      tsdb_set "*tsdb-default-skeleton*" "\"[lindex $skeleton 0]\"";
      set command "(create \"[lindex $skeleton 0]\")";
      send_to_lisp :event $command;
    }; # if
  } elseif {$action == "rename"} {
    if {[verify_ts_selection]} {return 1};
    set old $globals(data);
    set aold "$globals(home)$old";
    if {[file exists $aold]} {
      set anew [string range $aold 0 [string last $globals(slash) $aold]];
      if {![input "rename to:" $anew $globals(home) directory]} {
        set anew $globals(input);
        set new [string_strip $globals(home) $anew];
        if {[file exists $anew]} {
          tsdb_beep;
          status "database `$new' already exists" 10;
        } else {
          set parent \
            [string range $anew 0 [string last $globals(slash) $anew]];
          if {[catch {file mkdir $parent}]} {
            tsdb_beep;
            status "error creating parent directory `$parent'" 10;
          } else {
            if {![catch {file rename -force -- $aold $anew}]} {
              set globals(data) $new;
              set globals(relations) {};
              set globals(attributes) {};
              update_ts_list rename $old $new;
            } else {
              tsdb_beep;
              status "mysterious error renaming `$old'" 10;
            }; # else
          }; # else
        }; # else
      }; # if
    }; # if
  } elseif {$action == "reread"} {
    if {[verify_ts_selection]} {return 1};
    set command "(purge \"$globals(data)\")";
    send_to_lisp :event $command;
    update_ts_list update $globals(data);
  } elseif {$action == "purge"} {
    if {[verify_ts_selection]} {return 1};
    if {[file isdirectory $globals(home)$globals(data)] 
        && [yes-or-no-p "purge `$globals(data)'"] == 1} {
      set command "(purge \"$globals(data)\" :action :purge)";
      send_to_lisp :event $command;
    }; # if
  } elseif {$action == "delete"} {
    if {[verify_ts_selection]} {return 1};
    set old $globals(data);
    set aold "$globals(home)$old";
    if {[file isdirectory $aold] 
        && [yes-or-no-p "delete `$old'"] == 1} {
      set aold "[file dirname [file join $aold .]]$globals(slash)"
      set files [glob -nocomplain -- "$aold/*"];
      foreach afile $files {
        set file [string_strip $globals(home) $afile];
        status "deleting file `$file' ...";
        if {[catch {file delete -force -- $afile}]} {
          tsdb_beep;
          status "deleting file `$file' ... failed";
          after 1000;
        }; # if
        after 500;
      }; # foreach
      status "deleting directory `$old' ...";
      if {[catch {file delete -force -- $aold}]} {
        status "deleting directory `$old' ... failed";
        after 2000;
      }; # if
      after 500;
      if {[file exists $aold]} {
        tsdb_beep;
        status "deletion of `$old' may be incomplete" 10;
      } else {
        status "database `$old' successfully deleted" 10;
      }; # else
      update_ts_list delete $old;
    }; # if
  }; # elseif
}; # tsdb_file()


proc tsdb_import {code} {

  global globals;

  if {$code == "items"} {
    if {![input "item file:" [pwd]]} {
      set source $globals(input);
      if {![file isfile $source] || ![file readable $source]} {
        tsdb_beep;
        status "invalid file name `$source'" 10;
        return;
      }; # if

      if {![input "new database:" "" $globals(home) directory]} {
        set atarget $globals(input);
        set target [string_strip $globals(home) $atarget];
        if {$target == ""} {
          tsdb_beep;
          status "invalid name for target database" 10;
          return;
        }; # if
        if {[file exists $atarget]} {
          tsdb_beep;
          status "database `$target' already exists" 10;
          return;
        }; # if
        set parent \
          [string range $atarget 0 [string last $globals(slash) $atarget]];
        if {[catch {file mkdir $parent}]} {
          tsdb_beep;
          status "error creating parent directory `$parent'" 10;
          return;
        }; # if

        set command "(import :items \"$source\" \"$target\")";
        send_to_lisp :event $command;
      }; # if
    }; # if
  } else {
    if {![input "tsdb(1) database:" [pwd] "" directory]} {
      set source $globals(input);
      if {![file isdirectory $source] 
          || ![file readable $source$globals(slash)relations]} {
        tsdb_beep;
        status "invalid tsdb(1) database `$source'" 10;
        return;
      }; # if

      if {![input "new database:" "" $globals(home) directory]} {
        set atarget $globals(input);
        set target [string_strip $globals(home) $atarget];
        if {$target == ""} {
          tsdb_beep;
          status "invalid name for target database" 10;
          return;
        }; # if
        if {[file exists $atarget]} {
          tsdb_beep;
          status "database `$target' already exists" 10;
          return;
        }; # if
        set parent \
          [string range $atarget 0 [string last $globals(slash) $atarget]];
        if {[catch {file mkdir $parent}]} {
          tsdb_beep;
          status "error creating parent directory `$parent'" 10;
          return;
        }; # if

        set command "(import :database \"$source\" \"$target\")";
        send_to_lisp :event $command;
      }; # if
    }; # if
    
  }; # else

}; # tsdb_import()


proc tsdb_option {name} {

  global globals;

  switch $name {
    home {
      if {![input "database root:" $globals(home)]} {
        if {$globals(input) == $globals(home)} {
          status "tsdb(1) database root not changed" 5;
        } else {
          set path $globals(input);
          if {![file isdirectory $path]} {
            tsdb_beep;
            status "invalid directoy `$path'" 10;
          } else {
            set path "[file dirname [file join $path .]]$globals(slash)"
            set globals(home) $path;
            tsdb_set "*tsdb-home*" "\"$path\"";
            tsdb_update all;
          }; # else
        }; # else
      }; # if
    }
    skeleton_directory {
      if {![input "skeleton root:" $globals(skeleton_directory)]} {
        if {$globals(input) == $globals(skeleton_directory)} {
          status "tsdb(1) skeleton root not changed" 5;
        } else {
          set path $globals(input);
          set index [file join $path "Index.lisp"];
          if {![file isdirectory $path]
              || ![file exists $index]} {
            tsdb_beep;
            status "invalid directoy `$path'" 10;
          } else {
            set path "[file dirname [file join $path .]]$globals(slash)"
            set globals(skeleton_directory) $path;
            tsdb_set "*tsdb-skeleton-directory*" "\"$path\"";
            tsdb_update skeletons;
          }; # else
        }; # else
      }; # if
    }
  }; # switch
}; # tsdb_option()


proc tsdb_update {{name complete}} {

  global globals test_suites;

  set command "";
  if {$name == "selection"} {
    if {[verify_ts_selection]} {return 1};
    update_ts_list update $globals(data);
  } elseif {$name == "all" || $name == "complete"} {
    set command "(list)";
  }; # if
  if {$command != ""} {
    send_to_lisp :event $command;
  }; # if

  if {$name == "complete" || $name == "skeletons"} {
    set command "(skeletons)";
    send_to_lisp :event $command;
  }; # if

  if {$name == "complete" || $name == "condition"} {
    update_condition_cascade null;
  }; # if

  if {$name == "complete" || $name == "phenomena"} {
    update_phenomena_cascade reset;
  }; # if

}; # tsdb_update()


proc tsdb_quit {} {

  status "shutting down tsdb(1) podium ..."
  set command "(quit)";
  catch {send_to_lisp :event $command};
  run_meter 1000;
  exit;

}; # tsdb_quit()


proc tsdb_close {{toplevel ""}} {

  if {$toplevel == ""} {
    set command "(close)";
  } else {
    set command [format "(close (:toplevel . \"%s\"))" $toplevel];
    destroy $toplevel
  }
  send_to_lisp :event $command;

}; # tsdb_close()

proc tsdb_list_select {item} {

  global globals test_suites;

  set globals(data) [lindex $test_suites($item) 0];
  set globals(relations) {};
  set globals(attributes) {};

  set command [format "(set *tsdb-data* \"%s\")" $globals(data)];
  send_to_lisp :event $command;

}; # tsdb_list_select()


proc tsdb_list_run {item} {

  tsdb_list_select $item;
  analyze_performance;

}; # tsdb_list_run()


proc tsdb_browse_relations {} {

  global globals;

  if {[verify_ts_selection]} {return 1};
  set command [format "(relations \"%s\")" $globals(data)];
  send_to_lisp :event $command;
  
}; # tsdb_browse_relations()


proc tsdb_browse_vocabulary {{load 0}} {

  global globals;

  if {[verify_ts_selection]} {return 1};

  if {$load} {
    set command [format "(vocabulary \"%s\" :load :quiet" $globals(data)];
  } else {
    set command [format "(vocabulary \"%s\" :load :off" $globals(data)];
  }; # else
  if {[info exists globals(condition)] && $globals(condition) != ""} {
    set command "$command :condition \"$globals(condition)\")";
  } else {
    set command "$command)";
  }; # else
  send_to_lisp :event $command;

}; # tsdb_browse_vocabulary()


proc tsdb_browse {code {condition ""} {globalp 1}} {

  global globals;

  if {[verify_ts_selection]} {return 1};

  switch $code {
    items {
      set attributes "(\"i-id\" \"i-input\" \"i-wf\" \"i-category\")";
      set relations "(\"item\")";
    }
    phenomena {
      set attributes "(\"p-id\" \"p-name\" \"p-author\" \"p-date\")";
      set relations "(\"phenomenon\")";
    }
    runs {
      set attributes "(\"run-id\" \"comment\" \"platform\" \"application\" \"grammar\" \"avms\" \"sorts\" \"templates\" \"lexicon\" \"lrules\" \"rules\" \"user\" \"host\" \"start\" \"end\" \"items\" \"status\")";
      set relations "(\"run\")";
      set condition "";
    }
    parses {
      set attributes "(\"i-id\" \"i-input\" \"readings\" \"words\" \"first\" \"total\" \"tcpu\" \"tgc\" \"p-ftasks\" \"p-etasks\" \"p-stasks\" \"aedges\" \"pedges\"  \"raedges\" \"rpedges\" \"comment\")";
      set relations "(\"item\" \"parse\")";
    }
    results {
      tsdb_beep;
      status "no diplay mode available for `result' relation |:-\{" 5;
      return 1;
    }
    errors {
      if {$condition != ""} {
        set condition "$condition and (error != `')";
      } else {
        set condition "(error != `')";
      }; # else
      set attributes "(\"i-id\" \"i-input\" \"error\")";
      set relations "(\"item\" \"parse\")";
     }
  }; # switch

  if {$code != "runs" && $code != "errors"
      && $globalp && $globals(condition) != ""} {
    if {$condition != ""} {
      set condition "$globals(condition) and ($condition)";
    } else {
      set condition "$globals(condition)";
    }; # else
  }; # if

  if {[info exists attributes]} {
    set command [format "(select \"%s\" %s nil %s \"%s\")" \
                   $globals(data) $attributes $relations $condition];
    send_to_lisp :event $command;
  }; # if
  
}; # tsdb_browse()


proc tsdb_select {} {

  global globals;

  if {![input "select" "" "" select]} {
    history_add select $globals(input);
    set attributes "(";
    foreach i $globals(input) {
      set attributes "$attributes \"[lispify_string $i]\"";
    }; # foreach
    set attributes "$attributes)";
    if {![input "from" "" "" from]} {
      history_add from $globals(input);
      set relations "(";
      foreach i $globals(input) {
        set relations "$relations \"[lispify_string $i]\"";
      }; # foreach
      set relations "$relations)";
      if {![input "where" "" "" where]} {
        history_add where $globals(input);
        set condition [lispify_string $globals(input)];
        set command [format "(select \"%s\" %s nil %s \"%s\")" \
                     $globals(data) $attributes $relations $condition];
        send_to_lisp :event $command;
      }; # if
    }; # if
  }; # if
}; # tsdb_select()

proc tsdb_process {code {data ""} {key ""}} {

  global globals test_suites;

  if {$data == "" && [verify_ts_selection]} {return 1};

  switch $code {
    all {set condition ""}
    positive {set condition "i-wf == 1"}
    negative {set condition "i-wf == 0"}
    condition {
      set current [unlispify_string $globals(condition)];
       if {[condition_input "where" $current]} {
        return;
      }; # if
      set condition [lispify_string $globals(input)];
    }
    selection {
      set condition [lispify_string "i-id == $key"];
    }
    default {set condition ""}
  }; # switch
  if {$code == "selection"} {
    set interactive t;
    set comment "";
    set force 0;
  } else {
    set data $globals(data);
    set interactive nil;
    if {[input "comment:" "" "" comment]} {
      return;
    }; # if
    history_add comment $globals(input);
    set comment [lispify_string $globals(input)];
    set force 1;
  }; # else
  if {$globals(overwrite) && $interactive == "nil"} {
    set overwrite t;
  } else {
    set overwrite nil;
  }; # else
  set command \
    [format \
     "(process \"%s\" :condition \"%s\" :comment \"%s\" \
               :overwrite %s :interactive %s :vocabulary %s)" \
      $data $condition $comment $overwrite $interactive \
      [lispify_truth_value $globals(autoload_vocabulary)]];
  send_to_lisp :event $command 0 $force;
 
}; # tsdb_process()


proc tsdb_execute {code tag} {

  global globals;

  switch $code {
    browse {
      send_to_lisp :event "(execute :$code |$tag|)";
    }
    reconstruct {
      send_to_lisp :event "(execute :$code |$tag|)";
    }
  }; # switch

}; # tsdb_execute()


proc tsdb_abort {} {

  global globals;

  if {$globals(interrupt) != ""} {
    if {[catch {open $globals(interrupt) w} foo]} {
      tsdb_beep;
      status "mysterious problem generating interrupt" 10;
    } else {
      close $foo
    }; # else
  }; # if

}; # tsdb_abort()

proc tsdb_latex {toplevel} {

    set command [format "(latex (:toplevel . \"%s\"))" $toplevel];
    send_to_lisp :event $command;

}; # tsdb_latex()


proc analyze_competence {code} {

  global globals;

  if {[verify_ts_selection]} {return 1};

  set command \
      [format "(analyze-competence \"%s\" :wf %d)" $globals(data) $code];
  send_to_lisp :event $command;

}; # analyze_competence()


proc analyze_performance {{code "performance"}} {

  global globals;

  if {[verify_ts_selection]} {return 1};

  set command \
      [format "(analyze-performance \"%s\" :view :%s)" $globals(data) $code];
  send_to_lisp :event $command;

}; # analyze_performance()


proc analyze_rules {view} {

  global globals test_suites;

  if {[verify_ts_selection]} {return 1};

  #
  # _fix_me_ 
  # we need something like `ts_list find globals(data)' instead (27-jul-98)
  #
  for {set i 0} {$i < [array size test_suites]} {incr i} {
    if {![string compare $globals(data) [lindex $test_suites($i) 0]]} {
      set index $i;
      break;
    }; # if
  }; # for
  if {![info exists index] 
      || ![lindex $test_suites($index) 4]} {
    status [format "no rule data available for `%s' ... |:-\{" \
            $globals(data)] 10;
  } else {
    set attributes "(";
    foreach i {"actives" "passives" "successes" "executed" "filtered"} {
      if {$globals(rules,$i)} {
        set attributes "$attributes :$i";
      }; # if
    }; # foreach
    set attributes "$attributes)";
    set command \
        [format "(rules \"%s\" :attributes %s :logscale %s :view :%s)" \
           $globals(data) $attributes \
           [if {$globals(logscale)} {format "t"} {format "nil"}] \
           $view];
    send_to_lisp :event $command;
  }; # else

}; # analyze_rules()


proc tsdb_graph {{code "graph"}} {

  global globals test_suites;

  if {[verify_ts_selection]} {return 1};

  set command "($code \"$globals(data)\" :dimension $globals(graph,by)";
  if {$code != "chart"} {
    set command "$command :attributes $globals(graph_values)";
  }; # if
  if {$globals(graph_size) != ""} {
    set command "$command :aggregate $globals(graph_size)";
  }; # if
  if {$globals(graph_threshold) != ""} {
    set command "$command :threshold $globals(graph_threshold)";
  }; # if
  if {$globals(graph_lower) != ""} {
    set command "$command :lower $globals(graph_lower)";
  }; # if
  if {$globals(graph_upper) != ""} {
    set command "$command :upper $globals(graph_upper)";
  }; # if
  if {$globals(graph,scatterp)} {
    set command "$command :scatterp t";
  }; # if
  if {$globals(graph,extras)} {
    set command "$command :extras t";
  }; # if

  if {$code == "chart"} {
    set command "$command :title \"Aggregate Size (Number of Test Items)\"";
  } else {
    switch $globals(graph,values) {
      tasks {
        set command "$command :title \"Parser Tasks\"";
      }
      ptimes {
        set command "$command :title \"Parsing Time(s)\"";
      }
      ttimes {
        set command "$command :title \"Overall Processing Time(s)\"";
      }
      edges {
        set command "$command :title \"Chart Edges\"";
      }
    }; # switch
  }; # else
  switch $globals(graph,by) {
    :i-length {
      set command "$command :xtitle \"String Length (`i-length')\"";
    }
    :words {
      set command "$command :xtitle \"Lexical Items in Input (`words')\"";
    }
  }; # switch
  if {$globals(logscale)} {
    set command "$command :logscale t";
  }; # if
  set command "$command)";

  if {[info exists command]} {
    send_to_lisp :event $command;
  }; # if

}; # tsdb_graph()


proc tsdb_compare {action} {

  global globals compare_in_detail;

  if {[verify_ts_selection both]} {return 1};
  
  set source $compare_in_detail(source);
  set target $globals(data);

  switch $action {
    competence {
      set command "(compare-competence \"$source\" \"$target\")";
    }
    performance {
      set command "(compare-performance \"$source\" \"$target\")";
    }
  }; # switch 
  send_to_lisp :event $command;

}; # tsdb_compare()


proc tsdb_compare_in_detail {} {

  global globals phenomena compare_in_detail;

  if {[verify_ts_selection both]} {return 1};

  set show "";
  set compare "";

  if {$compare_in_detail(phenomena,all)} {
    set condition ""
  } else {
    foreach i [lsort [array names phenomena]] {
      if {$compare_in_detail(phenomena,$phenomena($i))} {
        if {[info exists condition]} {
          set condition "$condition or p-name ~ `$phenomena($i)'";
        } else {
          set condition "p-name ~ `$phenomena($i)'";
        }; # else
      }; # if
    }; # foreach
    if {![info exists condition]} {
      tsdb_beep;
      set message "no active phenomena for detailed comparison"
      status "$message; make up your mind ..." 5
      return
    }; # if
  }; # else

  if {$globals(condition) != ""} {
    if {$condition != ""} {
      set condition [lispify_string "($condition) and $globals(condition)"];
    } else {
      set condition $globals(condition);
    }; # else
  }; # if
  #
  # _fix_me_
  # this is rather awkward; presumably a global list for these attributes
  # (used in designing the menu and here) would be better (28-jul-98).
  #
  foreach attribute {i-wf i-input i-category} {
    if {$compare_in_detail(show,$attribute)} {
      set show "$show :$attribute";
    }; # if
  }; # foreach

  foreach attribute $compare_in_detail(compare,all) {
    if {$compare_in_detail(compare,$attribute)} {
      set compare "$compare :$attribute";
    }; # if
  }; # foreach

  set command [format "(detail \"%s\" \"%s\" :condition \"%s\" \
                          :show (%s) :compare (%s))" \
                      $compare_in_detail(source) $globals(data) \
                      $condition $show $compare];
  send_to_lisp :event $command;
 
}; # tsdb_compare_in_detail()

proc toggle_balloon_help {} {

  global globals;

  set globals(balloon_p) [expr ! $globals(balloon_p)];
  .menu.help.menu entryconfigure 0 \
    -label "[expr {$globals(balloon_p) ? "Disable" : "Enable"}] Balloon Help"
  balloon_setup $globals(balloons);

}; # toggle_balloon_help()


proc tsdb_todo {} {

  global globals;

  if {[winfo exists .todo]} {
    destroy .todo
  }; # if
  set directories [file split $globals(podium_home)];
  set directories [lreplace $directories [expr [llength $directories] -1] end];
  set directories [lreplace $directories 0 0];
  set root "$globals(slash)[join $directories $globals(slash)]";
  set todo [file join $root "ToDo"];
  if {[file exists $todo]} {
    show_text $todo .todo "$globals(name) ToDo List" 80 25;
  }; # if
}; # tsdb_todo()


proc tsdb_registration {} {

  global globals;

  if {[winfo exists .registration]} {
    destroy .registration
  }; # if
  set directories [file split $globals(podium_home)];
  set directories [lreplace $directories [expr [llength $directories] -1] end];
  set directories [lreplace $directories 0 0];
  set root "$globals(slash)[join $directories $globals(slash)]";
  set registration [file join $root "Registration"];
  if {[file exists $registration]} {
    show_text $registration .registration \
      "Registration Background Information" 80 25;
  }; # if
}; # tsdb_todo()
