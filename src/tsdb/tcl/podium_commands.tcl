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
      if {![input "rename to:" $anew $globals(home)]} {
        set anew $globals(input);
        set new [string_strip $globals(home) $anew];
        if {[file exists $anew]} {
          tsdb_beep;
          status "database `$new' already exists" 10;
        } else {
          if {![catch {file rename -force -- $aold $anew}]} {
            set globals(data) $new;
            update_ts_list rename $old $new;
          } else {
            tsdb_beep;
            status "mysterious error renaming `$old'" 10;
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
}; # tsdb_create()


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
          if {![file isdirectory $path]} {
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

  if {$name == "complete" || $name == "phenomena"} {
    update_phenomena_list;
  }; # if

}; # tsdb_close()


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

  set command [format "(set *tsdb-data* \"%s\")" $globals(data)];
  send_to_lisp :event $command;

}; # tsdb_list_select()


proc tsdb_list_run {item} {

  tsdb_list_select $item;
  analyze_performance :all;

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
    set command [format "(vocabulary \"%s\" :load :quiet)" $globals(data)];
  } else {
    set command [format "(vocabulary \"%s\" :load :off)" $globals(data)];
  }; # else
  send_to_lisp :event $command;

}; # tsdb_browse_vocabulary()


proc tsdb_browse {code condition} {

  global globals;

  if {[verify_ts_selection]} {return 1};

  switch $code {
    items {
      set attributes "(\"i-id\" \"i-input\" \"i-wf\" \"i-category\")";
      set types "(:integer :string :integer :string)";
      set relations "(\"item\")";
    }
    phenomena {
      set attributes "(\"p-id\" \"p-name\" \"p-presupposition\" \"p-author\" \"p-date\")";
      set types "(:integer :string :string :string :string)";
      set relations "(\"phenomenon\")";
    }
    runs {
      set attributes "(\"run-id\" \"comment\" \"application\" \"grammar\" \"avms\" \"sorts\" \"templates\" \"user\" \"host\" \"start\")";
      set types "(:integer :string :string :string :integer :integer :integer :string :string :string)";
      set relations "(\"run\")";
    }
    parses {
      set attributes "(\"i-id\" \"i-input\" \"readings\" \"words\" \"first\" \"total\" \"tcpu\" \"tgc\" \"p-etasks\" \"p-stasks\")";
      set types "(:integer :string :integer :integer :integer :integer :integer :integer :integer :integer)";
      set relations "(\"item\" \"parse\")";
    }
    results {
      tsdb_beep;
      status "no diplay mode available for `result' relation |:-\{" 5;
      return 1;
    }
    errors {
      if {$condition != ""} {
        set condition "$condition && error != `'";
      } else {
        set condition "error != `'";
      }; # else
      set attributes "(\"i-id\" \"i-input\" \"error\")";
      set types "(:integer :string :string)";
      set relations "(\"item\" \"parse\")";
     }
  }; # switch

  if {$code != "runs" && $globals(browse_condition) != ""} {
    if {$condition != ""} {
      set condition "$globals(browse_condition) && $condition";
    } else {
      set condition "$globals(browse_condition)";
    }; # else
  }; # if

  if {[info exists attributes]} {
    set command [format "(select \"%s\" %s %s %s \"%s\")" \
                   $globals(data) $attributes $types $relations $condition];
    send_to_lisp :event $command;
  }; # if
  
}; # tsdb_browse()


proc tsdb_browse_condition {} {

  global globals;

  if {![input "condition:" $globals(browse_condition)]} {
    set globals(browse_condition) [lispify_string $globals(input)];
  }; # if

}; # tsdb_browse_condition()

proc tsdb_process {code} {

  global globals test_suites;

  if {[verify_ts_selection]} {return 1};

  set comment "";
  if {![input "comment:"]} {
    set comment [lispify_string $globals(input)];
    switch $code {
      all {set condition ""}
      positive {set condition "i-wf == 1"}
      negative {set condition "i-wf == 0"}
      default {set condition ""}
    }; # switch
    if {$globals(overwrite)} {
      set overwrite t;
    } else {
      set overwrite nil;
    }; # else
    set command \
      [format \
       "(process \"%s\" :condition \"%s\" :comment \"%s\" :overwrite %s )" \
       $globals(data) $condition $comment $overwrite];
    send_to_lisp :event $command;
  }; # if
 
}; # tsdb_process()


proc tsdb_abort {} {

  set command "(abort)";
  send_to_lisp :interrupt $command;
  status "interrupt signal forwarded to processor" 5

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


proc analyze_performance {code} {

  global globals;

  if {[verify_ts_selection]} {return 1};

  set command \
      [format "(analyze-performance \"%s\" %s)" $globals(data) $code];
  send_to_lisp :event $command;

}; # analyze_performance()


proc analyze_rules {} {

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
      || ![lindex $test_suites(index) 4]} {
    status [format "no chart data available for `%s' ... |:-\{" \
            $globals(data)] 10;
  } else {
    set command \
        [format "(rules \"%s\" :logscale %s)" \
           $globals(data) \
           [if {$globals(logscale)} {format "t"} {format "nil"}]];
    send_to_lisp :event $command;
  }; # else

}; # analyze_rules()


proc tsdb_graph {code} {

  global globals test_suites;

  switch $code {
    :tasks {
      set command \
          [format "(graph-words-etasks-stasks-ftasks \"%s\" :logscale %s)" \
             $globals(data) \
             [if {$globals(logscale)} {format "t"} {format "nil"}]];
    }
    :time {
      set command \
          [format "(graph-words-first-total \"%s\" :logscale %s)" \
             $globals(data) \
             [if {$globals(logscale)} {format "t"} {format "nil"}]];
    }
  }; # switch
  send_to_lisp :event $command;
 
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
          set condition "$condition || p-name ~ `$phenomena($i)'";
        } else {
          set condition "p-name ~ `$phenomena($i)'";
        }; # else
      }; # if
    }; # foreach
    if {![info exists condition]} {
      tsdb_beep;
      set message "no active phenomena for detailed comparison"
      status "$message; make up your mind ..." 5
    }; # if
  }; # else

  #
  # _fix_me_
  # this is rather awkward; presumably a global list for these attributes
  # (getting used in designing the menu and here) would be better (28-jul-98).
  #
  foreach attribute {i-wf i-input i-category} {
    if {$compare_in_detail(show,$attribute)} {
      set show "$show :$attribute";
    }; # if
  }; # foreach

  foreach attribute {words readings first total gcs error} {
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


proc tsdb_todo {} {

  global globals;

  if {[winfo exists .todo]} {
    .todo destroy
  }; # if
  set directories [file split $globals(podium_home)];
  set directories [lreplace $directories [expr [llength $directories] -1] end];
  set directories [lreplace $directories 0 0];
  set root "$globals(slash)[join $directories $globals(slash)]";
  set todo [file join $root "ToDo"];
  if {[file exists $todo]} {
    show_text $todo .todo "tsdb(1) ToDo List" 80 25;
  }; # if
}; # tsdb_todo()

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
