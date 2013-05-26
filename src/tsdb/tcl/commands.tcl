#
# [incr tsdb()] --- Competence and Performance Profiling Environment
# Copyright (c) 1996 -- 2006 Stephan Oepen (oe@csli.stanford.edu)
# Copyright (c) 2007 -- 2009 Stephan Oepen (oe@ifi.uio.no)
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 2.1 of the License, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
# 

proc tsdb_file {action {index -1}} {

  global globals skeletons test_suites;

  set selection "";
  foreach i [lsort -integer [array names test_suites]] {
    if {![[.list subwidget hlist] info hidden $i]} {
      set item $test_suites($i);
      set name [lindex $item 0];
      if {$globals($name)} {
        set selection "$selection \"$name\"";
      }; # if
    }; # if
  }; #foreach

  if {$action == "create"} {
    if {$index == -1} {
      set command "(create nil)";
      send_to_lisp :event $command;
    } else {
      set skeleton $skeletons($index);
      if {$skeleton != ""} {
        set globals(skeleton) [lindex $skeleton 2];
        tsdb_set "*tsdb-default-skeleton*" "\"[lindex $skeleton 2]\"";
        set command "(create \"[lindex $skeleton 2]\")";
        send_to_lisp :event $command;
      }; # else
    }; # if
  } elseif {$action == "rename"} {
    if {[verify_ts_selection]} {return 1};
    set old $globals(data);
    set aold "$globals(home)$old";
    if {[file exists $aold]} {
      set anew [string range $aold 0 [string last $globals(slash) $aold]];
      if {![input "rename to:" $anew $globals(home) profile]} {
        set anew $globals(input);
        set new [string_strip $globals(home) $anew];
        if {[file exists $anew]} {
          tsdb_beep;
          status "database '$new' already exists" 10;
        } else {
          set parent \
            [string range $anew 0 [string last $globals(slash) $anew]];
          if {[catch {file mkdir $parent}]} {
            tsdb_beep;
            status "error creating parent directory '$parent'" 10;
          } else {
            if {![catch {file rename -force -- $aold $anew}]} {
              set globals(data) $new;
              set globals(relations) {};
              set globals(attributes) {};
              update_ts_list rename $old $new;
            } else {
              tsdb_beep;
              status "mysterious error renaming '$old'" 10;
            }; # else
          }; # else
        }; # else
      }; # if
    }; # if
  } elseif {$action == "reread"} {
    if {[verify_ts_selection]} {return 1};
    update_ts_list update $globals(data);
  } elseif {$action == "strip"} {
    if {[verify_ts_selection]} {return 1};
    set old $globals(data);
    set aold "$globals(home)$old";
    if {[file isdirectory $aold]} {
      set anew [string range $aold 0 [string last $globals(slash) $aold]];
      if {![input "new database:" $anew $globals(home) profile]} {
        set anew $globals(input);
        set new [string_strip $globals(home) $anew];
        if {$new == ""} {
          tsdb_beep;
          status "invalid name for target database" 10;
          return;
        }; # if
        if {[file exists $anew]} {
          tsdb_beep;
          status "database '$new' already exists" 10;
          return;
        }; # if
        set parent \
          [string range $anew 0 [string last $globals(slash) $anew]];
        if {[catch {file mkdir $parent}]} {
          tsdb_beep;
          status "error creating parent directory '$parent'" 10;
          return;
        }; # if
        history_add profile $new;
        set command "(strip  \"$old\" \"$new\")";
        send_to_lisp :event $command;
      }; # if
    }; # if
  } elseif {$action == "purge"} {
    switch -- $index {
      trees { set prefix "clear-cut"; }
      score { set prefix "reset"; }
      default { set prefix "purge"; }
    }; # switch
    if {[verify_ts_selection]} {return 1};
    set prompt [format "%s '%s'" $prefix $globals(data)];
    if {[file isdirectory $globals(home)$globals(data)] 
        && [yes-or-no-p $prompt] == 1} {
      if {$index == "trees"} {
        set command "(purge \"$globals(data)\" :action :tree)";
      } else {
        if {$index == "score"} {
          set command "(purge \"$globals(data)\" :action :score)";
        } else {
          set command "(purge \"$globals(data)\" :action :purge)";
        }; # else
      }; # else
      send_to_lisp :event $command;
    }; # if
  } elseif {$action == "compress"} {

    if {[verify_ts_selection]} {return 1};
    set selection "\"$globals(data)\"";

    foreach profile $selection {
      set old $profile;
      set aold "$globals(home)$old";
      if {[file isdirectory $aold]} {
        if {![input "new database:" $aold $globals(home) profile]} {
          set anew $globals(input);
          set new [string_strip $globals(home) $anew];
          if {$new == ""} {
            set new $old;
            set anew $aold;
          }; # if
          if {[file exists $anew] && [yes-or-no-p "overwrite '$new'"] == 0} {
            return 1;
          }; # if
          catch {file mkdir $anew};
          if {![file isdirectory $anew] || ![file writable $anew]} {
            tsdb_beep;
            status "target directory '$new' not writable" 10;
            return 1;
          }; # if
          set pattern [file join $globals(home) $old *];
          foreach file [glob -type f -nocomplain $pattern] {
            set name [file tail $file];
            set target [file join $anew $name];
            if {[catch {file copy -force $file $anew}]} {
              tsdb_beep;
              status "mysterious error copying '$name'" 10;
              return 1;
            }; # if
            if {$name != "relations" && [file size $target] > 0} {
              status "compressing file '$name' ...";
              if {"$globals(user)" != "bender" && "$globals(user)" != "danf"} {
                after 200;
              }; # if
              if {[catch [eval "exec $globals(zipper) $target"]]} {
                tsdb_beep;
                status "mysterious error compressing '$name'" 10;
                return 1;
              }; # if
            }; # if
          }; # foreach
          history_add profile $new;
          send_to_lisp :event "(list)";
        }; # if
      }; # if
    }; # foreach
  } elseif {$action == "export"} {
    if {[verify_ts_selection]} {return 1};
    if {![input "target directory:" "/tmp" "" export]} {
      set target $globals(input);
      if {[catch {file mkdir $target}]} {
        tsdb_beep;
        status "error creating target directory '$target'" 10;
        return 1;
      }; # if
      if {![file writable $target]} {
        tsdb_beep;
        status "target directory '$target' not writable" 10;
        return 1;
      }; # if
      history_add directory $target;
      set command "(export :redwoods \"$globals(data)\" \"$target\")";
      send_to_lisp :event $command;
    }; # if
  } elseif {$action == "delete"} {
    if {[verify_ts_selection]} {return 1};
    set old $globals(data);
    set aold "$globals(home)$old";
    if {[file isdirectory $aold] 
        && [yes-or-no-p "delete '$old'"] == 1} {
      set aold "[file dirname [file join $aold .]]$globals(slash)"
      set files [glob -nocomplain -- "$aold/*"];
      foreach afile $files {
        set file [string_strip $globals(home) $afile];
        status "deleting file '$file' ...";
        if {[catch {file delete -force -- $afile}]} {
          tsdb_beep;
          status "deleting file '$file' ... failed";
          after 1000;
        }; # if
        if {"$globals(user)" != "bender"} {
          after 300;
        }; # if
      }; # foreach
      status "deleting directory '$old' ...";
      if {[catch {file delete -force -- $aold}]} {
        status "deleting directory '$old' ... failed";
        after 2000;
      }; # if
      after 500;
      if {[file exists $aold]} {
        tsdb_beep;
        status "deletion of '$old' may be incomplete" 10;
      } else {
        status "database '$old' successfully deleted" 10;
      }; # else
      set globals(data) "";
      update_ts_list delete $old;
    }; # if
  }; # elseif
}; # tsdb_file()


proc tsdb_import {code} {

  global globals;

  if {$code == "ascii" || $code == "bitext" || $code == "rasp"} {
    if {![input "item file:" [pwd] "" import]} {
      set source $globals(input);
      if {![file isfile $source] || ![file readable $source]} {
        tsdb_beep;
        status "invalid file name '$source'" 10;
        return;
      }; # if
      history_add import $source;

      if {$code == "bitext"} {
        set prompt "new source database:";
      } else {
        set prompt "new database:";
      }; # else
      if {![input $prompt "" $globals(home) profile]} {
        set atarget $globals(input);
        set target [string_strip $globals(home) $atarget];
        if {$target == ""} {
          tsdb_beep;
          status "invalid name for target database" 10;
          return;
        }; # if
        if {[file exists $atarget]} {
          tsdb_beep;
          status "database '$target' already exists" 10;
          return;
        }; # if
        set parent \
          [string range $atarget 0 [string last $globals(slash) $atarget]];
        if {[catch {file mkdir $parent}]} {
          tsdb_beep;
          status "error creating parent directory '$parent'" 10;
          return;
        }; # if
        history_add profile $target;

        if {$code == "bitext"} {
          set prompt "new target database:";
          if {![input $prompt "" $globals(home) profile]} {
            set attarget $globals(input);
            set ttarget [string_strip $globals(home) $attarget];
            if {$ttarget == ""} {
              tsdb_beep;
              status "invalid name for target database" 10;
              return;
            }; # if
            if {[file exists $attarget]} {
              tsdb_beep;
              status "database '$ttarget' already exists" 10;
              return;
            }; # if
            set tparent \
              [string range \
               $attarget 0 [string last $globals(slash) $attarget]];
            if {[catch {file mkdir $tparent}]} {
              tsdb_beep;
              status "error creating parent directory '$tparent'" 10;
              return;
            }; # if

            history_add profile $ttarget;
            set command \
                [format \
                 "(import :items \"%s\" \"%s\" :format :%s :target \"%s\")" \
                 $source $target $code $ttarget];
            send_to_lisp :event $command;
          }; # if
        } else {
          set command \
              [format \
               "(import :items \"%s\" \"%s\" :format :%s)" \
               $source $target $code];
          send_to_lisp :event $command;
        }; # else
      }; # if
    }; # if
  } else {
    if {![input "tsdb(1) database:" [pwd] "" directory]} {
      set source $globals(input);
      if {![file isdirectory $source] 
          || ![file readable $source$globals(slash)relations]} {
        tsdb_beep;
        status "invalid tsdb(1) database '$source'" 10;
        return;
      }; # if
      history_add directory $source;

      if {![input "new database:" "" $globals(home) profile]} {
        set atarget $globals(input);
        set target [string_strip $globals(home) $atarget];
        if {$target == ""} {
          tsdb_beep;
          status "invalid name for target database" 10;
          return;
        }; # if
        if {[file exists $atarget]} {
          tsdb_beep;
          status "database '$target' already exists" 10;
          return;
        }; # if
        set parent \
          [string range $atarget 0 [string last $globals(slash) $atarget]];
        if {[catch {file mkdir $parent}]} {
          tsdb_beep;
          status "error creating parent directory '$parent'" 10;
          return;
        }; # if

        history_add profile $target;
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
            status "invalid directoy '$path'" 10;
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
            status "invalid directoy '$path'" 10;
          } else {
            set path "[file dirname [file join $path .]]$globals(slash)"
            set globals(skeleton_directory) $path;
            tsdb_set "*tsdb-skeleton-directory*" "\"$path\"";
            tsdb_update skeletons;
          }; # else
        }; # else
      }; # if
    }
    analyses {
      if {![integer_input "maximal number of analyses" \
                          $globals(maximal_number_of_analyses)]} {
        if {$globals(integer,lvalue) == ""} {
          set globals(integer,lvalue) 0;
        }; # if
        set globals(maximal_number_of_analyses) $globals(integer,lvalue);
        tsdb_set "*tsdb-maximal-number-of-analyses*" \
                 $globals(maximal_number_of_analyses);
      }; # if
    }
    pedges {
      if {![integer_input "maximal number of (passive) edges" \
                          $globals(maximal_number_of_edges)]} {
        if {$globals(integer,lvalue) == ""} {
          set globals(integer,lvalue) 0;
        }; # if
        set globals(maximal_number_of_edges) $globals(integer,lvalue);
        tsdb_set "*tsdb-maximal-number-of-edges*" \
                 $globals(maximal_number_of_edges);
      }; # if
    }
    results {
      if {![integer_input "maximal number of result details" \
                          $globals(maximal_number_of_results)]} {
        if {$globals(integer,lvalue) == ""} {
          set globals(integer,lvalue) 0;
        }; # if
        set globals(maximal_number_of_results) $globals(integer,lvalue);
        tsdb_set "*tsdb-maximal-number-of-results*" \
                 $globals(maximal_number_of_results);
      }; # if
    }
    delay {
      if {![integer_input \
              "delay for automatic update (seconds)" \
              $globals(tree,delay)]} {
        if {$globals(integer,lvalue) == ""} {
          set globals(integer,lvalue) 0;
        }; # if
        set globals(tree,delay) $globals(integer,lvalue);
        tsdb_set automatic_update_p;
      }; # if
    }
    beam {
      if {![integer_input \
              "maximal size of scoring beam" \
              $globals(tree,beam)]} {
        if {$globals(integer,lvalue) == ""} {
          set globals(integer,lvalue) 1;
        }; # if
        set globals(tree,beam) $globals(integer,lvalue);
      }; # if
    }
    nfold {
      if {![integer_input \
              "number of cross-validation runs" \
              $globals(tree,nfold)]} {
        if {$globals(integer,lvalue) == ""} {
          set globals(integer,lvalue) 0;
        }; # if
        set globals(tree,nfold) $globals(integer,lvalue);
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

  if {$name == "complete" || $name == "division"} {
    set globals(division,analyzed) 1;
    update_condition_cascade analyzed division;
  }; # if

  if {$name == "complete" || $name == "condition"} {
    update_condition_cascade null condition;
  }; # if

  if {$name == "complete" || $name == "phenomena"} {
    update_phenomena_cascade reset division;
    update_phenomena_cascade reset condition;
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


proc tsdb_capture {} {

  global globals test_suites;

  if {[verify_ts_selection "" "write"]} {return 1};
  set data $globals(data);
  if {[input "comment:" "" "" comment]} {
    return;
  }; # if
  history_add comment $globals(input);
  set comment [lispify_string $globals(input)];
  set command \
    [format \
     "(capture \"%s\" :comment \"%s\" :overwrite %s)" \
      $data $comment [lispify_truth_value $globals(overwrite)]];
  send_to_lisp :event $command 0 1;

}; # tsdb_capture()


proc tsdb_browse {code {condition ""} {globalp 1} {profile ""} {goldp 0}} {

  global globals test_suites compare_in_detail;

  if {$profile == ""} {
    if {[verify_ts_selection]} {return 1};
    set profile $globals(data);
  }; # if

  if {$code != "runs" && $code != "errors"
      && $globalp && $globals(condition) != ""} {
    if {$condition != ""} {
      set condition "$globals(condition) and ($condition)";
    }; # if
  }; # if

  switch $code {
    items {
      set attributes "(\"i-id\" \"i-input\" \"i-length\" \"i-wf\" \"i-category\")";
      set relations "(\"item\")";
    }
    phenomena {
      set attributes "(\"p-id\" \"p-name\" \"p-author\" \"p-date\")";
      set relations "(\"phenomenon\")";
    }
    runs {
      set attributes "(\"run-id\" \"r-comment\" \"run-comment\" \"platform\" \"application\" \"grammar\" \"avms\" \"sorts\" \"templates\" \"lexicon\" \"lrules\" \"rules\" \"user\" \"host\" \"start\" \"end\" \"items\" \"status\")";
      set relations "(\"run\")";
      set condition nil;
    }
    parses {
      set attributes "(\"i-id\" \"i-input\" \"readings\" \"words\" \"first\" \"total\" \"tcpu\" \"tgc\" \"p-ftasks\" \"p-etasks\" \"p-stasks\" \"aedges\" \"pedges\"  \"raedges\" \"rpedges\" \"comment\")";
      set relations "(\"item\" \"parse\")";
    }
    results {
      set command [format "(results \"%s\")" $profile];
      send_to_lisp :event $command;
      return 0;
    }
    trees {
      #
      # _fix_me_
      # we used to say `:condition $condition' here and always set $condition
      # to $globals(condition) above; hence, it was not possible for francis to
      # set creatively set *statistics-select-condition* in Lisp and have that
      # take effect.  the following should preserve the original functionality,
      # we hope.                                                (4-feb-04; oe)
      #
      if {$condition != ""} {
        set command [format "(trees \"%s\" :condition \"%s\" :interactive %s" \
                     $profile $condition \
                     [lispify_truth_value [expr {!$globalp}]]];
      } else {
        set command [format "(trees \"%s\" :interactive %s" \
                     $profile [lispify_truth_value [expr {!$globalp}]]];

      }; # else
      if {$goldp} {
        if {[verify_ts_selection both]} {return 1};
        set gold $compare_in_detail(source);
        #
        # _fix_me_ 
        # we somehow still need something like `ts_list find'  (8-oct-02)
        #
        for {set i 0} {$i < [array size test_suites]} {incr i} {
          if {![string compare $gold [lindex $test_suites($i) 0]]} {
            set index $i;
            break;
          }; # if
        }; # for
        if {![info exists index] 
            || ![lindex $test_suites($index) 6]} {
          tsdb_beep;
          status [format "no tree data available for '%s' ... |:-\{" $gold] 10;
          return 1;
        }; # if
        set command "$command :gold \"$gold\"";
      }; # if
      set command "$command)";
      send_to_lisp :event $command;
      return 0;
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

  if {[info exists attributes]} {
    if {$condition == ""} {
      set command [format "(select \"%s\" %s nil %s)" \
                   $profile $attributes $relations];
    } elseif {$condition == "nil"} {
      set command [format "(select \"%s\" %s nil %s \"\")" \
                   $profile $attributes $relations];
    } else {
      set command [format "(select \"%s\" %s nil %s \"%s\")" \
                   $profile $attributes $relations $condition];
    }; # else
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

  global globals test_suites compare_in_detail;

  if {$data == "" && [verify_ts_selection "" "write"]} {return 1};

  if {($globals(process,type) == ":transfer" ||
       $globals(process,type) == ":generate")
      && [verify_ts_selection both]} {
    return 1;
  }; # if

  switch $code {
    all {set condition ""}
    positive {set condition "i-wf == 1"}
    negative {set condition "i-wf == 0"}
    condition {
      set current [unlispify_string $globals(condition)];
       if {[condition_input "where" $current where]} {
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
  set source "";
  if {$globals(process,type) == ":transfer"
      || $globals(process,type) == ":generate"} {
    set source " :gold \"$compare_in_detail(source)\"";
  }; # if
  set command \
    [format \
     "(process \"%s\" :condition \"%s\" :comment \"%s\" :type %s \
               :overwrite %s :interactive %s :vocabulary %s%s)" \
      $data $condition $comment $globals(process,type) \
      $overwrite $interactive \
      [lispify_truth_value $globals(autoload_vocabulary)] \
      $source];
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
    inspect {
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


proc tsdb_html {toplevel} {

    set command [format "(html (:toplevel . \"%s\"))" $toplevel];
    send_to_lisp :event $command;

}; # tsdb_html()


proc analyze_competence {code} {

  global globals;

  if {[verify_ts_selection]} {return 1};

  set command \
      [format "(analyze-competence \"%s\" :wf %d" $globals(data) $code];
  if {$globals(division) != ""} {
    set command "$command :division \"$globals(division)\"";
  }; # if
  set command "$command)";
  send_to_lisp :event $command;

}; # analyze_competence()


proc analyze_performance {{code "performance"}} {

  global globals;

  if {[verify_ts_selection]} {return 1};

  set command \
      [format "(analyze-performance \"%s\" :view :%s)" $globals(data) $code];
  send_to_lisp :event $command;

}; # analyze_performance()


proc analyze_trees {{code "trees"}} {

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
      || ![lindex $test_suites($index) 6]} {
    tsdb_beep;
    status [format "no tree data available for '%s' ... |:-\{" \
            $globals(data)] 10;
  } else {
    set command \
      [format "(analyze-%s \"%s\")" $code $globals(data)];
    send_to_lisp :event $command;
  }; # else

}; # analyze_trees()


proc analyze_update {{code ""}} {

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
      || ![lindex $test_suites($index) 6]} {
    tsdb_beep;
    status [format "no tree data available for '%s' ... |:-\{" \
            $globals(data)] 10;
  } else {
    set command \
      [format "(analyze-update \"%s\")" $globals(data)];
    send_to_lisp :event $command;
  }; # else

}; # analyze_trees()


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
      || ![lindex $test_suites($index) 5]} {
    tsdb_beep;
    status [format "no rule data available for '%s' ... |:-\{" \
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
  } elseif {$globals(division) != ""} {
    set command "$command :division \"$globals(division)\"";
  }; # else
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
    if {$globals(division) != ""} {
      set command \
        "$command :title \"Aggregate Size (divided by '$globals(division)')\"";
    } else {
      set command "$command :title \"Aggregate Size\"";
    }; # else
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

  set command [format "(detail \"%s\" \"%s\" :show (%s) :compare (%s) \
                               :subsetp %s :bestp %s)" \
                      $compare_in_detail(source) $globals(data) \
                      $show $compare \
                      [lispify_truth_value $globals(detail,subsetp)] \
                      [lispify_truth_value $globals(detail,bestp)]];
  send_to_lisp :event $command;
 
}; # tsdb_compare_in_detail()

proc tsdb_evolution {code} {

  global globals test_suites;

  set list [.list subwidget hlist];

  set selection "";
  foreach i [lsort -integer [array names test_suites]] {
    if {![$list info hidden $i]} {
      set item $test_suites($i);
      set name [lindex $item 0];
      if {$globals($name)} {
        set selection "$selection \"$name\"";
      }; # if
    }; # if
  }; #foreach

  if {$selection == ""} {
    tsdb_beep;
    status "no test suite database(s) active; make up your mind ... |:-\}" 5;
    return 1;
  }; # if

  set attributes "";
  foreach attribute $globals(evolution,all) {
    if {$globals(evolution,$attribute)} {
      set attributes "$attributes :$attribute";
    }; # if
  }; # foreach

  if {$attributes == ""} {
    tsdb_beep;
    status "no evolutionary attributes active; make up your mind ... |:-\}" 5;
    return 1;
  }; # if
  
  set command "(evolution ($selection)";
  if {$globals(division) != ""} {
    set command "$command :division \"$globals(division)\"";
  }; # if
  set command "$command :attributes ($attributes))";

  send_to_lisp :event $command;

}; # tsdb_evolution()

proc tsdb_trees {action {modifier ""}} {

  global globals test_suites compare_in_detail;

  if {$action != "features" && [verify_ts_selection]} {return 1};

  set target $globals(data);
  if {![info exists compare_in_detail(source)] \
      || $compare_in_detail(source) == ""} {
    set source $target;
  } else {
    set source $compare_in_detail(source);
  }; # else

  set spartanp false;
  #
  # _fix_me_ 
  # we somehow still need something like `ts_list find'  (29-oct-02)
  #
  for {set i 0} {$i < [array size test_suites]} {incr i} {
    if {![string compare $target [lindex $test_suites($i) 0]]} {
      set index $i;
      break;
    }; # if
  }; # for
  if {![info exists index] || ![lindex $test_suites($index) 4]} {
    set spartanp true;
  }; # if

  set command "";
  if {$action == "features"} {
    set list [.list subwidget hlist];

    set selection "";
    foreach i [lsort -integer [array names test_suites]] {
      if {![$list info hidden $i]} {
        set item $test_suites($i);
        set name [lindex $item 0];
        if {$globals($name)} {
          set selection "$selection \"$name\"";
        }; # if
      }; # if
    }; #foreach

    if {$selection == ""} {
      if {[verify_ts_selection]} {return 1};
      set selection "\"$globals(data)\"";
    }; # if
    set command \
      [format \
       "(operate-on-profiles (%s) :task :%s)" \
       $selection [expr {$modifier == "clear" ? "unfc" : "fc"}]];
  } elseif {$action == "train"} {
    set selection "\"$globals(data)\"";

    if {![input "output file:" "/tmp/" "" train]} {
      set file $globals(input);
      if {[file isdirectory $file]
           || [file exists $file] && ![file writable $file]} {
        tsdb_beep;
        status "target file '$file' not writable" 5;
        after 5000;
        return [tsdb_trees $action];
      }; # if
      if {[file exists $file] 
          && [yes-or-no-p "overwrite existing '$file'"] != 1 } {
        return [tsdb_trees $action];
      }; # if

      set command \
        [format \
         "(train %s \"%s\" :type %s)" \
         $selection $file $globals(tree,model)];

    }; # if
  } elseif {$action == "score"} {
    set command \
      [format \
       "(analyze-scores \"%s\" \"%s\" :scorep %s :spartanp %s \
         :n %d :test %s :loosep %s)" \
       $target $source \
       [lispify_truth_value $globals(tree,scorep)] \
       [lispify_truth_value $spartanp] \
       $globals(tree,beam) $globals(tree,comparison) \
       [lispify_truth_value $globals(tree,loosep)]];
  } elseif {$action == "errors"} {
    set command \
      [format \
       "(analyze-errors \"%s\" \"%s\" :scorep %s :spartanp %s \
         :n %d :test %s :loosep %s)" \
       $target $source \
       [lispify_truth_value $globals(tree,scorep)] \
       [lispify_truth_value $spartanp] \
       $globals(tree,beam) $globals(tree,comparison) \
       [lispify_truth_value $globals(tree,loosep)]];
  } elseif {$action == "compare"} {
    set command \
      [format \
       "(analyze-agreement \"%s\" \"%s\")" \
       $target $source];
  } elseif {$action == "rank"} {
      set command \
        [format \
         "(rank-profile \"%s\" \"%s\" :nfold %d :type %s)" \
         $source $target $globals(tree,nfold) $globals(tree,model)];
  }; # elseif

  if {$command != ""} {
    send_to_lisp :event $command;
  }; # if

}; # tsdb_trees()

proc tsdb_readers {} {

  global globals;

  switch $globals(readers,mrs) {
    nil -
    "\"mrs::read-mrs-from-string\"" -
    "\"mrs::read-rmrs-from-string\"" {
      set command "(set (:reader :mrs) $globals(readers,mrs))";
      send_to_lisp :event $command;
    }
  }; # switch

}; # tsdb_readers()

proc tsdb_filters {} {

  global globals;

  set code "(";
  if {$globals(filters,sparseness)} { set code "$code :sparseness"; }
  if {$globals(filters,predicate)} { set code "$code :predicate"; }
  if {$globals(filters,lnk)} { set code "$code :lnk"; }
  if {$globals(filters,syntax)} { set code "$code :syntax"; }
  if {$globals(filters,semi)} { set code "$code :semi"; }
  if {$globals(filters,ascope)} { set code "$code :ascope"; }
  if {$globals(filters,cscope)} { set code "$code :cscope"; }
  if {$globals(filters,unet)} { set code "$code :unet"; }
  if {$globals(filters,uscope)} { set code "$code :uscope"; }
  if {$globals(filters,fragmentation)} { set code "$code :fragmentation"; }
  if {$globals(filters,cycle)} { set code "$code :cycle"; }
  if {$globals(filters,connectivity)} { set code "$code :connectivity"; }
  set code "$code)";
  tsdb_set "*filter-test*" $code;

}; # tsdb_filters()

proc tsdb_switch {code} {

  global globals;
 
}; # tsdb_switch()

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
