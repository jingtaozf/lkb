#!/bin/sh
# the next line restarts using wish(1) \
exec /coli/apps/tcl+tk/bin/wish++ "$0" "$@"

wm title . "tsdb(1) podium"
wm iconname . "tsdb(1) podium"

#
# import BLT library (for `table' and `graph' widgets)
#
namespace import blt::*
namespace import -force blt::tile::*

#
# all state is encoded in a global (associative) array `globals'
#
if {![info exists globals(podium_home)]} {
  set globals(podium_home) "/user/oe/src/page/src/tsdb/tcl/";
}; # if
if {![info exists globals(home)]} {
  set globals(home) "/user/oe/src/page/src/tsdb/small/";
}; # if
if {![info exists globals(data)]} {
  set globals(data) "";
}; # if
if {![info exists globals(balloons)]} {
  set globals(balloons) "$globals(podium_home)balloons";
}; # if
set globals(default_skeleton) "english"
set globals(status) "initializing tsdb(1) podium"
set globals(balloon) "";
set globals(browse_condition) "";
set globals(overwrite) 1;
set globals(logscale) 0;
set globals(selection) -1;
set globals(input) "";
set globals(errno) 0;
set globals(gensym) 0;
set globals(slash) "/";
set globals(user) $env(USER)

#
# determine defaults for (display) options local to the podium(1) universe
#
set compare_in_detail(source) "";
set compare_in_detail(phenomena,all) 1;
set compare_in_detail(show,i-input) 1;
set compare_in_detail(compare,words) 0;
set compare_in_detail(compare,readings) 1;
#
#
# load table display modules by oliver (genius) `plaehn@coli.uni-sb.de' 
#
source "$globals(podium_home)goodies.tcl"
source "$globals(podium_home)table.tcl"
source "$globals(podium_home)showtable.tcl"
source "$globals(podium_home)showgraph.tcl"
source "$globals(podium_home)utilities.tcl"
source "$globals(podium_home)podium_commands.tcl"
source "$globals(podium_home)/balloon.tcl"

#
# log activity to trace file (for debugging)
#
if {$globals(user) == ""} {
  set globals(user) "unknown";
}; # if
set trace [open "/tmp/podium.debug.$globals(user)" "w"];


proc logger {string} {

    global trace;

    if {[info exists trace]} {
        puts $trace $string;
        flush $trace;
    }; # if

}; # logger()


proc main {} {

  global globals;

  #
  # withdraw podium application until it is fully decorated
  #
  wm withdraw .

  #
  # decorate top-level container and create menu bar
  # 
  frame .menu -relief raised -bd 2
  pack .menu -side top -fill x

  menubutton .menu.file \
             -text "File" -underline 0 -menu .menu.file.menu
  menubutton .menu.browse \
             -text "Browse" -underline 0 -menu .menu.browse.menu
  menubutton .menu.process \
             -text "Process" -underline 0 -menu .menu.process.menu
  menubutton .menu.analyze \
             -text "Analyze" -underline 0 -menu .menu.analyze.menu
  menubutton .menu.compare \
             -text "Compare" -underline 0 -menu .menu.compare.menu
  menubutton .menu.detail \
             -text "Detail" -underline 0 -menu .menu.detail.menu
  menubutton .menu.help \
             -text "Help" -underline 0 -menu .menu.help.menu
  pack .menu.file .menu.browse .menu.process \
       .menu.analyze .menu.compare .menu.detail -side left
  pack .menu.help -side right

  #
  # `File' menu (and embedded cascades)
  #
  menu .menu.file.menu -tearoff 0
  .menu.file.menu add command -label "Rename" \
    -command {tsdb_file rename}
  .menu.file.menu add command -label "Reread" \
    -command {tsdb_file reread}
  .menu.file.menu add command -label "Purge" \
    -command {tsdb_file purge}
  .menu.file.menu add command -label "Delete" \
    -command {tsdb_file delete}
  .menu.file.menu add separator
  .menu.file.menu add cascade -label "Create" \
    -menu .menu.file.menu.create
  .menu.file.menu add separator
  .menu.file.menu add cascade -label "Directories" \
    -menu .menu.file.menu.directories
  .menu.file.menu add cascade -label "Options" \
    -menu .menu.file.menu.options
  .menu.file.menu add separator
  .menu.file.menu add cascade -label "Update" \
    -menu .menu.file.menu.update
  .menu.file.menu add command -label "Close" -command tsdb_close 
  .menu.file.menu add command -label "Quit" -command tsdb_quit

  menu .menu.file.menu.update -tearoff 0
  .menu.file.menu.update add command -label "Skeleton List" \
    -command [list tsdb_update skeletons]
  .menu.file.menu.update add command -label "Database List" \
    -command [list tsdb_update all]
  .menu.file.menu.update add separator;
  .menu.file.menu.update add command -label "All tsdb(1) Status" \
    -command [list tsdb_update complete]

  #
  # the `File.Create' (cascaded) menu will be filled in dynamically
  #
  menu .menu.file.menu.create -tearoff 0

  menu .menu.file.menu.directories -tearoff 0
  .menu.file.menu.directories add command -label "Database Root" \
    -command "tsdb_option home"
  .menu.file.menu.directories add command -label "Skeleton Root" \
    -command "tsdb_option skeleton_directory"

  menu .menu.file.menu.options -tearoff 0
  .menu.file.menu.options add checkbutton -label "Exhaustive Search" \
    -variable globals(exhaustive_p) -command {tsdb_set exhaustive_p};
  .menu.file.menu.options add separator
  .menu.file.menu.options add checkbutton -label "Write `run' Relation" \
    -variable globals(write_run_p) -command {tsdb_set write_run_p};
  .menu.file.menu.options add checkbutton -label "Write `parse' Relation" \
    -variable globals(write_parse_p) -command {tsdb_set write_parse_p};
  .menu.file.menu.options add checkbutton -label "Write `result' Relation" \
    -variable globals(write_result_p) -command {tsdb_set write_result_p};
  .menu.file.menu.options add checkbutton -label "Write `output' Relation" \
    -variable globals(write_output_p) -command {tsdb_set write_output_p};
  .menu.file.menu.options add checkbutton -label "Write Lexicon Chart" \
    -variable globals(write_lexicon_chart_p) \
    -command {tsdb_set write_lexicon_chart_p};
  .menu.file.menu.options add checkbutton -label "Write Syntax Chart" \
    -variable globals(write_syntax_chart_p) \
    -command {tsdb_set write_syntax_chart_p};
  .menu.file.menu.options add separator
  .menu.file.menu.options add radiobutton -label "On Demand Garbage Collect" \
    -variable globals(gc_p) -value nil -command {tsdb_set gc_p}
  .menu.file.menu.options add radiobutton -label "Preliminary  Scavenge" \
    -variable globals(gc_p) -value :local -command {tsdb_set gc_p}
  .menu.file.menu.options add radiobutton \
    -label "Preliminary Garbage Collect" \
    -variable globals(gc_p) -value :global -command {tsdb_set gc_p}
  .menu.file.menu.options add separator
  .menu.file.menu.options add checkbutton -label "Overwrite Test Run" \
    -variable globals(overwrite);
  .menu.file.menu.options add checkbutton -label "Logarithmic Scales" \
    -variable globals(logscale);

  #
  # `Browse' menu (and embedded cascades)
  #
  menu .menu.browse.menu -tearoff 0
  .menu.browse.menu add command -label "Database Schema" \
      -command tsdb_browse_relations
  .menu.browse.menu add command -label "Vocabulary" \
      -command tsdb_browse_vocabulary
  .menu.browse.menu add separator
  .menu.browse.menu add cascade \
      -label "Test Items" -menu .menu.browse.menu.items
  .menu.browse.menu add cascade \
      -label "Phenomena" -menu .menu.browse.menu.phenomena
  .menu.browse.menu add command \
      -label "Test Run(s)" -command {tsdb_browse runs ""}
  .menu.browse.menu add cascade \
      -label "Parses" -menu .menu.browse.menu.parses
  .menu.browse.menu add cascade \
      -label "Results" -menu .menu.browse.menu.results
  .menu.browse.menu add cascade \
    -label "Errors" -menu .menu.browse.menu.errors
  .menu.browse.menu add separator
  .menu.browse.menu add command -label "Condition" \
    -command tsdb_browse_condition;
  .menu.browse.menu add command -label "Custom Query"

  menu .menu.browse.menu.items -tearoff 0
  .menu.browse.menu.items add command -label "All Test Items" \
      -command {tsdb_browse items ""}
   menu .menu.browse.menu.phenomena -tearoff 0
  .menu.browse.menu.phenomena add command -label "All Phenomena" \
      -command {tsdb_browse phenomena ""}
  menu .menu.browse.menu.parses -tearoff 0
  .menu.browse.menu.parses add command -label "All Parses" \
      -command {tsdb_browse parses ""}
  menu .menu.browse.menu.results -tearoff 0
  .menu.browse.menu.results add command -label "All Results" \
      -command {tsdb_browse results ""}
  menu .menu.browse.menu.errors -tearoff 0
  .menu.browse.menu.errors add command -label "All Errors" \
      -command {tsdb_browse errors ""}

  #
  # `Process' menu (and embedded cascades)
  #
  menu .menu.process.menu -tearoff 0
  .menu.process.menu add command -label "Vocabulary" \
          -command {tsdb_browse_vocabulary 1}
  .menu.process.menu add separator
  .menu.process.menu add command -label "All Items" \
      -command {tsdb_process all}
  .menu.process.menu add command -label "Positive Items" \
      -command {tsdb_process positive}
  .menu.process.menu add command -label "Negative Items" \
      -command {tsdb_process negative}
  .menu.process.menu add separator
  .menu.process.menu add command -label "Interrupt" -command {tsdb_abort}

  #
  # `Analyze' menu (and embedded cascades)
  #
  menu .menu.analyze.menu -tearoff 0
  .menu.analyze.menu add command \
          -label "Coverage" \
          -command {analyze_competence 1}
  .menu.analyze.menu add command \
          -label "Overgeneration" \
          -command {analyze_competence 0}
  .menu.analyze.menu add cascade \
          -label "Performance" -command {analyze_performance :all} \
          -menu .menu.analyze.menu.performance
  .menu.analyze.menu add separator
  .menu.analyze.menu add cascade \
          -label "Condition" \
          -menu .menu.analyze.menu.condition
  .menu.analyze.menu add cascade \
          -label "Aggregate" \
          -menu .menu.analyze.menu.aggregate
  .menu.analyze.menu add separator
  .menu.analyze.menu add command \
          -label "Parsing Times" \
          -command {tsdb_graph :time}
  .menu.analyze.menu add command \
          -label "Parser Tasks" \
          -command {tsdb_graph :tasks}
  .menu.analyze.menu add command \
          -label "Rule Statistics" \
          -command {analyze_rules}

  menu  .menu.analyze.menu.performance -tearoff 0
  .menu.analyze.menu.performance add command \
          -label "All Items" -command {analyze_performance :all}
  .menu.analyze.menu.performance add command \
          -label "Positive Items" -command {analyze_performance :positive}
  .menu.analyze.menu.performance add command \
          -label "Negative Items" -command {analyze_performance :negative}
  .menu.analyze.menu.performance add command \
          -label "Analyzed Items" -command {analyze_performance :analyzed}
  .menu.analyze.menu.performance add command \
          -label "Unanalyzed Items" -command {analyze_performance :unanalyzed}

  menu .menu.analyze.menu.condition -tearoff 0
  menu .menu.analyze.menu.aggregate -tearoff 0

  #
  # `Compare' menu (and embedded cascades)
  #
  menu .menu.compare.menu -tearoff 0
  .menu.compare.menu add command -label "Competence" \
    -command {tsdb_compare competence};
  .menu.compare.menu add command -label "Performance" \
    -command {tsdb_compare performance};
  .menu.compare.menu add separator
  .menu.compare.menu add cascade -label "Source Database" \
    -menu .menu.detail.menu.compare
   .menu.compare.menu add separator
  .menu.compare.menu add cascade -label "Aggregate" \
    -menu .menu.compare.menu.aggregate -state disabled;
  menu .menu.compare.menu.aggregate -tearoff 0

  #
  # `Detail' menu (and embedded cascades)
  #
  menu .menu.detail.menu -tearoff 0
  .menu.detail.menu add command -label "Compare" \
    -command tsdb_compare_in_detail
  .menu.detail.menu add separator
  .menu.detail.menu add cascade -label "Source Database" \
    -menu .menu.detail.menu.compare
  .menu.detail.menu add cascade -label "Phenomena" \
    -menu .menu.detail.menu.phenomena
  .menu.detail.menu add cascade -label "Decoration" \
    -menu .menu.detail.menu.decoration
  .menu.detail.menu add cascade -label "Intersection" \
    -menu .menu.detail.menu.intersection

  menu .menu.detail.menu.phenomena -tearoff 0
  .menu.detail.menu.phenomena add checkbutton -label "All Phenomena" \
    -variable compare_in_detail(phenomena,all)

  menu .menu.detail.menu.decoration -tearoff 0
  .menu.detail.menu.decoration add checkbutton -label "i-wf" \
    -variable compare_in_detail(show,i-wf)
  .menu.detail.menu.decoration add checkbutton -label "i-input" \
    -variable compare_in_detail(show,i-input)
  .menu.detail.menu.decoration add checkbutton -label "i-category" \
    -variable compare_in_detail(show,i-category)

  menu .menu.detail.menu.intersection -tearoff 0
  .menu.detail.menu.intersection add checkbutton -label "words" \
    -variable compare_in_detail(compare,words)
  .menu.detail.menu.intersection add checkbutton -label "readings" \
    -variable compare_in_detail(compare,readings)
  .menu.detail.menu.intersection add checkbutton -label "first" \
    -variable compare_in_detail(compare,first)
  .menu.detail.menu.intersection add checkbutton -label "total" \
    -variable compare_in_detail(compare,total)
  .menu.detail.menu.intersection add checkbutton -label "gcs" \
    -variable compare_in_detail(compare,gcs)
  .menu.detail.menu.intersection add checkbutton -label "error" \
    -variable compare_in_detail(compare,error)

  menu .menu.detail.menu.compare -tearoff 0

  #
  # `Help' menu (and embedded cascades)
  #
  menu .menu.help.menu -tearoff 0
  .menu.help.menu add command -label "tsdb(1) ToDo List" \
    -command tsdb_todo;

  tk_menuBar .menu .menu.file .menu.analyze .menu.help
  focus .menu

  frame .body -width 19.1c -height 4c
  pack .body -side top -expand yes -fill y;

  #
  # the bottom line: podium status display 
  #
  frame .status -relief raised -bd 2
  tixMeter .status.meter -value 1
  label .status.label \
          -relief sunken -bd 3 -font {helvetica 10} \
          -textvariable globals(status);
  label .status.balloon \
          -relief flat -bd 0 -font {helvetica 10} -bg yellow \
           -padx 0 -pady 0 -textvariable globals(balloon);

  tixLabelEntry .status.entry \
          -relief flat -bd 0 \
          -options { label.font {Helvetica 10} \
                     label.padx 0 label.pady 0 \
                     entry.font {Courier 10} \
                     entry.relief flat entry.highlightThickness 0 }
  [.status.entry subwidget label] config \
    -bg [[.status.entry subwidget entry] cget -bg]
  pack .status -side bottom -fill x -expand no
  pack .status.meter -side left -padx 2
  pack .status.label -side left -padx 1 -pady 1 -fill both -expand yes

  pack .status.balloon -in .status.label -fill both -expand yes -padx 0 -pady 0
  lower .status.balloon

  pack .status.entry -in .status.balloon -fill both -expand yes -padx 0 -pady 0
  lower .status.entry
  status "initializing tsdb(1) podium ..." 2

  #
  # body of tsdb(1) podium: a scrolled multi-column listbox
  #
  tixScrolledHList .list -width 19c -scrollbar "auto +y" \
          -options { hlist.columns 5 hlist.height 4\
                     hlist.header true hlist.itemtype text};
  set list [.list subwidget hlist];
  $list config -selectmode single \
          -browsecmd tsdb_list_select -command tsdb_list_run

  bind $list <Button-2> {
    set list [.list subwidget hlist];
    set entry [$list nearest %y];
    if {$entry != "" && [info exists test_suites($entry)]} {
      if {$compare_in_detail(source) == [lindex $test_suites($entry) 0]} {
        set compare_in_detail(source) "";
      } else {
        set compare_in_detail(source) [lindex $test_suites($entry) 0];
        set globals(selection) $entry;
      }; # else
    }; # if
    update_ts_list;
  }; # bind

  bind $list <Control-Double-Button-1> {
    tsdb_compare_in_detail;
    break;
  }; # bind
  bind $list <Shift-Double-Button-1> [bind $list <Control-Double-Button-1>];
  bind $list <Alt-Double-Button-1> [bind $list <Control-Double-Button-1>];

  $list column width 0 10c
  $list column width 1 2c
  $list column width 2 2c
  $list column width 3 2c
  $list column width 4 2c

  set hleft [tixDisplayStyle text -font {Helvetica 12 bold} -anchor w -padx 5]
  set hcenter [tixDisplayStyle text -font {Helvetica 12 bold} -anchor c]

  $list header create 0 -itemtype text \
          -text "Test Suite Database" -style $hleft
  $list header create 1 -itemtype text -text "Status" -style $hcenter
  $list header create 2 -itemtype text -text "Items" -style $hcenter
  $list header create 3 -itemtype text -text "Parses" -style $hcenter
  $list header create 4 -itemtype text -text "Chart" -style $hcenter

  pack .list -in .body -side bottom -padx 0.1c -pady 0.1c -expand yes -fill y;

  #
  # set up (minimal) set of default bindings
  #
  bind all <Escape> { 
    foreach window [busy isbusy] {
      busy release $window 
    }; # foreach
  }
  bind all <Tab> {};

  #
  # read in balloon help bindings and message
  #
  balloon_setup $globals(balloons);

  #
  # expose podium; wait for display update
  #
  wm deiconify .
  tkwait visibility .
  tsdb_update complete;

}; # main()

proc evaluate {script {quiet 0}} {

    global globals;
    global test_suites skeletons phenomena;

    set status [catch {eval $script} return];
    
    if {$status == 0} {
        if {!$quiet} {
          puts [format "(:ok %s)" $return];
        }
    } else {
      set return [lispify_string $return];
      if {$status == 1} {
        puts [format "(:error \"%s\")" $return];
      } elseif {$status == 2} {
        puts [format "(:return \"%s\")" $return];
      } elseif {$status == 3} {
        puts [format "(:break)" $return];
      } elseif {$status == 4} {
        puts [format "(:continue)" $return];
      } else {
        puts [format "(:user \"%s\")" $return];
      }; # else
    }; # else
    
    logger [format "(%d \"%s\")" $status $return];

}; # evaluate()

#
# set status display with reset timer (`duration'); prevent superseded timers
# from overwriting the current status.  reset_status() is solely motivated by
# frustration with quoting and delay of evaluation; there should be a better
# way to achieve the same result but i seem unable to think of it tonight :-{.
#                                                         (19-apr-98  -  oe)
#
proc status {string {duration 0}} {

  global globals;

  set string " $string";
  set globals(status) $string;
  raise .status.label

  if {$duration} {
    after [expr int($duration * 1000)] \
          [list reset_status $string];
  }
  update idletasks; 

}; # status()

proc reset_status {string} {

  global globals;

  if {$globals(status) == $string} {
    set globals(status) "";
  }
  update idletasks; 

};# reset_status()

proc meter {value {text ""}} {

  .status.meter config -value $value -text $text;
  update idletasks; 

}; # meter()

proc meter_advance {increment} {

  set current [.status.meter cget -value];
  meter [expr $current + $increment];

}; # meter_advance()

proc run_meter {duration {initial 0.0} {background 0}} {

  set increment 0.05
  set steps [expr int((1.0 - $initial) / $increment)];

  if {$steps} {
    set delay [expr int($duration / $steps)];
    if {$background} {
      set value [expr $initial + $increment];
      meter $value;
      after $delay [list run_meter [expr $duration - $delay] \
                                   [expr $initial + $increment] 1];
    } else {
      for {set i 1} {$i <= $steps} {incr i} {
      set value [expr $initial + ($i * $increment)];
      meter $value;
      after $delay;
      }; # for
    }; # else
  }; # if

}; # run_meter()

proc gensym {} {

  global globals;

  incr globals(gensym);

}; # gensym()


proc input {prompt {default ""} {base ""}} {

  global globals;

  busy release 
  set label [.status.entry subwidget label];
  set entry [.status.entry subwidget entry];
  set prompt " $prompt";

  $label config -text $prompt;
  $entry delete 0 end
  set default [string_strip $base $default];
  $entry insert 0 $default;
  $entry icursor end;
  $entry xview end;

  bind $entry <Return> { 
    set globals(errno) 0;
    set globals(input) [.status.entry.frame.entry get];
  }; # bind
  bind $entry <Tab> [list input_completion $entry $base]
  bind $entry {<Escape> <BackSpace>} {
    set cursor [[.status.entry subwidget entry] index insert];
    set string [[.status.entry subwidget entry] get];
    set string [string range $string 0 [expr $cursor - 1]];
    regexp -nocase -indices { *[a-z0-9_-]*[^a-z0-9_-]? *$} $string indices
    set start [lindex $indices 0];
    if {$start == $cursor && $start > 0} {incr start -1}
    [.status.entry subwidget entry] delete $start $cursor;
    [.status.entry subwidget entry] icursor $start;
    break;
  }; # bind
  bind $entry <Alt-BackSpace> [bind $entry {<Escape> <BackSpace>}]
  bind $entry <Meta-BackSpace> [bind $entry {<Escape> <BackSpace>}]
  bind $entry <Control-g> {
    set globals(errno) 1;
  }; # bind
  bind $entry <Control-G> [bind $entry <Control-g>];

  set focus [focus -displayof .]
  focus $entry
  grab set $entry

  raise .status.entry;
  tkwait variable globals(errno);

  grab release $entry
  focus $focus
  raise .status.label

  if {!$globals(errno)} {
    set globals(input) "$base$globals(input)"
  }; # if
  return $globals(errno)

}; # input()


proc input_completion {entry {base ""}} {

  set prefix "$base[$entry get]";
  set completion [complete $prefix];
  if {$completion != "" } {
    set completion [string_strip $base $completion];
    $entry delete 0 end;
    $entry insert 0 $completion;
    $entry selection clear;
    $entry icursor end;
  }; # if
  
}; # input_completion()


proc complete {prefix {completions ""}} {

  global globals;

  set file 0;
  if {$completions == ""} {
    set file 1;
    set completions [lsort [glob -nocomplain -- "$prefix*"]]
  }; # if

  if {![llength $completions]} {
    tsdb_beep;
    return ""
  }; # if
  set completion [lindex $completions 0];
  if {[llength $completions] > 1} {
    set kaerb 0;
    for {set i [expr [string length $prefix] - 1]} {!$kaerb} {incr i} {
      set prefix [string range $completion 0 $i];
      foreach item $completions {
        if {[string compare $prefix [string range $item 0 $i]]} {
          set completion [string range $prefix 0 [expr $i - 1]];
          set kaerb 1;
        }; # if
      }; # foreach
    }; # for
  }; # if

  if {$file && [file isdirectory $completion]
      && $completion != $globals(slash)} {
    set completion "[file dirname [file join $completion .]]$globals(slash)"
  }; # if

  return $completion

}; # complete()


proc yes-or-no-p {prompt} {

  global globals;

  set timeout 1.5;

  if {![input "$prompt (yes or no)?"]} {
    switch -- $globals(input) {
      yes {return 1}
      no {return 0}
      default {
        tsdb_beep;
        status "please answer `yes' or `no'" $timeout;
        after [expr int($timeout * 1000)];
        return [yes-or-no-p $prompt];
      }
    }; switch
  }; # if

  return -1;

}; # yes-or-no-p()


main
