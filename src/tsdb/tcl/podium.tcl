#!/bin/sh
# the next line restarts using wish(1) \
exec /coli/apps/tcl+tk/bin/wish++ "$0" "$@"

#
# facilitate stand-alone debugging for `oe'
#
if {![info exists itsdb_root]} {
  set itsdb_root [expr {[info exists env(HOSTNAME)] 
                        && ![string first "cp" $env(HOSTNAME)]
                        ? "/home/oe/src/itsdb" 
                        : "/home/oe/src/lkb"}];
}; # if
#
# import BLT library (for `table' and `graph' widgets)
#
namespace import -force blt::*
namespace import -force blt::tile::*

#
# all state is encoded in a global (associative) array `globals'
#
set globals(name) {[incr tsdb()]};
if {![info exists globals(podium_home)]} {
  set globals(podium_home) "$itsdb_root/src/tsdb/tcl/";
}; # if
if {![info exists globals(home)]} {
  set globals(home) "$itsdb_root/src/tsdb/small/";
}; # if
if {![info exists globals(data)]} {
  set globals(data) "";
}; # if
if {![info exists globals(balloons)]} {
  set globals(balloons) "$globals(podium_home)balloons";
}; # if
set globals(default_skeleton) "english"
if {![info exists globals(version)]} {
  set globals(version) "unknown";
}; # if
if {![info exists globals(application)]} {
  set globals(application) "unknown";
}; # if

if {![info exists globals(aggregate_dimension)]} {
  set globals(aggregate_dimension) :phenomena;
}; # if
if {![info exists globals(aggregate_size)]} {
  set globals(aggregate_size) 2;
}; # if
if {![info exists globals(aggregate_threshold)]} {
  set globals(aggregate_threshold) 1;
}; # if
if {![info exists globals(aggregate_lower)]} {
  set globals(aggregate_lower) 0;
}; # if
if {![info exists globals(aggregate_upper)]} {
  set globals(aggregate_upper) "";
}; # if
if {![info exists globals(graph_size)]} {
  set globals(graph_size) 1;
}; # if
if {![info exists globals(graph_threshold)]} {
  set globals(graph_threshold) 1;
}; # if
if {![info exists globals(graph_lower)]} {
  set globals(graph_lower) 0;
}; # if
if {![info exists globals(graph_upper)]} {
  set globals(graph_upper) "";
}; # if

set globals(menu_font) {Helvetica 14 bold};

set globals(graph,by) :i-length;
set globals(graph,scatterp) 1;
set globals(graph,extras) 0;

set globals(rules,actives) 1;
set globals(rules,passives) 1;

set globals(status) "";
set globals(balloon_font) {Helvetica 10};
set globals(status_font) {Helvetica 10};
set globals(input_font) {Courier 10};
set globals(copyright_font) {Helvetica 8};
set globals(user) "";
if {[info commands oe] == "oe"} {
  set globals(user) [oe user];
}; # if
if {$globals(user) == ""} {
  set globals(user) \
    [expr {[info exists env(USER)] ? $env(USER) : "anonymous"}];
}; # if

set globals(balloon_p) [expr {$globals(user) != "oe"}];
set globals(balloon) "";
set globals(interrupt) "";
set globals(landscape) 0;
set globals(abort) 0;
set globals(idle_colours) {yellow orange red violet blue green};
set globals(special_menues) {
  .menu.analyze.menu.values
  .menu.analyze.menu.division
  .menu.analyze.menu.phenomena
  .menu.analyze.menu.rvalues
  .menu.compare.menu.decoration
  .menu.compare.menu.intersection
  .menu.compare.menu.options
  .menu.evolution.menu.values
  .menu.options.menu.condition
  .menu.options.menu.phenomena
  .menu.options.menu.switches
}; # globals(special_menues)
set globals(division) "";
set globals(division,null) 1;
set globals(division,size) 0;
set globals(phenomena,division,all) 1;
set globals(condition) "";
set globals(condition,null) 1;
set globals(condition,size) 0;
set globals(phenomena,condition,all) 1;
set globals(overwrite) 1;
set globals(autoload_vocabulary) 1;
set globals(logscale) 0;
set globals(selection) -1;
set globals(input) "";
set globals(errno) 0;
set globals(gensym) 0;
set globals(slash) "/";
set globals(saarbruecken) "mail.coli.uni-sb.de";
set globals(busy_cursor) "watch";
set globals(gc_cursor) "pirate";
set globals(kanji_p) [expr {[info commands kanji] == "kanji"}];
set globals(critical_cell_threshold) 2000;

#
# relation and attributes for current database; reset when selection changes
# and filled on-demand (completion request) from back-end process.
#
set globals(relations) {};
set globals(attributes) {};

set globals(evolution,all) {
  coverage overgeneration words readings
  types rules lexicon
  first total tcpu tgc p-ftasks p-etasks p-stasks aedges pedges raedges rpedges
}; # globals(evolution,all)
set globals(evolution,coverage) 1;

#
# determine defaults for (display) options local to the podium(1) universe
#
set compare_in_detail(source) "";
set compare_in_detail(show,i-input) 1;
set compare_in_detail(compare,all) {
  words readings first total aedges pedges rpedges gcs error 
  derivation mrs tree
}; # compare_in_detail(compare,all)
foreach attribute $compare_in_detail(compare,all) {
  set compare_in_detail(compare,$attribute) 0;
}; # foreach
set compare_in_detail(compare,readings) 1;

#
# load table display modules by oliver `plaehn@coli.uni-sb.de' 
#
source "$globals(podium_home)goodies.tcl";
source "$globals(podium_home)table.tcl";
source "$globals(podium_home)showtable.tcl";
namespace eval test {source "$globals(podium_home)nshowtable.tcl"};
source "$globals(podium_home)showgraph.tcl";
source "$globals(podium_home)utilities.tcl";
source "$globals(podium_home)commands.tcl";
source "$globals(podium_home)input.tcl";
source "$globals(podium_home)balloon.tcl";
source "$globals(podium_home)copyleft.tcl";
if {[info exists globals(yy_p)] && $globals(yy_p)
    || $globals(user) == "oe"} {
  if {[file exists "$globals(podium_home)yy.tcl"]} {
    source "$globals(podium_home)yy.tcl";
  }; # if
}; #if
#
# log activity to trace file (for debugging)
#
if {$globals(user) == ""} {
  set globals(user) "unknown";
}; # if
catch {set trace [open "/tmp/podium.debug.$globals(user)" "w"]};


proc logger {string} {

  global trace;

  if {[info exists trace] && $string != ""} {
    puts $trace $string;
    flush $trace;
  }; # if

}; # logger()

#
# establish vanilla handler for background errors; the gc cursor handling
# seems to cause trouble sometimes that, we hope, can be ignored ...
#                                                     (25-jun-99  -  oe)
#
proc bgerror {message} {
  catch {logger [format "(error: %s)" $message]};
}; # bgerror()


proc main {} {

  global globals compare_in_detail;

  #
  # withdraw podium application until it is fully decorated
  #
  wm withdraw .

  #
  # read `.podiumrc' from user home directory if available
  #
  if {[file exists ~/.podiumrc]} {
    catch {source ~/.podiumrc};
  }; # if

  #
  # decorate top-level container and create menu bar
  # 
  label .balloon \
        -relief flat -bd 4 -font $globals(balloon_font) -bg yellow \
        -anchor c -textvariable globals(balloon);

  frame .menu -relief raised -bd 2
  pack .menu -side top -fill x

  menubutton .menu.file \
             -text "File" -font $globals(menu_font) \
             -underline 0 -menu .menu.file.menu 
  menubutton .menu.browse \
             -text "Browse" -font $globals(menu_font) \
             -underline 0 -menu .menu.browse.menu
  menubutton .menu.process \
             -text "Process" -font $globals(menu_font) \
             -underline 0 -menu .menu.process.menu
  menubutton .menu.analyze \
             -text "Analyze" -font $globals(menu_font) \
             -underline 0 -menu .menu.analyze.menu
  menubutton .menu.compare \
             -text "Compare" -font $globals(menu_font) \
             -underline 0 -menu .menu.compare.menu
  menubutton .menu.evolution \
             -text "Evolution" -font $globals(menu_font) \
             -underline 0 -menu .menu.evolution.menu
  menubutton .menu.trees \
             -text "Trees" -font $globals(menu_font) \
             -underline 0 -menu .menu.trees.menu
  menubutton .menu.options \
             -text "Options" -font $globals(menu_font) \
             -underline 0 -menu .menu.options.menu
  menubutton .menu.help \
             -text "?" -font $globals(menu_font) \
             -underline 0 -menu .menu.help.menu
  pack .menu.file .menu.browse .menu.process \
       .menu.analyze .menu.compare .menu.evolution \
       .menu.trees .menu.options -side left
  pack .menu.help -side right

  #
  # `File' menu (and embedded cascades)
  #
  menu .menu.file.menu -tearoff 0
  .menu.file.menu add cascade -label "Selection" \
    -menu .menu.file.menu.selection;
  .menu.file.menu add separator
  .menu.file.menu add command -label "Copy" \
    -command {tsdb_file copy} -state disabled
  .menu.file.menu add command -label "Rename" \
    -command {tsdb_file rename}
  .menu.file.menu add command -label "Reread" \
    -command {tsdb_file reread}
  .menu.file.menu add command -label "Swap" \
    -command {tsdb_file swap} -state disabled
  .menu.file.menu add command -label "Compress" \
    -command {tsdb_file compress} -state disabled
  .menu.file.menu add command -label "Purge" \
    -command {tsdb_file purge}
  .menu.file.menu add command -label "Delete" \
    -command {tsdb_file delete}
  .menu.file.menu add separator
  .menu.file.menu add cascade -label "Create" \
    -menu .menu.file.menu.create
  .menu.file.menu add separator
  .menu.file.menu add cascade -label "Import" \
    -menu .menu.file.menu.import
  .menu.file.menu add command -label "Export" \
    -command {tsdb_file export} -state disabled
  .menu.file.menu add command -label "Tenure" \
    -command {tsdb_file tenure} -state disabled
  .menu.file.menu add separator
  .menu.file.menu add command -label "Close" -command tsdb_close 
  .menu.file.menu add command -label "Quit" -command tsdb_quit

  #
  # `File -- Selection' cascade
  #
  menu .menu.file.menu.selection -tearoff 0
  .menu.file.menu.selection add command -label "Select Pattern" \
    -command {list_select "" pattern};
  .menu.file.menu.selection add command -label "Select All" \
    -command {list_select "" all};
  .menu.file.menu.selection add command -label "Unselect All" \
    -command {list_select "" none};
  .menu.file.menu.selection add separator;
  .menu.file.menu.selection add command -label "Zoom In" \
    -command {list_select "" in};
  .menu.file.menu.selection add command -label "Zoom Out" \
    -command {list_select "" out};
  
  #
  # the `File.Create' (cascaded) menu will be filled in dynamically
  #
  menu .menu.file.menu.create -tearoff 0

  #
  # `File -- Import' cascade
  #
  menu .menu.file.menu.import -tearoff 0
  .menu.file.menu.import add command -label "Test Items" \
    -command {tsdb_import items};
  .menu.file.menu.import add command -label "tsdb(1) Database" \
    -command {tsdb_import database};
  .menu.file.menu.import add separator
  .menu.file.menu.import add command -label "Skeleton Data" \
    -state disabled;

  #
  # `Browse' menu (and embedded cascades)
  #
  menu .menu.browse.menu -tearoff 0
  .menu.browse.menu add command -label "Database Schema" \
      -command tsdb_browse_relations
  .menu.browse.menu add command -label "Vocabulary" \
      -command tsdb_browse_vocabulary
  .menu.browse.menu add separator
  .menu.browse.menu add command \
      -label "Test Items" -command {tsdb_browse items ""}
  .menu.browse.menu add command \
      -label "Phenomena" -command {tsdb_browse phenomena ""}
  .menu.browse.menu add command \
      -label "Test Run(s)" -command {tsdb_browse runs ""}
  .menu.browse.menu add command \
      -label "Parses" -command {tsdb_browse parses ""}
  .menu.browse.menu add command \
      -label "Results" -command {tsdb_browse results ""}
  .menu.browse.menu add command \
    -label "Errors" -command {tsdb_browse errors ""}
  .menu.browse.menu add separator
  .menu.browse.menu add command -label "Custom Query" -command "tsdb_select"

  #
  # `Process' menu (and embedded cascades)
  #
  menu .menu.process.menu -tearoff 0
  .menu.process.menu add command -label "All Items" \
      -command {tsdb_process all}
  .menu.process.menu add command -label "Positive Items" \
      -command {tsdb_process positive}
  .menu.process.menu add command -label "Negative Items" \
      -command {tsdb_process negative}
  .menu.process.menu add command -label "TSQL Condition" \
      -command {tsdb_process condition}
  .menu.process.menu add separator
  .menu.process.menu add command -label "Vocabulary" \
      -command {tsdb_browse_vocabulary 1}
  .menu.process.menu add separator;
  .menu.process.menu add command -label "Passive Mode" -command {tsdb_capture};

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
  .menu.analyze.menu add command \
          -label "Performance" -command {analyze_performance}
  .menu.analyze.menu add command \
          -label "Processor" -command {analyze_performance parser}
  .menu.analyze.menu add separator
  .menu.analyze.menu add cascade \
          -label "Divison" \
          -menu .menu.analyze.menu.division;
  .menu.analyze.menu add cascade \
          -label "Phenomena" \
          -menu .menu.analyze.menu.phenomena;
  .menu.analyze.menu add command \
          -label "New Division" \
          -command "condition_input where \"\" division";
  .menu.analyze.menu add separator
  .menu.analyze.menu add command \
          -label "Show Graph" \
          -command tsdb_graph;
  .menu.analyze.menu add command \
          -label "Show Chart" \
          -command {tsdb_graph chart};
  .menu.analyze.menu add cascade \
          -label "Graph By" \
          -menu .menu.analyze.menu.by;
  .menu.analyze.menu add cascade \
          -label "Graph Values" \
          -menu .menu.analyze.menu.values;
  .menu.analyze.menu add command \
          -label "Graph Parameters" \
          -command graph_parameter_input
  .menu.analyze.menu add separator
  .menu.analyze.menu add command \
          -label "Rule Table" \
          -command {analyze_rules table};
  .menu.analyze.menu add command \
          -label "Rule Graph" \
          -command {analyze_rules graph};
  .menu.analyze.menu add cascade \
          -label "Rule Values" \
          -menu .menu.analyze.menu.rvalues;

  menu .menu.analyze.menu.by -tearoff 0
  .menu.analyze.menu.by add radiobutton \
    -label "String Length (`i-length')" \
    -variable globals(graph,by) -value :i-length;
  .menu.analyze.menu.by add radiobutton \
    -label "Lexical Entries (`words')" \
    -variable globals(graph,by) -value :words;
  .menu.analyze.menu.by add radiobutton \
    -label "Parser Analyses (`readings')" \
    -variable globals(graph,by) -value :readings;
  .menu.analyze.menu.by add radiobutton \
    -label "Executed Tasks (`p-etasks')" \
    -variable globals(graph,by) -value :p-etasks;
  .menu.analyze.menu.by add radiobutton \
    -label "Successful Tasks (`p-stasks')" \
    -variable globals(graph,by) -value :p-stasks;
  .menu.analyze.menu.by add radiobutton \
    -label "Active Edges (`aedges')" \
    -variable globals(graph,by) -value :aedges;
  .menu.analyze.menu.by add radiobutton \
    -label "Passive Edges (`pedges')" \
    -variable globals(graph,by) -value :pedges;
  .menu.analyze.menu.by add radiobutton \
    -label "Item Identifier (`i-id')" \
    -variable globals(graph,by) -value :i-id;

  menu .menu.analyze.menu.values -tearoff 0
  .menu.analyze.menu.values add radiobutton \
    -label "Parser Tasks" \
    -variable globals(graph,values) -value tasks \
    -command {update_graph_cascade tasks};
  .menu.analyze.menu.values add radiobutton \
    -label "Parsing Times" \
    -variable globals(graph,values) -value ptimes \
    -command {update_graph_cascade ptimes};
  .menu.analyze.menu.values add radiobutton \
    -label "Total Times" \
    -variable globals(graph,values) -value ttimes \
    -command {update_graph_cascade ttimes};
  .menu.analyze.menu.values add separator
  .menu.analyze.menu.values add checkbutton \
    -label "Time For First Reading (`first')" \
    -variable globals(graph,first) \
    -command {update_graph_cascade first};
  .menu.analyze.menu.values add checkbutton \
    -label "Time For All Readings (`total')" \
    -variable globals(graph,total) \
    -command {update_graph_cascade total};
  .menu.analyze.menu.values add checkbutton \
    -label "Overall Processing Time (`tcpu')" \
    -variable globals(graph,tcpu) \
    -command {update_graph_cascade tcpu};
  .menu.analyze.menu.values add checkbutton \
    -label "Garbage Collection Time (`tgc')" \
    -variable globals(graph,tgc) \
    -command {update_graph_cascade tgc};
  .menu.analyze.menu.values add checkbutton \
    -label "Parser Analyses (`readings')" \
    -variable globals(graph,readings) \
    -command {update_graph_cascade readings};
  .menu.analyze.menu.values add checkbutton \
    -label "Filtered Tasks (`p-ftasks')" \
    -variable globals(graph,p-ftasks) \
    -command {update_graph_cascade p-ftasks};
  .menu.analyze.menu.values add checkbutton \
    -label "Executed Tasks (`p-etasks')" \
    -variable globals(graph,p-etasks) \
    -command {update_graph_cascade p-etasks};
  .menu.analyze.menu.values add checkbutton \
    -label "Successful Tasks (`p-stasks')" \
    -variable globals(graph,p-stasks) \
    -command {update_graph_cascade p-stasks};
  .menu.analyze.menu.values add checkbutton \
    -label "Active Chart Items (`aedges')" \
    -variable globals(graph,aedges) \
    -command {update_graph_cascade aedges};
  .menu.analyze.menu.values add checkbutton \
    -label "Passive Chart Items (`pedges')" \
    -variable globals(graph,pedges) \
    -command {update_graph_cascade pedges};
  .menu.analyze.menu.values add checkbutton \
    -label "Active Result Items (`raedges')" \
    -variable globals(graph,raedges) \
    -command {update_graph_cascade raedges};
  .menu.analyze.menu.values add checkbutton \
    -label "Passive Result Items (`rpedges')" \
    -variable globals(graph,rpedges) \
    -command {update_graph_cascade rpedges};
  if {$globals(user) == "oe"} {
    .menu.analyze.menu.values add checkbutton \
      -label "Packed Parse Trees (`trees')" \
      -variable globals(graph,trees) \
      -command {update_graph_cascade trees};
    .menu.analyze.menu.values add checkbutton \
      -label "Unpacking Time (`utcpu')" \
      -variable globals(graph,utcpu) \
      -command {update_graph_cascade utcpu};
    .menu.analyze.menu.values add checkbutton \
      -label "Unpacking Space (`uspace')" \
      -variable globals(graph,uspace) \
      -command {update_graph_cascade utcpu};
    .menu.analyze.menu.values add checkbutton \
      -label "Subsumption Tests (`subsumptions')" \
      -variable globals(graph,subsumptions) \
      -command {update_graph_cascade subsumptions};
    .menu.analyze.menu.values add checkbutton \
      -label "Equivalence Packings (`equivalence')" \
      -variable globals(graph,equivalence) \
      -command {update_graph_cascade equivalence};
    .menu.analyze.menu.values add checkbutton \
      -label "Proactive Packings (`proactive')" \
      -variable globals(graph,proactive) \
      -command {update_graph_cascade proactive};
    .menu.analyze.menu.values add checkbutton \
      -label "Retroactive Packings (`retroactive')" \
      -variable globals(graph,retroactive) \
      -command {update_graph_cascade retroactive};
    .menu.analyze.menu.values add checkbutton \
      -label "Frozen Edges (`frozen')" \
      -variable globals(graph,frozen) \
      -command {update_graph_cascade frozen};
    .menu.analyze.menu.values add checkbutton \
      -label "Unpacking Failures (`failures')" \
      -variable globals(graph,failures) \
      -command {update_graph_cascade failures};
  }; # if

  menu .menu.analyze.menu.division -tearoff 0
  menu .menu.analyze.menu.phenomena -tearoff 0

  menu .menu.analyze.menu.rvalues -tearoff 0
  .menu.analyze.menu.rvalues add checkbutton \
    -label "Active Chart Items (`actives')" \
    -variable globals(rules,actives);
  .menu.analyze.menu.rvalues add checkbutton \
    -label "Passive Chart Items (`passives')" \
    -variable globals(rules,passives);
  .menu.analyze.menu.rvalues add checkbutton \
    -label "Successful Tasks (`successes')" \
    -variable globals(rules,successes);
  .menu.analyze.menu.rvalues add checkbutton \
    -label "Executed Tasks (`executed')" \
    -variable globals(rules,executed);
  .menu.analyze.menu.rvalues add checkbutton \
    -label "Filtered Tasks (`filtered')" \
    -variable globals(rules,filtered);

  #
  # `Compare' menu (and embedded cascades)
  #
  menu .menu.compare.menu -tearoff 0
  .menu.compare.menu add command -label "Competence" \
    -command {tsdb_compare competence};
  .menu.compare.menu add command -label "Performance" \
    -command {tsdb_compare performance};
  .menu.compare.menu add separator
  .menu.compare.menu add command -label "Detail" \
    -command tsdb_compare_in_detail
  .menu.compare.menu add separator
  .menu.compare.menu add cascade -label "Decoration" \
    -menu .menu.compare.menu.decoration
  .menu.compare.menu add cascade -label "Intersection" \
    -menu .menu.compare.menu.intersection
  .menu.compare.menu add separator
  .menu.compare.menu add cascade -label "Source Database" \
    -menu .menu.compare.menu.compare
  .menu.compare.menu add cascade -label "Switches" \
    -menu .menu.compare.menu.switches

  menu .menu.compare.menu.decoration -tearoff 0
  .menu.compare.menu.decoration add checkbutton -label "i-wf" \
    -variable compare_in_detail(show,i-wf)
  .menu.compare.menu.decoration add checkbutton -label "i-input" \
    -variable compare_in_detail(show,i-input)
  .menu.compare.menu.decoration add checkbutton -label "i-category" \
    -variable compare_in_detail(show,i-category)

  menu .menu.compare.menu.intersection -tearoff 0
  foreach attribute $compare_in_detail(compare,all) {
    .menu.compare.menu.intersection add checkbutton -label $attribute \
      -variable compare_in_detail(compare,$attribute)
  }; # foreach

  menu .menu.compare.menu.compare  -tearoff 0

  menu .menu.compare.menu.switches -tearoff 0
  .menu.compare.menu.switches add checkbutton \
    -label "Sloppy Alignment" \
    -variable compare_in_detail(options,sloppy_alignment) \
    -command {tsdb_set detail_sloppy_alignment_p};

  #
  # `Evolution' menu (and embedded cascades)
  #
  menu .menu.evolution.menu -tearoff 0;
  .menu.evolution.menu add command -label "Tabulate" \
    -command {tsdb_evolution tabulate} -state disabled;
  .menu.evolution.menu add command -label "Graph" \
    -command {tsdb_evolution graph};
  .menu.evolution.menu add separator;
  .menu.evolution.menu add cascade -label "Value(s)" \
    -menu .menu.evolution.menu.values;

  menu .menu.evolution.menu.values -tearoff 0;
  .menu.evolution.menu.values add checkbutton \
    -label "Grammatical Coverage" \
    -variable globals(evolution,coverage);
  .menu.evolution.menu.values add checkbutton \
    -label "Grammatical Overgeneration" \
    -variable globals(evolution,overgeneration);
  .menu.evolution.menu.values add checkbutton \
    -label "Lexical Ambiguity (`words')" \
    -variable globals(evolution,words);
  .menu.evolution.menu.values add checkbutton \
    -label "Phrasal Ambiguity (`readings')" \
    -variable globals(evolution,readings);
  .menu.evolution.menu.values add separator;
  .menu.evolution.menu.values add checkbutton \
    -label "Grammar Types" \
    -variable globals(evolution,types);
  .menu.evolution.menu.values add checkbutton \
    -label "Grammar Rules" \
    -variable globals(evolution,rules);
  .menu.evolution.menu.values add checkbutton \
    -label "Lexical Entries" \
    -variable globals(evolution,lexicon);
  .menu.evolution.menu.values add separator;
  .menu.evolution.menu.values add checkbutton \
    -label "Time For First Reading (`first')" \
    -variable globals(evolution,first);
  .menu.evolution.menu.values add checkbutton \
    -label "Time For All Readings (`total')" \
    -variable globals(evolution,total);
  .menu.evolution.menu.values add checkbutton \
    -label "Overall Processing Time (`tcpu')" \
    -variable globals(evolution,tcpu);
  .menu.evolution.menu.values add checkbutton \
    -label "Garbage Collection Time (`tgc')" \
    -variable globals(evolution,tgc);
  .menu.evolution.menu.values add checkbutton \
    -label "Filtered Tasks (`p-ftasks')" \
    -variable globals(evolution,p-ftasks);
  .menu.evolution.menu.values add checkbutton \
    -label "Executed Tasks (`p-etasks')" \
    -variable globals(evolution,p-etasks);
  .menu.evolution.menu.values add checkbutton \
    -label "Successful Tasks (`p-stasks')" \
    -variable globals(evolution,p-stasks);
  .menu.evolution.menu.values add checkbutton \
    -label "Active Chart Items (`aedges')" \
    -variable globals(evolution,aedges);
  .menu.evolution.menu.values add checkbutton \
    -label "Passive Chart Items (`pedges')" \
    -variable globals(evolution,pedges);
  .menu.evolution.menu.values add checkbutton \
    -label "Active Result Items (`raedges')" \
    -variable globals(evolution,raedges);
  .menu.evolution.menu.values add checkbutton \
    -label "Passive Result Items (`rpedges')" \
    -variable globals(evolution,rpedges);

  #
  # `Trees' menu (and embedded cascades)
  #
  menu .menu.trees.menu -tearoff 0;
  .menu.trees.menu add command \
     -label "Browse" -state disabled -command {tsdb_browse trees ""};
  .menu.trees.menu add command \
     -label "Annotate" -command {tsdb_browse trees ""};
  .menu.trees.menu add command \
    -label "Summarize" -command {analyze_trees};
  .menu.trees.menu add command \
    -label "Update" -command {tsdb_browse trees "" 1 "" 1};
  .menu.trees.menu add command \
    -label "Normalize" -command {tsdb_file strip};
  .menu.trees.menu add command \
    -label "Export" -state disabled -command {tsdb_file export trees};
  .menu.trees.menu add command \
    -label "Clear-Cut" -command {tsdb_file purge trees};

  #
  # `Options' menu (and embedded cascades)
  #
  menu .menu.options.menu -tearoff 0
  .menu.options.menu add command -label "Database Root" \
    -command "tsdb_option home"
  .menu.options.menu add command -label "Skeleton Root" \
    -command "tsdb_option skeleton_directory"
  .menu.options.menu add separator
  .menu.options.menu add cascade -label "Update" \
    -menu .menu.options.menu.update
  .menu.options.menu add separator
  .menu.options.menu add cascade \
    -label "TSQL Condition" \
    -menu .menu.options.menu.condition
  .menu.options.menu add cascade \
    -label "Phenomena" \
    -menu .menu.options.menu.phenomena
  .menu.options.menu add command -label "New Condition" \
    -command condition_input;
  .menu.options.menu add separator
  .menu.options.menu add cascade \
    -label "Aggregate By" \
    -menu .menu.options.menu.aggregate
  .menu.options.menu add command \
    -label "Aggregation Parameters" -command aggregate_input;
  .menu.options.menu add separator
  .menu.options.menu add cascade \
    -label "Switches" -menu .menu.options.menu.switches

  menu .menu.options.menu.update -tearoff 0
  .menu.options.menu.update add command -label "Skeleton List" \
    -command [list tsdb_update skeletons]
  .menu.options.menu.update add command -label "Database List" \
    -command [list tsdb_update all]
  .menu.options.menu.update add separator;
  .menu.options.menu.update add command -label "All tsdb(1) Status" \
    -command [list tsdb_update complete]

  menu .menu.options.menu.condition -tearoff 0

  menu .menu.options.menu.phenomena -tearoff 0


  menu .menu.options.menu.aggregate -tearoff 0
  .menu.options.menu.aggregate add radiobutton \
    -label "Phenomena" \
    -variable globals(aggregate_dimension) -value :phenomena \
    -command {tsdb_set aggregate_dimension};
  .menu.options.menu.aggregate add radiobutton \
    -label "Grammaticality (`i-wf')" \
    -variable globals(aggregate_dimension) -value :i-wf \
    -command {tsdb_set aggregate_dimension};
  .menu.options.menu.aggregate add radiobutton \
    -label "String Length (`i-length')" \
    -variable globals(aggregate_dimension) -value :i-length \
    -command {tsdb_set aggregate_dimension};
  .menu.options.menu.aggregate add radiobutton \
    -label "Lexical Entries (`words')" \
    -variable globals(aggregate_dimension) -value :words \
    -command {tsdb_set aggregate_dimension};
  .menu.options.menu.aggregate add radiobutton \
    -label "Parser Analyses (`readings')" \
    -variable globals(aggregate_dimension) -value :readings \
    -command {tsdb_set aggregate_dimension};
  .menu.options.menu.aggregate add radiobutton \
    -label "Executed Tasks (`p-etasks')" \
    -variable globals(aggregate_dimension) -value :p-etasks \
    -command {tsdb_set aggregate_dimension};
  .menu.options.menu.aggregate add radiobutton \
    -label "Filtered Tasks (`p-ftasks')" \
    -variable globals(aggregate_dimension) -value :p-ftasks \
    -command {tsdb_set aggregate_dimension};
  .menu.options.menu.aggregate add radiobutton \
    -label "Successful Tasks (`p-stasks')" \
    -variable globals(aggregate_dimension) -value :p-stasks \
    -command {tsdb_set aggregate_dimension};
  .menu.options.menu.aggregate add radiobutton \
     -label "Active Edges (`aedges')" \
    -variable globals(aggregate_dimension) -value :aedges \
    -command {tsdb_set aggregate_dimension};
  .menu.options.menu.aggregate add radiobutton \
    -label "Passive Edges (`pedges')" \
    -variable globals(aggregate_dimension) -value :pedges \
    -command {tsdb_set aggregate_dimension};
  .menu.options.menu.aggregate add radiobutton \
    -label "Item Identifier (`i-id')" \
    -variable globals(aggregate_dimension) -value :i-id \
    -command {tsdb_set aggregate_dimension};
  .menu.options.menu.aggregate add radiobutton \
    -label "Total CPU Time (`tcpu')" \
    -variable globals(aggregate_dimension) -value :tcpu \
    -command {tsdb_set aggregate_dimension};
  .menu.options.menu.aggregate add radiobutton \
    -label "Total Memory Usage" \
    -variable globals(aggregate_dimension) -value :space \
    -command {tsdb_set aggregate_dimension};

  menu .menu.options.menu.switches -tearoff 0
  .menu.options.menu.switches add checkbutton \
    -label "Exhaustive Search" \
    -variable globals(exhaustive_p) -command {tsdb_set exhaustive_p};
  .menu.options.menu.switches add command \
    -label "Chart Size Limit" \
    -command {tsdb_option pedges};
  .menu.options.menu.switches add command \
    -label "Result Storage Limit" \
    -command {tsdb_option results};
  .menu.options.menu.switches add separator
  .menu.options.menu.switches add checkbutton \
    -label "Write `run' Relation" \
    -variable globals(write_run_p) -command {tsdb_set write_run_p};
  .menu.options.menu.switches add checkbutton \
    -label "Write `parse' Relation" \
    -variable globals(write_parse_p) -command {tsdb_set write_parse_p};
  .menu.options.menu.switches add checkbutton \
    -label "Write `result' Relation" \
    -variable globals(write_result_p) -command {tsdb_set write_result_p};
  .menu.options.menu.switches add checkbutton \
    -label "Write `output' Relation" \
    -variable globals(write_output_p) -command {tsdb_set write_output_p};
  .menu.options.menu.switches add checkbutton \
    -label "Write `rule' Relation" \
    -variable globals(write_rule_p) -command {tsdb_set write_rule_p};
  .menu.options.menu.switches add checkbutton \
    -label "Write Lexicon Chart" \
    -variable globals(write_lexicon_chart_p) \
    -command {tsdb_set write_lexicon_chart_p};
  .menu.options.menu.switches add checkbutton \
    -label "Write Syntax Chart" \
    -variable globals(write_syntax_chart_p) \
    -command {tsdb_set write_syntax_chart_p};
  .menu.options.menu.switches add separator
  .menu.options.menu.switches add radiobutton \
    -label "On Demand Garbage Collect" \
    -variable globals(gc_p) -value nil -command {tsdb_set gc_p}
  .menu.options.menu.switches add radiobutton \
    -label "Preliminary  Scavenge" \
    -variable globals(gc_p) -value :local -command {tsdb_set gc_p}
  .menu.options.menu.switches add radiobutton \
    -label "Preliminary Garbage Collect" \
    -variable globals(gc_p) -value :global -command {tsdb_set gc_p}
  .menu.options.menu.switches add checkbutton \
    -label "Enable Tenuring" \
    -variable globals(tenure_p) -command {tsdb_set tenure_p}
  .menu.options.menu.switches add separator
  .menu.options.menu.switches add checkbutton \
    -label "Overwrite Test Run" \
    -variable globals(overwrite);
  .menu.options.menu.switches add checkbutton \
    -label "Autoload Vocabulary" \
    -variable globals(autoload_vocabulary);
   .menu.options.menu.switches add separator
  .menu.options.menu.switches add checkbutton \
    -label "Exclude GC Time" \
    -variable globals(exclude_tgc_p) \
    -command {tsdb_set exclude_tgc_p}; 
  .menu.options.menu.switches add checkbutton \
    -label "Scatter Plots" \
    -variable globals(graph,scatterp);
  .menu.options.menu.switches add checkbutton \
    -label "Logarithmic Scales" \
    -variable globals(logscale);
  .menu.options.menu.switches add checkbutton \
    -label "Analogy Aggregation" \
    -variable globals(analogy_aggregation_p) \
    -command {tsdb_set analogy_aggregation_p}; 
  .menu.options.menu.switches add checkbutton \
    -label "Sloppy Alignment" \
    -variable compare_in_detail(options,sloppy_alignment) \
    -command {tsdb_set detail_sloppy_alignment_p};
  if {$globals(user) == "oe"} {
    .menu.options.menu.switches add checkbutton \
      -label "Custom Fields" \
      -variable globals(graph,extras);
  }; # if

  #
  # `Help' menu (and embedded cascades)
  #
  menu .menu.help.menu -tearoff 0
  .menu.help.menu add command \
    -label "[expr {$globals(balloon_p) ? "Disable" : "Enable"}] Balloon Help" \
    -command toggle_balloon_help;
  .menu.help.menu add separator;
  .menu.help.menu add command -label "Registration Information" \
    -command tsdb_registration;
  .menu.help.menu add command -label "$globals(name) ToDo List" \
    -command tsdb_todo;

  frame .body -width 19.1c -height 5c
  pack .body -side top -expand yes -fill y;

  #
  # the bottom line: podium status display 
  #
  frame .status -relief raised -bd 2;
  tixMeter .status.meter -value 1 -bd 1 -relief sunken;
  frame .status.meter.blind -relief sunken -bd 1;
  label .status.meter.blind.label -relief sunken -bd 0 -anchor c \
    -font [linsert $globals(status_font) end bold];

  label .status.dummy \
          -relief sunken -bd 3 -font $globals(status_font)
  label .status.label \
          -relief flat -bd 0 -font $globals(status_font) -padx 0 -pady 0 \
          -textvariable globals(status);

  tixLabelEntry .status.entry \
          -relief flat -bd 0 \
          -options [list label.font $globals(status_font) \
                         label.padx 0 label.pady 0 \
                         entry.font $globals(input_font) \
                         entry.relief flat entry.highlightThickness 0 ];
  [.status.entry subwidget label] config \
    -bg [[.status.entry subwidget entry] cget -bg];
  pack .status -side bottom -fill x -expand no;
  pack .status.dummy -side right -padx 1 -pady 1 -fill both -expand yes;
  pack .status.meter -side right -padx 2;
  place .status.meter.blind -in .status.meter -relwidth 1 -relheight 1;
  pack .status.meter.blind.label -fill both -expand 1;

  place .status.label -in .status.dummy -relwidth 1 -relheight 1
  place .status.entry -in .status.dummy -relwidth 1 -relheight 1
  lower .status.entry
  status "initializing $globals(name) podium ..." 2

  #
  # create entry pane for aggregation parameters
  #
  set grey lightgrey;
  frame .status.aggregate -bd 0 -bg $grey;
  frame .status.aggregate.fill0 -bd 0 -bg $grey;
  frame .status.aggregate.fill1 -bd 0 -bg $grey;
  frame .status.aggregate.fill2 -bd 0 -bg $grey;
  frame .status.aggregate.fill3 -bd 0 -bg $grey;
  frame .status.aggregate.fill4 -bd 0 -bg $grey;

  set loptions [list -font $globals(status_font) -padx 0 -pady 0 -bg $grey];
  set eoptions [list -justify right -font $globals(input_font) \
                     -bd 1 -relief solid -width 3 -highlightthickness 0 \
                     -bg white];
  eval label .status.aggregate.lsize -text "size\\ " $loptions;
  eval entry .status.aggregate.esize $eoptions;
  eval label .status.aggregate.lthreshold -text "threshold\\ " $loptions;
  eval entry .status.aggregate.ethreshold $eoptions;
  eval label .status.aggregate.llower -text "lower\\ " $loptions;
  eval entry .status.aggregate.elower $eoptions;
  eval label .status.aggregate.lupper -text "upper\\ " $loptions;
  eval entry .status.aggregate.eupper  $eoptions;

  place .status.aggregate -in .status.dummy -relwidth 1 -relheight 1;
  pack .status.aggregate.fill0 -side left -fill both -expand yes;
  pack .status.aggregate.lsize -side left;
  pack .status.aggregate.esize -side left;
  pack .status.aggregate.fill1 -side left -fill both -expand yes;
  pack .status.aggregate.lthreshold -side left;
  pack .status.aggregate.ethreshold -side left;
  pack .status.aggregate.fill2 -side left -fill both -expand yes;
  pack .status.aggregate.llower -side left;
  pack .status.aggregate.elower -side left;
  pack .status.aggregate.fill3 -side left -fill both -expand yes;
  pack .status.aggregate.lupper -side left;
  pack .status.aggregate.eupper -side left;
  pack .status.aggregate.fill4 -side left -fill both -expand yes;
  lower .status.aggregate;

  #
  # entry pane for one or two integers, side by side
  #
  frame .status.integer -bd 0 -bg $grey;
  frame .status.integer.fill0 -bd 0 -bg $grey;
  frame .status.integer.fill1 -bd 0 -bg $grey;
  frame .status.integer.fill2 -bd 0 -bg $grey;

  set loptions [list -font $globals(status_font) -padx 0 -pady 0 -bg $grey];
  set eoptions [list -justify right -font $globals(input_font) \
                     -bd 1 -relief solid -width 8 -highlightthickness 0 \
                     -bg white];
  eval label .status.integer.lprompt -text "lprompt" $loptions;
  eval entry .status.integer.lvalue $eoptions;
  eval label .status.integer.rprompt -text "rprompt" $loptions;
  eval entry .status.integer.rvalue $eoptions;

  place .status.integer -in .status.dummy -relwidth 1 -relheight 1;
  lower .status.integer;

  #
  # body of tsdb(1) podium: a scrolled multi-column listbox
  #
  tixScrolledHList .list -width 19.4c -scrollbar "auto +y" \
          -options { hlist.columns 6 hlist.height 8 \
                     hlist.header true hlist.itemtype text
                     hlist.background white };
  set list [.list subwidget hlist];
  $list config -selectmode single -selectbackground cyan \
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

  bind $list <Control-a> {list_select all};
  bind $list <Enter> {
    balloon post "<Button-1> selects active test suite; \
                  <Button-2> selects (gold standard) comparison source"
  }; # bind
  bind $list <Leave> {balloon unpost};

  $list column width 0 0.4c
  $list column width 1 10c
  $list column width 2 2c
  $list column width 3 2c
  $list column width 4 2c
  $list column width 5 2c

  set hleft [tixDisplayStyle text -font {Helvetica 12 bold} -anchor w -padx 5]
  set hcenter [tixDisplayStyle text -font {Helvetica 12 bold} -anchor c]

  $list header create 1 -itemtype text \
          -text "Test Suite Instance" -style $hleft;
  $list header create 2 -itemtype text -text "Status" -style $hcenter;
  $list header create 3 -itemtype text -text "Items" -style $hcenter;
  $list header create 4 -itemtype text -text "Parses" -style $hcenter;
  $list header create 5 -itemtype text -text "Options" -style $hcenter;

  pack .list -in .body -side bottom -padx 0.1c -pady 0.1c -expand yes -fill y;
  focus $list;

  #
  # another scrolled multi-column listbox; pops up for completions on demand
  #
  tixScrolledHList .completions -width [winfo width .list] \
    -scrollbar "auto +y" \
    -options { hlist.columns 3 hlist.header true 
               hlist.itemtype text hlist.background yellow };
  set clist [.completions subwidget hlist];
  $clist header create 1 -itemtype text -text "Completions" -style $hcenter;
  place .completions -in .list -relwidth 1 -relheight 1;
  lower .completions;

  #
  # set up (minimal) set of default bindings
  #
  bind all <Escape> { 
    foreach window [busy isbusy] {
      busy release $window 
    }; # foreach
  }
  bind all <Tab> {};
  bind Menu <Button> {menu_button_down %W %b};
  bind Menu <ButtonRelease> {menu_button_up %W %b};

  #
  # read in balloon help bindings and message
  #
  balloon_setup $globals(balloons);

  #
  # decorate and expose podium; wait for display update
  #
  set foo [format {%s podium @ %s [version %s @ %s]} \
             $globals(name) [info hostname] \
             $globals(version) $globals(application)];
  wm title . $foo;
  wm iconname . {tsdb()}
  if {[file exists [set icon "$globals(podium_home)icon.xbm"]]} {
    wm iconbitmap . @$icon;
  }; # if
  wm deiconify .
  tkwait visibility .

  copyleft initialize;
  tsdb_update complete;
  set globals(graph,tcpu) 1;
  update_graph_cascade tcpu;

  if {[info procs yy_initialize] != ""} {
    yy_initialize;
  }; # if
  #
  # the .list geometry somehow only propagates when we run the event loop :-(.
  #
  update idletasks;
  set width [expr int([winfo width $list] / 3)];
  $clist column width 0 $width;
  $clist column width 1 [expr [winfo width $list] - $width * 2];
  $clist column width 2 $width;

  after 2000 idle;

}; # main()

proc evaluate {script {quiet 0}} {

  #
  # only variables imported here will be visible to the Lisp side of the 
  # [incr tsdb()] podium; all commands from Lisp are wrapped into evaluate()
  # calls.
  #
  global globals;
  global test_suites skeletons phenomena;

  logger [format "(evaluate: %s)" $script];
  set status [catch {eval $script} return];
    
  if {$status == 0 && !$quiet} {
    puts [format "(:ok %s)" $return];
  } elseif {!$quiet} {
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
    
  logger [expr {$return != "" \
                ? "(return: $status $return)" \
                : "(return: $status)"}];

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
  raise .status.label;

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

  if {$value == "hide"} {
    raise .status.meter.blind;
    return;
  }; # if
  lower .status.meter.blind;
  .status.meter config -value $value -text $text;
  update idletasks; 

}; # meter()

proc meter_advance {increment} {

  set current [.status.meter cget -value];
  meter [min [expr $current + $increment] 1];

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

proc idle {} {

  global globals;

  if {[llength $globals(idle_colours)]} {
    set time [clock seconds];
    set foo [expr {int($time / 10)}];
    set colour [lindex $globals(idle_colours) \
                       [expr {$foo % [llength $globals(idle_colours)]}]];
    set time [clock format $time -format "%d-%b-%y (%H:%M h)"];
    set time [string tolower [string trimleft $time "0"]];
    .status.meter.blind.label config -text $time -fg $colour -bg black;
    if {!$globals(abort)} {after 1000 idle};
  } else {
    .status.meter.blind.label config -text "" -bg [.status.dummy cget -bg];
  }; # else

}; # idle()


main

