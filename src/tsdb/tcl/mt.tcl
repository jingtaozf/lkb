proc analyze_results {} {

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
    if {[verify_ts_selection]} {return 1};
    set selection "\"$globals(data)\"";
  }; # if

  set command [format "(summarize (%s))" $selection];
  send_to_lisp :event $command;

}; # analyze_results()