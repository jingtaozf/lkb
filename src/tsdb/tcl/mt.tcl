proc analyze_results {} {

  global globals;

  if {[verify_ts_selection]} {return 1};
  set command [format "(summarize \"%s\")" $globals(data)];
  send_to_lisp :event $command;

}; # analyze_results()