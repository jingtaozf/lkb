#
# [incr tsdb()] --- Competence and Performance Profiling Environment
# Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
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