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

proc balloon_setup {file} {

  global globals balloons;

  if {[catch {set input [open $file r]}] != 0} {
    tsdb_beep;
    status "unable to open `$file' |:-(" 10;
    return 1;
  }; # if

  while {![eof $input]} {
    set line [gets $input];
    if {$line == "" || [regexp {^ *#} $line]} {
      continue;
    }; # if

    set window [lindex $line 0]
    set text [string range $line [expr [string length $window] + 2] end];
    set text [string trim $text];
    
    if {[winfo exists $window]} {
      bind $window <Enter> [list balloon post $text];    
      bind $window <Leave> [list balloon unpost];
    } else {
      set menu [lindex $window 0];
      set entry [string range $window [expr [string length $menu] + 1] end];
      set entry [string trim $entry];
      set "balloons($menu,$entry)" $text;
    }; # else
  }; # while

  if {$globals(balloon_p)} {
    pack .balloon -before .menu -fill x;
    foreach i [array names balloons] {
      set menu [string range $i 0 [expr [string first "," $i] - 1]];
      bind $menu <<MenuSelect>> {balloon_menu %W}
    }; # foreach
  } else {
    pack forget .balloon;
  }; # else
}; # balloon_setup() 


proc balloon {action {text ""}} {

  global globals;

  if {$action == "post"} {
    set globals(balloon) $text;
  } elseif {$action == "unpost"} {
    set globals(balloon) "";
  } else {
    set globals(balloon) "";
  }; # else
}; # balloon()

proc balloon_menu {menu} {

  global balloons;

  set active [$menu index active];
  set label [$menu entrycget $active -label];
  if {[info exists balloons($menu,$label)]} {
    balloon post $balloons($menu,$label);
  } else {
    balloon unpost;
  }; # else
}; # balloon_menu()