#
# [incr tsdb()] --- Competence and Performance Profiling Environment
# Copyright (c) 1996 -- 2006 Stephan Oepen (oe@csli.stanford.edu)
# Copyright (c) 2007 -- 2013 Stephan Oepen (oe@ifi.uio.no)
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

set globals(copyleft,key) 42;

proc copyleft {action} {
 
  global globals;

  set bg black;
  set fg yellow;
  set large {Helvetica 18};
  set normalsize {Helvetica 12};
  set small {Helvetica 10};
  set message "contacting protocol server 'mail.coli.uni-sb.de' ...";
  set pointer "(See 'Help - Registration')";
  switch $action {
    initialize {
      if {[winfo exists .list.copyleft]} {
        destroy .list.copyleft
      }; # if
      frame .list.copyleft -bg $bg
      place .list.copyleft -in .list \
        -relwidth 1 -relheight 1 -bordermode outside;
      raise .list.copyleft;
      set copyleft [frame .list.copyleft.body -bg $bg];
      pack $copyleft -fill none -expand yes;

      label $copyleft.title -bg $bg -fg grey -font [concat $large {bold}] \
        -text "$globals(name)  -  version $globals(version)"
      frame $copyleft.zero -bg $bg -height 5
      label $copyleft.first -bg $bg -fg grey -font $small \
        -text "Copyleft (c) 1995 - 2013 \
               by Stephan Oepen (oe@ifi.uio.no) \
               - All Rights Reserved"
      frame $copyleft.second -bg $bg -height 8
      label $copyleft.third -bg $bg -fg green -font [concat $normalsize] \
        -text "$globals(name) is free software \
               under the GNU LGPL $pointer."
      if {[info exists globals(copyleft,key)]
          && [oe copyleft $globals(copyleft,key)]} {
        frame $copyleft.fourth -bg $bg -height 6
        label $copyleft.fifth -bg $bg -fg grey -font [concat $normalsize] \
          -text [format {-  Registered Copy [%s]  -} $globals(copyleft,key)];
      } else {
        frame $copyleft.fourth -bg $bg -height 5
        label $copyleft.fifth -bg $bg -fg grey -font [concat $normalsize] \
          -text "Until registered, after several minutes of continuous use, \
                 a log entry will be generated and\n\
                 automatically sent to a central protocol server \
                 (at Saarland University)."
      }; # else
      pack $copyleft.title \
        $copyleft.zero $copyleft.first $copyleft.second $copyleft.third \
        $copyleft.fourth $copyleft.fifth

      #
      # enforce that the copyleft notice will be displayed at least for several
      # seconds; the handler in update_skeleton_list() will block until the
      # system time has reached this value.
      #
      set globals(copyleft) [expr {[clock seconds] + 5}];
      set globals(copyleft,status) pending;
    } 
    hide { 
      if {[winfo exists .list.copyleft] && [info exists globals(copyleft)]} {
        while {[clock seconds] <= $globals(copyleft)} {
          after 500;
        }; # while
        place forget .list.copyleft;
      }; # if
    }
    register {
      if {![set busy [expr {[busy isbusy .] != ""}]]} {
        tsdb_busy freeze;
      }; # if
      winop map .;
      winop raise .;
      set status $globals(status);
      set visible [lindex [winfo children .status] end];
      #
      # aesthetics: strip duplicate reference to registration information
      #
      set availability [.list.copyleft.body.third cget -text];
      set strip [string last $pointer $availability];
      if {$strip > 0} {
        incr strip -2;
        set availability "[string range $availability 0 $strip].";
        .list.copyleft.body.third config -text $availability;
      }; # if
      .list.copyleft.body.fifth config -fg yellow \
        -text "Notification in Progress $pointer";
      place .list.copyleft -in .list \
        -relwidth 1 -relheight 1 -bordermode outside;
      raise .list.copyleft;
      status $message;
      update; update idletasks;
      #
      # wait for confirmation from the background registrar process; once the
      # registration is completed, an asynchronous event will change the oe()
      # registration status.
      #
      for {set timeout 20; set i 0} \
          {$i <= 4
           || $globals(copyleft,status) == "pending" && $i <= $timeout} \
          {incr i} {
        after 1000;
        if {$i % 2} {
          .list.copyleft.body.fifth config -fg black;
        } else {
          .list.copyleft.body.fifth config -fg yellow;
        }; # else
        update idletasks;
      }; # for
      if {$i <= $timeout} {
        .list.copyleft.body.fifth config -fg green \
          -text "- Notification Completed -";
        status "$message done";
        after 2000;
      } else {
        tsdb_beep;
        .list.copyleft.body.fifth config -fg black;
        status "$message timeout";
        after 2000;
      }; # else
      place forget .list.copyleft;
      set globals(status) $status;
      raise $visible;
      if {!$busy} {
        tsdb_busy release;
      }; # if
      oe reap;
      update idletasks;
    }
  }; # switch

}; # copyleft_register()