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

proc input {prompt {default ""} {base ""} {mode ""}} {

  global globals;

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
    set globals(input) [.status.entry.frame.entry get];
    set globals(errno) 0;
  }; # bind
  if {$mode == "select"} {
    bind $entry <Tab> [list select_completion $entry $mode]
    set file 0;
  } elseif {$mode == "from"} {
    bind $entry <Tab> [list select_completion $entry $mode]
    set file 0;
  } elseif {$mode == "where" || $mode == "condition" || $mode == "division" } {
    bind $entry <Tab> [list select_completion $entry $mode]
    set file 0;
  } else {
    bind $entry <Tab> [list file_completion $entry $base $mode]
    set file 1;
  }; # else
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

  #
  # until we sort out proper (mouse-driven) operation of the .completions
  # scrollbar (while grab()ing the input field) establish a cludge
  #
  bind $entry <ButtonPress> {
    if {[winfo containing %X %Y] == [.completions subwidget vsb]} {
      tsdb_beep;
      status [format "%%s %%s" \
              "<PageUp> or <PageDown> (and equivalents)" \
              "scroll the list of completions"];
      after 1500;
      status "";
      raise .status.entry;
      update idletasks;
    }; # if
  }; # bind
  bind $entry <Prior> {[.completions subwidget hlist] yview scroll -1 pages};
  bind $entry <Alt-v> [bind $entry {<Prior>}];
  bind $entry {<Escape> <v>} [bind $entry {<Prior>}];
  bind $entry <Next> {[.completions subwidget hlist] yview scroll 1 pages};
  bind $entry <Control-v> [bind $entry {<Next>}];

  #
  # enable history mechanism for appropriate input classes 
  #
  if {$mode != ""} {
    bind $entry <Up> [list entry_history $entry $mode 1];
    bind $entry <Down> [list entry_history $entry $mode -1];
  } else {
    bind $entry <Up> {};
    bind $entry <Down> {};
  }; # else

  set focus [focus -displayof .]
  focus $entry
  grab set $entry

  raise .status.entry;
  tkwait variable globals(errno);

  grab release $entry
  focus $focus
  raise .status.label

  if {!$globals(errno) && $file} {
    set globals(input) "$base$globals(input)"
  }; # if
  lower .completions;
  return $globals(errno)

}; # input()


proc condition_input {{prompt "where"} {default ""} {context "condition"}} {

  global globals;

  if {![set status [input $prompt $default "" $context]]} {
    if {$globals(input) == ""} {
      update_condition_cascade null $context;
    } else {
      history_add $context $globals(input);
      if {$context == "division" || $context == "condition"} {
        update_condition_cascade null $context;
        set globals($context,0) 1;
        update_condition_cascade 0 $context;
      }; # if
    }; # else
  }; # if

  return $status;

}; # condition_input()


proc profile_input {prompt {default ""} {base ""} {context "directory"}} {

  global globals;

  if {![set status [input $prompt $default $base $context]]} {
    if {$globals(input) != ""} {
      set atarget $globals(input);
      set target [string_strip $base $atarget];
      if {$target == ""} {
        tsdb_beep;
        status "invalid name for target database" 10;
        return -1;
      }; # if
      if {[file exists $atarget]} {
        tsdb_beep;
        status "database `$target' already exists" 10;
        return -1;
      }; # if
      set parent \
        [string range $atarget 0 [string last $globals(slash) $atarget]];
      if {[catch {file mkdir $parent}]} {
        tsdb_beep;
        status "error creating parent directory `$parent'" 10;
        return -1;
      }; # if
      history_add $context $target;
    }; # if
  }; # if

  return $status;

}; # profile_input()


proc aggregate_input {{set 1}} {

  global globals;

  .status.aggregate.esize delete 0 end;
  .status.aggregate.esize insert 0 $globals(aggregate_size);
  .status.aggregate.ethreshold delete 0 end;
  .status.aggregate.ethreshold insert 0 $globals(aggregate_threshold);
  .status.aggregate.elower delete 0 end;
  .status.aggregate.elower insert 0 $globals(aggregate_lower);
  .status.aggregate.eupper delete 0 end;
  .status.aggregate.eupper insert 0 $globals(aggregate_upper);

  #
  # <Tab> and shifted variants cycle (both directions) through entry fields
  #
  bind .status.aggregate.esize <Tab> {
    if {[entry_validate .status.aggregate.esize 1 "" "size"]} {
      focus .status.aggregate.ethreshold;
    }; # if
  }; # bind
  bind .status.aggregate.ethreshold <Tab> {
    if {[entry_validate .status.aggregate.ethreshold 1 "" "threshold"]} {
      focus .status.aggregate.elower;
    }; # if
  }; # bind
  bind .status.aggregate.elower <Tab> {
    if {[entry_validate .status.aggregate.elower 0 "" "lower"]} {
      focus .status.aggregate.eupper;
    }; # if
  }; # bind
  bind .status.aggregate.eupper <Tab> {
    if {[entry_validate .status.aggregate.eupper 0 "" "upper"]} {
      focus .status.aggregate.esize;
    }; # if
  }; # bind

  bind .status.aggregate.esize <Control-Tab> {
    if {[entry_validate .status.aggregate.esize 1 "" "size"]} {
      focus .status.aggregate.eupper;
    }; # if
  }; # bind
  bind .status.aggregate.esize <Alt-Tab> \
    [bind .status.aggregate.esize <Control-Tab>];
  bind .status.aggregate.esize <Shift-Tab> \
    [bind .status.aggregate.esize <Control-Tab>];
  catch {
    bind .status.aggregate.esize <ISO_Left_Tab> \
      [bind .status.aggregate.esize <Control-Tab>];
  }; # catch
  bind .status.aggregate.ethreshold <Control-Tab> {
    if {[entry_validate .status.aggregate.ethreshold 0 "" "threshold"]} {
      focus .status.aggregate.esize;
    }; # if
  }; # bind
  bind .status.aggregate.ethreshold <Alt-Tab> \
    [bind .status.aggregate.ethreshold <Control-Tab>];
  bind .status.aggregate.ethreshold <Alt-Tab> \
    [bind .status.aggregate.ethreshold <Control-Tab>];
  bind .status.aggregate.ethreshold <Shift-Tab> \
    [bind .status.aggregate.ethreshold <Control-Tab>];
  catch {
    bind .status.aggregate.ethreshold <ISO_Left_Tab> \
     [bind .status.aggregate.ethreshold <Control-Tab>];
  }; # catch

  bind .status.aggregate.elower <Control-Tab> {
    if {[entry_validate .status.aggregate.elower 0 "" "lower"]} {
      focus .status.aggregate.ethreshold;
    }; # if
  }; # bind
  bind .status.aggregate.elower <Alt-Tab> \
    [bind .status.aggregate.elower <Control-Tab>];
  bind .status.aggregate.elower <Shift-Tab> \
    [bind .status.aggregate.elower <Control-Tab>];
  catch {
    bind .status.aggregate.elower <ISO_Left_Tab> \
      [bind .status.aggregate.elower <Control-Tab>];
  }; # catch

  bind .status.aggregate.eupper <Control-Tab> {
    if {[entry_validate .status.aggregate.eupper 0 "" "upper"]} {
      focus .status.aggregate.elower;
    }; # if
  }; # bind
  bind .status.aggregate.eupper <Alt-Tab> \
    [bind .status.aggregate.eupper <Control-Tab>];
  bind .status.aggregate.eupper <Shift-Tab> \
    [bind .status.aggregate.eupper <Control-Tab>];
  catch {
    bind .status.aggregate.eupper <ISO_Left_Tab> \
      [bind .status.aggregate.eupper <Control-Tab>];
  }; # catch

  #
  # cursor <Up> and <Down> increment and decrement values; shifted variants
  # give steps of 10
  #
  bind .status.aggregate.esize <Up> {
    entry_incr .status.aggregate.esize 1 1 "" size;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.esize <Down> {
    entry_incr .status.aggregate.esize -1 1 "" size;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.esize <Shift-Up> {
    entry_incr .status.aggregate.esize 10 1 "" size;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.esize <Shift-Down> {
    entry_incr .status.aggregate.esize -10 1 "" size;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.esize <Control-Down> \
    [bind .status.aggregate.esize <Shift-Down>];
  bind .status.aggregate.esize <Control-Up> \
    [bind .status.aggregate.esize <Shift-Up>];
  bind .status.aggregate.esize <Alt-Down> \
    [bind .status.aggregate.esize <Shift-Down>];
  bind .status.aggregate.esize <Alt-Up> \
    [bind .status.aggregate.esize <Shift-Up>];

  bind .status.aggregate.ethreshold <Up> {
    entry_incr .status.aggregate.ethreshold 1 1 "" threshold;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.ethreshold <Down> {
    entry_incr .status.aggregate.ethreshold -1 1 "" threshold;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.ethreshold <Shift-Up> {
    entry_incr .status.aggregate.ethreshold 10 1 "" threshold;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.ethreshold <Shift-Down> {
    entry_incr .status.aggregate.ethreshold -10 1 "" threshold;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.ethreshold <Control-Down> \
    [bind .status.aggregate.ethreshold <Shift-Down>];
  bind .status.aggregate.ethreshold <Control-Up> \
    [bind .status.aggregate.ethreshold <Shift-Up>];
  bind .status.aggregate.ethreshold <Alt-Down> \
    [bind .status.aggregate.ethreshold <Shift-Down>];
  bind .status.aggregate.ethreshold <Alt-Up> \
    [bind .status.aggregate.ethreshold <Shift-Up>];

  bind .status.aggregate.elower <Up> {
    entry_incr .status.aggregate.elower 1 0 "" lower;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.elower <Down> {
    entry_incr .status.aggregate.elower -1 0 "" lower;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.elower <Shift-Up> {
    entry_incr .status.aggregate.elower 10 1 "" lower;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.elower <Shift-Down> {
    entry_incr .status.aggregate.elower -10 1 "" lower;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.elower <Control-Down> \
    [bind .status.aggregate.elower <Shift-Down>];
  bind .status.aggregate.elower <Control-Up> \
    [bind .status.aggregate.elower <Shift-Up>];
  bind .status.aggregate.elower <Alt-Down> \
    [bind .status.aggregate.elower <Shift-Down>];
  bind .status.aggregate.elower <Alt-Up> \
    [bind .status.aggregate.elower <Shift-Up>];

  bind .status.aggregate.eupper <Up> {
    entry_incr .status.aggregate.eupper 1 0 "" upper; 
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.eupper <Down> {
    entry_incr .status.aggregate.eupper -1 0 "" upper;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.eupper <Shift-Up> {
    entry_incr .status.aggregate.eupper 10 1 "" upper;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.eupper <Shift-Down> {
    entry_incr .status.aggregate.eupper -10 1 "" upper;
    raise .status.aggregate;
  }; # bind
  bind .status.aggregate.eupper <Control-Down> \
    [bind .status.aggregate.eupper <Shift-Down>];
  bind .status.aggregate.eupper <Control-Up> \
    [bind .status.aggregate.eupper <Shift-Up>];
  bind .status.aggregate.eupper <Alt-Down> \
    [bind .status.aggregate.eupper <Shift-Down>];
  bind .status.aggregate.eupper <Alt-Up> \
    [bind .status.aggregate.eupper <Shift-Up>];

  #
  # attach additional tag to all aggregate input widgets; makes life easier
  #
  foreach widget {lsize esize lthreshold ethreshold 
                  llower elower lupper eupper} {
    bindtags .status.aggregate.$widget \
      [concat [bindtags .status.aggregate.$widget] aggregate_input];
  }; # foreach

  #
  # <Return> terminates aggregate parameter input: read out values from entry
  # fields, validate, and store into global variables. <Control-G> aborts.
  #
  bind aggregate_input <Return> { 
    if {[entry_validate .status.aggregate.esize 1 "" size]
        && [entry_validate .status.aggregate.ethreshold 1 "" threshold]
        && [entry_validate .status.aggregate.elower 0 "" lower]
        && [entry_validate .status.aggregate.eupper 0 "" upper]} {
      set globals(aggregate_size) [.status.aggregate.esize get];
      set globals(aggregate_threshold) [.status.aggregate.ethreshold get];
      set globals(aggregate_lower) [.status.aggregate.elower get];
      set globals(aggregate_upper) [.status.aggregate.eupper get];
      set globals(errno) 0;
    }; # if
    break;
  }; # bind

  bind aggregate_input <Control-g> {
    set globals(errno) 1;
  }; # bind
  bind aggregate_input <Control-G> [bind aggregate_input <Control-g>];

  #
  # produce reminder of input modality if necessary
  #
  bind .status.aggregate <ButtonPress> {
    if {[winfo containing %X %Y] != ".status.aggregate.esize"
        && [winfo containing %X %Y] != ".status.aggregate.ethreshold"
        && [winfo containing %X %Y] != ".status.aggregate.elower"
        && [winfo containing %X %Y] != ".status.aggregate.eupper"} {
      tsdb_beep;
      status [format "%%s %%s" \
              "<Return> completes parameter input;" \
              "<Control-G> aborts"];
      after 1500;
      status "";
      raise .status.aggregate;
      update idletasks;
    }; # if
  }; # bind
  bind .status.aggregate.fill0 <ButtonPress> \
    [bind .status.aggregate <ButtonPress>];
  bind .status.aggregate.lsize <ButtonPress> \
    [bind .status.aggregate <ButtonPress>];
  bind .status.aggregate.fill1 <ButtonPress> \
    [bind .status.aggregate <ButtonPress>];
  bind .status.aggregate.lthreshold <ButtonPress> \
    [bind .status.aggregate <ButtonPress>];
  bind .status.aggregate.fill2 <ButtonPress> \
    [bind .status.aggregate <ButtonPress>];
  bind .status.aggregate.llower <ButtonPress> \
    [bind .status.aggregate <ButtonPress>];
  bind .status.aggregate.fill3 <ButtonPress> \
    [bind .status.aggregate <ButtonPress>];
  bind .status.aggregate.lupper <ButtonPress> \
    [bind .status.aggregate <ButtonPress>];

  #
  # activate aggregate parameter input pane
  #
  set focus [focus -displayof .];
  focus .status.aggregate.esize;
  grab set .status.aggregate;
  raise .status.aggregate;
  tkwait variable globals(errno);

  #
  # restore previous state
  #
  grab release .status.aggregate;
  focus $focus;
  raise .status.label;
  if {!$globals(errno) && $set} {
    tsdb_set "*statistics-aggregate-size*" $globals(aggregate_size);
    tsdb_set "*statistics-aggregate-threshold*" $globals(aggregate_threshold);
    tsdb_set "*statistics-aggregate-lower*" $globals(aggregate_lower);
    tsdb_set "*statistics-aggregate-upper*" $globals(aggregate_upper);
  }; # if
  return $globals(errno);

}; # aggregate_input()


proc graph_parameter_input {} {

  global globals;

  set aggregate_size $globals(aggregate_size);
  set aggregate_threshold $globals(aggregate_threshold);
  set aggregate_lower $globals(aggregate_lower);
  set aggregate_upper $globals(aggregate_upper);
  set globals(aggregate_size) $globals(graph_size);
  set globals(aggregate_threshold) $globals(graph_threshold);
  set globals(aggregate_lower) $globals(graph_lower);
  set globals(aggregate_upper) $globals(graph_upper);

  if {![set return [aggregate_input 0]]} {
    set globals(graph_size) $globals(aggregate_size);
    set globals(graph_threshold) $globals(aggregate_threshold);
    set globals(graph_lower) $globals(aggregate_lower);
    set globals(graph_upper) $globals(aggregate_upper);
  }; # if

  set globals(aggregate_size) $aggregate_size;
  set globals(aggregate_threshold) $aggregate_threshold;
  set globals(aggregate_lower) $aggregate_lower;
  set globals(aggregate_upper) $aggregate_upper;

  return $return;

}; # graph_parameter_input()


proc integer_input {lprompt {ldefault 0} {rprompt ""} {rdefault ""}} {

  global globals;


  .status.integer.lprompt config -text " $lprompt ";
  .status.integer.lvalue delete 0 end;
  .status.integer.lvalue insert 0 $ldefault;
  .status.integer.rprompt config -text "$rprompt ";
  .status.integer.rvalue delete 0 end;
  .status.integer.rvalue insert 0 $rdefault

  foreach slave [pack slaves .status.integer] {
    pack forget $slave;
  }; # foreach
  if {$rprompt == ""} {
    pack .status.integer.fill0 -side left -fill both -expand yes;
  }; # if
  pack .status.integer.lprompt -side left;
  pack .status.integer.lvalue -side left;
  pack .status.integer.fill1 -side left -fill both -expand yes;
  if {$rprompt != ""} {
    pack .status.integer.rprompt -side left;
    pack .status.integer.rvalue -side left;
  } else {
    pack .status.integer.fill2 -side left -fill both -expand yes;
  }; # else
  
  #
  # <Tab> and shifted variants cycle (both directions) through entry fields
  #
  bind .status.integer.lvalue <Tab> {
    if {[entry_validate .status.integer.lvalue]} {
      focus .status.integer.rvalue;
    }; # if
  }; # bind
  bind .status.integer.rvalue <Tab> {
    if {[entry_validate .status.integer.rvalue]} {
      focus .status.integer.lvalue;
    }; # if
  }; # bind

  bind .status.integer.lvalue <Control-Tab> {
    if {[entry_valdate .status.integer.lvalue]} {
      focus .status.integer.rvalue;
    }; # if
  }; # bind
  bind .status.integer.lvalue <Alt-Tab> \
    [bind .status.integer.lvalue <Control-Tab>];
  bind .status.integer.lvalue <Shift-Tab> \
    [bind .status.integer.lvalue <Control-Tab>];
  catch {
    bind .status.integer.lvalue <ISO_Left_Tab> \
      [bind .status.integer.lvalue <Control-Tab>];
  }; # catch

  bind .status.integer.rvalue <Control-Tab> {
    if {[entry_validate .status.integer.rvalue]} {
      focus .status.integer.lvalue;
    }; # if
  }; # bind
  bind .status.integer.rvalue <Alt-Tab> \
    [bind .status.integer.rvalue <Control-Tab>];
  bind .status.integer.rvalue <Alt-Tab> \
    [bind .status.integer.rvalue <Control-Tab>];
  bind .status.integer.rvalue <Shift-Tab> \
    [bind .status.integer.rvalue <Control-Tab>];
  catch {
    bind .status.integer.rvalue <ISO_Left_Tab> \
     [bind .status.integer.rvalue <Control-Tab>];
  }; # catch

  #
  # cursor <Up> and <Down> increment and decrement values; shifted variants
  # give steps of 10
  #
  bind .status.integer.lvalue <Up> {
    entry_incr .status.integer.lvalue 1;
    raise .status.integer;
  }; # bind
  bind .status.integer.lvalue <Down> {
    entry_incr .status.integer.lvalue -1;
    raise .status.integer;
  }; # bind
  bind .status.integer.lvalue <Shift-Up> {
    entry_incr .status.integer.lvalue 10;
    raise .status.integer;
  }; # bind
  bind .status.integer.lvalue <Shift-Down> {
    entry_incr .status.integer.lvalue -10;
    raise .status.integer;
  }; # bind
  bind .status.integer.lvalue <Control-Down> \
    [bind .status.integer.lvalue <Shift-Down>];
  bind .status.integer.lvalue <Control-Up> \
    [bind .status.integer.lvalue <Shift-Up>];
  bind .status.integer.lvalue <Alt-Down> \
    [bind .status.integer.lvalue <Shift-Down>];
  bind .status.integer.lvalue <Alt-Up> \
    [bind .status.integer.lvalue <Shift-Up>];

  bind .status.integer.rvalue <Up> {
    entry_incr .status.integer.rvalue 1;
    raise .status.integer;
  }; # bind
  bind .status.integer.rvalue <Down> {
    entry_incr .status.integer.rvalue -1;
    raise .status.integer;
  }; # bind
  bind .status.integer.rvalue <Shift-Up> {
    entry_incr .status.integer.rvalue 10;
    raise .status.integer;
  }; # bind
  bind .status.integer.rvalue <Shift-Down> {
    entry_incr .status.integer.rvalue -10;
    raise .status.integer;
  }; # bind
  bind .status.integer.rvalue <Control-Down> \
    [bind .status.integer.rvalue <Shift-Down>];
  bind .status.integer.rvalue <Control-Up> \
    [bind .status.integer.rvalue <Shift-Up>];
  bind .status.integer.rvalue <Alt-Down> \
    [bind .status.integer.rvalue <Shift-Down>];
  bind .status.integer.rvalue <Alt-Up> \
    [bind .status.integer.rvalue <Shift-Up>];

  #
  # attach additional tag to all integer input widgets; makes life easier
  #
  foreach widget {lprompt lvalue rprompt rvalue} {
    bindtags .status.integer.$widget \
      [concat [bindtags .status.integer.$widget] integer_input];
  }; # foreach

  #
  # <Return> terminates aggregate parameter input: read out values from entry
  # fields, validate, and store into global variables. <Control-G> aborts.
  #
  bind integer_input <Return> { 
    if {[entry_validate .status.integer.lvalue]
        && [entry_validate .status.integer.rvalue]} {
      set globals(integer,lvalue) [.status.integer.lvalue get];
      set globals(integer,rvalue) [.status.integer.rvalue get];
      set globals(errno) 0;
    }; # if
    break;
  }; # bind

  bind integer_input <Control-g> {
    set globals(errno) 1;
  }; # bind
  bind integer_input <Control-G> [bind integer_input <Control-g>];

  #
  # produce reminder of input modality if necessary
  #
  bind .status.integer <ButtonPress> {
    if {[winfo containing %X %Y] != ".status.integer.lvalue"
        && [winfo containing %X %Y] != ".status.integer.rvalue"} {
      tsdb_beep;
      status [format "%%s %%s" \
              "<Return> completes parameter input;" \
              "<Control-G> aborts"];
      after 1500;
      status "";
      raise .status.integer;
      update idletasks;
    }; # if
  }; # bind
  bind .status.integer.fill0 <ButtonPress> \
    [bind .status.integer <ButtonPress>];
  bind .status.integer.lprompt <ButtonPress> \
    [bind .status.integer <ButtonPress>];
  bind .status.integer.fill1 <ButtonPress> \
    [bind .status.integer <ButtonPress>];
  bind .status.integer.rprompt <ButtonPress> \
    [bind .status.integer <ButtonPress>];
  bind .status.integer.fill2 <ButtonPress> \
    [bind .status.integer <ButtonPress>];

  #
  # activate integer parameter input pane
  #
  set focus [focus -displayof .];
  focus .status.integer.lvalue;
  grab set .status.integer;
  raise .status.integer;
  tkwait variable globals(errno);

  #
  # restore previous state
  #
  grab release .status.integer;
  focus $focus;
  raise .status.label;
  return $globals(errno);

}; # integer_input()


proc file_completion {entry {base ""} {mode ""}} {

  set prefix "$base[$entry get]";
  set completion [complete $prefix "" $mode];
  if {$completion != "" } {
    set completion [string_strip $base $completion];
    $entry delete 0 end;
    $entry insert 0 $completion;
    $entry selection clear;
    $entry icursor end;
    $entry xview moveto 1; 
  }; # if
  
}; # file_completion()


proc select_completion {entry mode} {

  global globals;

  if {![info exists globals(relations)] 
      || ![info exists globals(attributes)]
      || $globals(relations) == "" 
      || $globals(attributes) == ""
      || ![llength $globals(relations)]
      || ![llength $globals(attributes)]} {
    read_database_schema $globals(data);
  }; # if

  set icursor [$entry index insert];
  set prefix [$entry get];
  set prefix [string range $prefix 0 [expr $icursor - 1]];

  if {$mode == "select"} {
    regexp -nocase {[a-z0-9_-]*$} $prefix match;
    set completions $globals(attributes);
  } elseif {$mode == "from"} {
    regexp -nocase {[a-z0-9_-]*$} $prefix match;
    set completions $globals(relations);
  } else {
    regexp -nocase {^(.*(&|\||!|\())? *([a-z0-9_-]*)$} $prefix \
      foo bar baz match;
    set completions $globals(attributes);
  }; # else

  if {[info exists match]} {
    set completion [complete $match $completions];
    if {$completion != "" } {
      set start [expr [string length $prefix] - [string length $match]];
      set end [expr $start + [string length $completion]];
      $entry delete $start $icursor;
      $entry insert $start $completion;
      $entry selection clear;
      $entry icursor $end;
    }; # if
  } else {
    tsdb_beep;
  }; # else

};# select_completion()


proc complete {prefix {completions ""} {mode ""}} {

  global globals;

  set file 0;
  if {$completions == ""} {
    set file 1;
    set completions [lsort -dictionary [glob -nocomplain -- "$prefix*"]]
  } else {
    set length [expr [string length $prefix] - 1];
    set foo {};
    foreach item $completions {
      if {![string compare $prefix [string range $item 0 $length]]} {
        lappend foo $item;
      }; # if
    };# foreach
    set completions $foo;
  }; # else

  if {$mode == "directory" || $mode == "profile"} {
    set foo {};
    foreach item $completions {
      if {[file isdirectory $item]} {
        lappend foo $item;
      }; # if
    }; # foreach
    set completions $foo;
  }; # if

  if {![llength $completions]} {
    tsdb_beep;
    return ""
  }; # if
  set completion [lindex $completions 0];
  if {[llength $completions] > 1} {
    set kaerb 0;
    for {set i [expr [string length $prefix] - 1]} {!$kaerb} {incr i} {
      set match [string range $completion 0 $i];
      foreach item $completions {
        if {[string compare $match [string range $item 0 $i]]} {
          set completion [string range $match 0 [expr $i - 1]];
          set kaerb 1;
        }; # if
      }; # foreach
    }; # for
  }; # if

  if {$file && [llength $completions] == 1 
      && [file isdirectory $completion] && $completion != $globals(slash)} {
    set completion "[file dirname [file join $completion .]]$globals(slash)"
  }; # if

  if {!$file && [llength $completions] == 1} {
    set completion "$completion ";
  }; # if

  if {[llength $completions] > 1} {
    show_completions $completions $file;
  } else {
    [.completions subwidget hlist] delete all;
  }; # else
  return $completion

}; # complete()


proc show_completions {completions {file 0}} {

  global globals;

  set clist [.completions subwidget hlist];
  set center [tixDisplayStyle text -bg yellow -anchor w -font {Helvetica 10}];

  $clist delete all
  set i 0;
  foreach item [lsort $completions] {
    set nitem $item;
    if {$file && [set slash [string last $globals(slash) $item]] >= 0} {
      set nitem [string range $item [expr $slash + 1] end];
    }; # if
    if {$file && [file isdirectory $item] && $item != $globals(slash)} {
      set nitem "[file dirname [file join $nitem .]]$globals(slash)"
    }; # if
    set hposition [expr {int($i / 3)}];
    set vposition [expr {$i % 3}];
    if {!$vposition} {
      $clist add $hposition -at $hposition -text $nitem -style $center;
    } else {
      $clist item create $hposition $vposition -text $nitem -style $center;
    }; # else
    incr i;
  };# foreach

  raise .completions;

};# show_completions()
