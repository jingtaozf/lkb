proc balloon_setup {file} {

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

    if {![regexp -indices {[ \t][ \t]*} $line indices]} {
      tsdb_beep;
      status "ignoring balloon entry `$line'" 10;
      continue;
    }; # if

    set window [string range $line 0 [expr [lindex $indices 0] - 1]];
    set text [string range $line [expr [lindex $indices 1] + 1] end];
    if {[winfo exists $window]} {
      bind $window <Enter> [list balloon post $text];    
      bind $window <Leave> [list balloon unpost];
    }; # if
  }; # while

}; # balloon_setup() 


proc balloon {action {text ""}} {

  global globals;

  if {$action == "post"} {
    set text " $text";
    set globals(balloon) $text;
    raise .status.balloon;
  } else {
    raise .status.label;
  }; # else

}; # balloon()