
set bltHierBox(afterId) {}
set bltHierBox(space) off
set bltHierBox(x) 0
set bltHierBox(y) 0

proc blt::HierBoxAutoScan { w } {
    global bltHierBox
    if { ![winfo exists $w] } {
	return
    }
    set x $bltHierBox(x)
    set y $bltHierBox(y)
    if { $y >= [winfo height $w] } {
	$w yview scroll 1 units
    } elseif { $y < 0 } {
	$w yview scroll -1 units
    } 
    blt::HierBoxMotion $w $x $y
    set bltHierBox(afterId) [after 10 blt::HierBoxAutoScan $w]
}

proc blt::HierBoxSelect { widget x y } {
    set node [$widget nearest $x $y where]
    switch $where {
	"gadget" { 
	    $widget toggle $node 
	}
	"select" {
	    $widget selection clear 0 end
	    $widget selection set $node
	    $widget activate $node
	    $widget selection anchor $node 
	}
    }
}

proc blt::HierBoxStartSelect { widget x y } {
    set node [$widget nearest $x $y]
    $widget selection anchor $node
}

proc blt::HierBoxMotion { widget x y } {
    set node [$widget nearest $x $y]
    global bltHierBox
    $widget selection dragto $node $bltHierBox(action)
}

proc blt::HierBoxEndSelect { widget x y } {
    set node [$widget nearest $x $y where]
    if { $node == "" } {
	$widget selection cancel
	return
    }
    global bltHierBox
    $widget selection $bltHierBox(action) anchor $node
    $widget selection anchor $node
}

bind Hierbox <ButtonPress-1> { 
    set bltHierBox(action) ""
    blt::HierBoxSelect %W %x %y
}
bind Hierbox <Shift-ButtonPress-1> { 
    set bltHierBox(action) "set"
    blt::HierBoxStartSelect %W %x %y
}
bind Hierbox <Control-ButtonPress-1> { 
    set bltHierBox(action) "toggle"
    blt::HierBoxStartSelect %W %x %y
}
bind Hierbox <B1-Motion> { 
    if { $bltHierBox(action) != "" } {
	set bltHierBox(x) %x
	set bltHierBox(y) %y
	blt::HierBoxMotion %W %x %y
    } 
}
bind Hierbox <ButtonRelease-1> { 
    if { $bltHierBox(action) != "" } {
	after cancel $bltHierBox(afterId)
	blt::HierBoxEndSelect %W %x %y
    }
}

bind Hierbox <ButtonPress-3> {
    %W hide [%W nearest %x %y]
}
bind Hierbox <Shift-ButtonPress-3> {
    %W show [%W nearest %x %y]
}

bind Hierbox <B2-Motion> {
    %W scan dragto @%x,%y
}
bind Hierbox <ButtonPress-2> {
    set bltHierBox(cursor) [%W cget -cursor]
    %W configure -cursor hand1
    %W scan mark @%x,%y
}
bind Hierbox <ButtonRelease-2> {
    %W configure -cursor $bltHierBox(cursor)
}
bind Hierbox <KeyPress-Up> {
    %W activate up
    %W see active
    if { $bltHierBox(space) } {
	%W selection toggle active
    }
}
bind Hierbox <KeyPress-Down> {
    %W activate down
    %W see active
    if { $bltHierBox(space) } {
	%W selection toggle active
    }
}

bind Hierbox <Shift-KeyPress-Up> {
    %W activate parent
    %W see active
}
bind Hierbox <Shift-KeyPress-Down> {
    %W activate sibling
    %W see active
}

bind Hierbox <B1-Leave> {
    blt::HierBoxAutoScan %W 
}

bind Hierbox <B1-Enter> {
    after cancel $bltHierBox(afterId)
}

# PageUp
bind Hierbox <KeyPress-Prior> {
    if { [%W index active] != [%W index view.top] } {
	%W activate view.top
    } else {
	%W yview scroll -1 pages
	update
	%W activate view.bottom
    }
}
# PageDown
bind Hierbox <KeyPress-Next> {
    if { [%W index active] != [%W index view.bottom] } {
	%W activate view.bottom
    } else {
	%W yview scroll 1 pages
	update; update
	%W activate view.top
    }
}

bind Hierbox <Control-KeyPress-l> {
    %W see active
}
bind Hierbox <KeyPress-Left> {
    %W close active
}
bind Hierbox <KeyPress-Right> {
    %W open active
}

bind Hierbox <KeyPress-space> {
    %W selection toggle active
    set bltHierBox(space) on
}

bind Hierbox <KeyRelease-space> {
    set bltHierBox(space) off
}


bind Hierbox <KeyPress> {
    if { [string match {[A-Za-z0-9]*} "%A"] } {
	set last [%W index active]
	set next [%W index next]
	while { $next != $last } {
	    set label [%W entry cget $next -label]
	    if { [string index $label 0] == "%A" } {
		break
	    }
	    set next [%W index -at $next next]
	}
	%W activate $next
	%W see active
    }
}

# Home
bind Hierbox <KeyPress-Home> {
    %W activate root
    %W see active
}
bind Hierbox <KeyPress-End> {
    %W activate end
    %W see active
}

bind Hierbox <KeyPress-F1> {
    %W open -r root
}

bind Hierbox <KeyPress-F2> {
    eval %W close -r [%W entry children root 0 end] 
}
