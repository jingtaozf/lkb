#!/coli/apps/tcl8.0+tk8.0/bin/bltwish
#!../bltwish
source bltDemo.tcl
# ----------------------------------------------------------------------
# This procedure is invoked each time a token is grabbed from the
# sample window.  It configures the token to display the current
# color, and returns the color value that is later passed to the
# target handler.
# ----------------------------------------------------------------------
proc package_color {token} {
    set bg [.sample cget -background]
    set fg [.sample cget -foreground]

    $token.label configure -background $bg -foreground $fg
    return $bg
}

# ----------------------------------------------------------------------
# Main application window...
# ----------------------------------------------------------------------
label .sample -text "Color" -height 2 -borderwidth 3 -relief sunken

#
# Set up the color sample as a drag&drop source for "color" values:
#
drag&drop source .sample -packagecmd {package_color %t}
drag&drop source .sample handler color

#
# Set up the color sample as a drag&drop target for "color" values:
#
drag&drop target .sample handler color {set_color %v}

#
# Establish the appearance of the token window:
#
set token [drag&drop token .sample]
label $token.label -text "Color"
pack $token.label

scale .redScale -label "Red" -orient horizontal \
    -from 0 -to 255 -command adjust_color
frame .redSample -width 20 -height 20 -borderwidth 3 -relief sunken

scale .greenScale -label "Green" -orient horizontal \
    -from 0 -to 255 -command adjust_color
frame .greenSample -width 20 -height 20 -borderwidth 3 -relief sunken

scale .blueScale -label "Blue" -orient horizontal \
    -from 0 -to 255 -command adjust_color
frame .blueSample -width 20 -height 20 -borderwidth 3 -relief sunken

# ----------------------------------------------------------------------
# This procedure loads a new color value into this editor.
# ----------------------------------------------------------------------
proc set_color {cval} {
    set rgb [winfo rgb . $cval]

    set rval [expr round([lindex $rgb 0]/65535.0*255)]
    .redScale set $rval

    set gval [expr round([lindex $rgb 1]/65535.0*255)]
    .greenScale set $gval

    set bval [expr round([lindex $rgb 2]/65535.0*255)]
    .blueScale set $bval
}

# ----------------------------------------------------------------------
# This procedure is invoked whenever an RGB slider changes to
# update the color samples in this display.
# ----------------------------------------------------------------------
proc adjust_color {args} {
    set rval [.redScale get]
    .redSample configure -background [format "#%.2x0000" $rval]
    set gval [.greenScale get]
    .greenSample configure -background [format "#00%.2x00" $gval]
    set bval [.blueScale get]
    .blueSample configure -background [format "#0000%.2x" $bval]

    .sample configure -background \
        [format "#%.2x%.2x%.2x" $rval $gval $bval]
    if {$rval+$gval+$bval < 1.5*255} {
        .sample configure -foreground white
    } else {
        .sample configure -foreground black
    }
}

table . .sample      0,0 -columnspan 2 -fill both -pady {0 4}
table . .redScale    1,0 -fill both
table . .redSample   1,1 -fill both
table . .greenScale  2,0 -fill both
table . .greenSample 2,1 -fill both
table . .blueScale   3,0 -fill both
table . .blueSample  3,1 -fill both
