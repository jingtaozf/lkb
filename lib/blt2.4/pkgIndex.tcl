# Tcl package index file, version 1.0

proc LoadBLT { version dir } {

    set suffix [info sharedlibextension]
    regsub {\.} $version {} version_no_dots

    # Determine whether to load the normal BLT library or 
    # the "lite" tcl-only version.
    
    if { [info commands tk] == "tk" } {
        set library BLT${version_no_dots}${suffix}
    } else {
        set library BLTlite${version_no_dots}${suffix}
    }
    
    global tcl_platform
    if { $tcl_platform(platform) == "unix" } {
	set library [file join $dir lib${library}]
    } 
    load $library BLT
}

set version "2.4"
set libdir  "/usr/tcltk/lib"

package ifneeded BLT $version [list LoadBLT $version $libdir]

# End of package index file
