# Tix.tcl --
#
#	This file implements the Tix application context class
#
# Copyright (c) 1993-1999 Ioi Kim Lam.
# Copyright (c) 2000-2001 Tix Project Group.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id$

if 0 {
proc tix {} {
    # dummy proc. make sure the entry "tix" is in the tclIndex file
    #
}
}

tixClass tixAppContext {
    -superclass {}
    -classname  TixAppContext
    -method {
	cget configure addbitmapdir filedialog getbitmap getimage
	option platform resetoptions setbitmap
    }
    -flag {
	-binding -debug -extracmdargs -filedialog -fontset -grabmode
	-haspixmap -libdir -scheme -schemepriority -percentsubst
    }
    -readonly {
	-haspixmap
    }
    -configspec {
	{-binding    		TK}
	{-debug      		false}
	{-extracmdargs 		1}
	{-filedialog    	""}
	{-fontset    		TK}
	{-grabmode 		global}
	{-haspixmap 		0}
	{-libdir     		""}
	{-percentsubst		0}
	{-scheme     		TK}
	{-schemepriority     	21}
    }
    -alias {
    }
}

proc tixAppContext:Constructor {w} {
    upvar #0 $w data
    global tix_priv env argv0 tixPriv
    global tix_library tixOption tcl_platform

    if {[info exists tcl_platform] && $tcl_platform(platform) == "windows"} {
	regsub -all "/" $tix_library \\ tix_library
    }

    if {[info exists data(initialized)]} {
	error "tixAppContext has already be initialized"
    } else {
	set data(initialized) 1
    }

    if {[tixStrEq $tix_library ""]} {
	set data(et) 1
    } else {
	set data(et) 0
    }

    # Thses options were set when tixwish started up
    #
    set data(-binding)		$tix_priv(-binding)
    set data(-fontset)		$tix_priv(-fontset)
    set data(-scheme)		$tix_priv(-scheme)
    set data(-schemepriority)	$tix_priv(-schemepriority)

    if {![info exists tix_priv(isSafe)]} {
	set data(-libdir)	[tixFSAbsPath $tix_library]
    }
    set tixOption(prioLevel) $tix_priv(-schemepriority)

    tixAppContext:initTixOptions

    # -fontset and -scheme are not supported in this version
    #
    #
    #if {![info exists tix_priv(isSafe)]} {
    #	tixAppContext:config-fontset $w $data(-fontset)
    #	tixAppContext:config-scheme  $w $data(-scheme)
    #}

    tixAppContext:BitmapInit $w
    tixAppContext:FileDialogInit $w
}

proc tixAppContext:initTixOptions {} {
    global tixOption

    #
    # TEMP: backwards compatibility code to make [tix option get] work
    # without the help of -fontset and -scheme
    
    set tixOption(font)         [tixGetDefault CTL_FONT]
    set tixOption(bold_font)    [tixGetDefault CTL_FONT]
    set tixOption(menu_font)    [tixGetDefault CTL_FONT]
    set tixOption(italic_font)  [tixGetDefault CTL_FONT]
    set tixOption(fixed_font)   [tixGetDefault CTL_FONT]
    set tixOption(border1)      1


    set tixOption(bg)           #d9d9d9
    set tixOption(fg)           black

    set tixOption(dark1_bg)     #c3c3c3
    set tixOption(dark1_fg)     black
    set tixOption(dark2_bg)     #a3a3a3
    set tixOption(dark2_fg)     black
    set tixOption(inactive_bg)  #a3a3a3
    set tixOption(inactive_fg)  black

    set tixOption(light1_bg)    #ececec
    set tixOption(light1_fg)    white
    set tixOption(light2_bg)    #fcfcfc
    set tixOption(light2_fg)    white

    set tixOption(active_bg)    $tixOption(dark1_bg)
    set tixOption(active_fg)    $tixOption(fg)
    set tixOption(disabled_fg)  gray55

    set tixOption(input1_bg)    #d9d9d9
    set tixOption(input2_bg)    #d9d9d9
    set tixOption(output1_bg)   $tixOption(dark1_bg)
    set tixOption(output2_bg)   $tixOption(bg)

    set tixOption(select_fg)    black
    set tixOption(select_bg)    #c3c3c3

    set tixOption(selector)	#b03060
}

#----------------------------------------------------------------------
#  Configurations
#
#----------------------------------------------------------------------
proc tixAppContext:resetoptions {w scheme fontset {schemePrio ""}} {
    upvar #0 $w data

    if {! $data(et)} {
	global tixOption
	option clear

	if {$schemePrio != ""} {
	    set tixOption(prioLevel) $schemePrio
	}
	tixAppContext:config-scheme  $w $scheme
	tixAppContext:config-fontset $w $fontset
    }
}

proc tixAppContext:config-fontset {w value} {
    upvar #0 $w data
    global tix_priv tixOption

    set data(-fontset) $value

    #-----------------------------------
    # Initialization of options database
    #-----------------------------------
    # Load the fontset
    #
    if {!$data(et)} {
        set prefDir [file join $data(-libdir) pref]
        set fontSetFile [file join $prefDir $data(-fontset).fsc]
	if {[file exists $fontSetFile]} {
	    source $fontSetFile
	    tixPref:InitFontSet:$data(-fontset)
	    tixAppContext:CheckFontSets $w
	    tixPref:SetFontSet:$data(-fontset)
	} else {
	    tixAppContext:StartupError \
		"\aError: cannot use fontset \"$data(-fontset)\""
	    tixAppContext:StartupError \
		"       Using default fontset "
	    tixSetDefaultFontset
	    tixAppContext:CheckFontSets $w
	}
    } else {
	if [catch {
	    tixPref:InitFontSet:$data(-fontset)
	    tixAppContext:CheckFontSets $w
	    tixPref:SetFontSet:$data(-fontset)
	}] {
	    # User chose non-existent fontset
	    #
	    tixAppContext:StartupError \
		"\aError: cannot use fontset \"$data(-fontset)\""
	    tixAppContext:StartupError \
		"       Using default fontset "
	    tixSetDefaultFontset
	    tixAppContext:CheckFontSets $w
	}
    }

    # Compatibility stuff: the obsolete name courier_font has been changed to
    # fixed_font
    set tixOption(courier_font) $tixOption(fixed_font)
}

proc tixAppContext:config-scheme {w value} {
    upvar #0 $w data
    global tix_priv

    set data(-scheme) $value

    # Load the color scheme
    #
    if {!$data(et)} {
	set schemeName [file join [file join $data(-libdir) pref] \
	    $data(-scheme).csc]
	if {[file exists $schemeName]} {
	    source $schemeName
	    if {[winfo depth .] >= 8} {
		tixPref:SetScheme-Color:$data(-scheme)
	    } else {
		tixPref:SetScheme-Mono:$data(-scheme)
	    }
	} else {
	    tixAppContext:StartupError \
		"\aError: cannot use color scheme \"$data(-scheme)\""
	    tixAppContext:StartupError \
		"       Using default color scheme"
	    if {[winfo depth .] >= 8} {
		tixSetDefaultScheme-Color
	    } else {
		tixSetDefaultScheme-Mono
	    }
	}
    } else {
	if [catch {
	    if {[winfo depth .] >= 8} {
		tixPref:SetScheme-Color:$data(-scheme)
	    } else {
		tixPref:SetScheme-Mono:$data(-scheme)
	    }
	}] {
	    # User chose non-existent color scheme
	    #
	    tixAppContext:StartupError \
		"\aError: cannot use color scheme \"$data(-scheme)\""
	    tixAppContext:StartupError \
		"       Using default color scheme"
	    if {[winfo depth .] >= 8} {
		tixSetDefaultScheme-Color
	    } else {
		tixSetDefaultScheme-Mono
	    }
	}
    }
}

#----------------------------------------------------------------------
#  Private methods
#
#----------------------------------------------------------------------
proc tixAppContext:BitmapInit {w} {
    upvar #0 $w data

    # See whether we have pixmap extension
    #
    set data(-haspixmap) true

    # Dynamically set the bitmap directory
    #
    if {! $data(et)} {
	set data(bitmapdirs) [list [file join $data(-libdir) bitmaps]]
    } else {
	set data(bitmapdirs) ""
    }
}

proc tixAppContext:FileDialogInit {w} {
    upvar #0 $w data

    if {$data(-filedialog) == ""} {
	set data(-filedialog) [option get . fileDialog FileDialog]
    }
    if {$data(-filedialog) == ""} {
	set data(-filedialog) tixFileSelectDialog
    }
}

#----------------------------------------------------------------------
# If a font in the fontset is not available, use a default fontset.
#
proc tixAppContext:CheckFontSets  {w} {
    upvar #0 $w data
    global tixOption tcl_version

    if {$tcl_version >= "8.0"} {
	# fonts will never fail ..
	return
    }

    set default_font "fixed"
    set options {font bold_font menu_font italic_font fixed_font}

    if {[winfo exists .tix-xxx-test]} {
	destroy .tix-xxx-test
    }
    set lab [label .tix-xxx-test]
    foreach opt $options {
	if {[catch {$lab config -font $tixOption($opt)}]} {
	    tixAppContext:StartupError \
		"\aError: cannot use font \"$tixOption($opt)\" as \"$opt\""
	    puts  stderr \
		"       using \"$default_font\" instead"

	    set tixOption($opt) $default_font
	}
    }
    destroy $lab
}

#----------------------------------------------------------------------
# 	Public methods
#----------------------------------------------------------------------
proc tixAppContext:addbitmapdir {w bmpdir} {
    upvar #0 $w data

    if {[lsearch $data(bitmapdirs) $bmpdir] == "-1"} {
	lappend data(bitmapdirs) $bmpdir 
    }
}

proc tixAppContext:getimage {w name} {
    upvar #0 $w data
    global tixPriv tix_priv

    if {[info exists data(img:$name)]} {
	return $data(img:$name)
    }

    if {![info exists tix_priv(isSafe)]} {
	foreach dir $data(bitmapdirs) {
	    if {[file exists [file join $dir $name.xpm]]} {
		if {![catch {
		    set data(img:$name) \
			[image create pixmap -file [file join $dir $name.xpm]]
		}]} {
		    break
		}
	    }
	    if {[file exists [file join $dir $name.gif]]} {
		global TRANSPARENT_GIF_COLOR
		if {![catch {
		    set data(img:$name) \
			[image create photo -file [file join $dir $name.gif]]
		}]} {
		    break
		}
	    }
	    if {[file exists [file join $dir $name.ppm]]} {
		if {![catch {
		    set data(img:$name) \
			[image create photo -file [file join $dir $name.ppm]]
		}]} {
		    break
		}
	    }
	    if {[file exists [file join $dir $name.xbm]]} {
		if {![catch {
		    set data(img:$name) \
			[image create bitmap -file [file join $dir $name.xbm]]
		}]} {
		    break
		}
	    }
	    if {[file exists [file join $dir $name]]} {
		if {![catch {
		    set data(img:$name) \
			[image create bitmap -file [file join $dir $name]]
		}]} {
		    break
		}
	    }
	}
    }

    if {![info exists data(img:$name)]} {
	catch {
	    # This is for compiled-in images
	    set data(img:$name) [image create pixmap -id $name]
	} err
	if {[string match internal* $err]} {
	    error $err
	}
    }

    if {[info exists data(img:$name)]} {
	return $data(img:$name)
    } else {
	error "image file \"$name\" cannot be found"
    }
}


proc tixAppContext:getbitmap {w bitmapname} {
    upvar #0 $w data
    global tix_priv

    if {[info exists data(bmp:$bitmapname)]} {
	return $data(bmp:$bitmapname)
    } else {
	set ext [file extension $bitmapname]
	if {$ext == ""} {
	    set ext .xbm
	}

	# This is the fallback value. If we can't find the bitmap in
	# the bitmap directories, then use the name of the bitmap
	# as the default value.
	#
	set data(bmp:$bitmapname) $bitmapname

	if {[info exists tix_priv(isSafe)]} {
	    return $data(bmp:$bitmapname)
	}

	foreach dir $data(bitmapdirs) {
	    case $ext {
		.xbm {
		    if {[file exists [file join $dir $bitmapname.xbm]]} {
			set data(bmp:$bitmapname) \
			    @[file join $dir $bitmapname.xbm]
			break
		    }
		    if {[file exists [file join $dir $bitmapname]]} {
			set data(bmp:$bitmapname) @[file join $dir $bitmapname]
			break
		    }
		}
		default {
		    if {[file exists [file join $dir $bitmapname]]} {
			set data(bmp:$bitmapname) @[file join $dir $bitmapname]
			break
		    }
		}
	    }
	}

	return $data(bmp:$bitmapname)
    }
}

proc tixAppContext:filedialog {w {type tixFileSelectDialog}} {
    upvar #0 $w data

    if {$type == ""} {
	set type $data(-filedialog)
    }
    if {![info exists data(filedialog,$type)]} {
	set data(filedialog,$type) ""
    }

    if {"$data(filedialog,$type)" == "" ||
        ![winfo exists $data(filedialog,$type)]} {
	set data(filedialog,$type) [$type .tixapp_filedialog_$type]
    }

    return $data(filedialog,$type)
}

proc tixAppContext:option {w action option {value ""}} {
    upvar #0 $w data
    global tixOption

    if {$action == "get"} {
	return $tixOption($option)
    }
}

proc tixAppContext:platform {w} {
    global tcl_platform

    return $tcl_platform(platform)
}

proc tixAppContext:StartupError {msg} {
    catch {
	puts stderr $msg
    }
}
