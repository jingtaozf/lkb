#! /usr/local/bin/perl
##
## tdl2tex.perl
##
## This program converts a types file in TDL notation to LaTex file with AVM notation.
##
## Chris Callison-Burch	10/3/99

$infile = "types.tdl";
$outfile = "tex-types.TEX";
$initial_table_header = "TYPES";
$table_boundry_marker = "<<< >>>";

open(TYPES, $infile) or die "Cannot open the types file \"$infile\" for reading: $!";
open(TEX, ">$outfile") or die "Cannot open the output file \"$outfile\": $!";; 


print TEX "\\documentclass[11pt]{article}\n";   #LaTex Header
print TEX "\\usepackage{avm}\n\n"; 														 #LaTex Header
print TEX "\\begin{document}\n\n"; 														 #LaTex Header

print TEX "\\begin\{tabular\}\[t\]\{\|l\|l\|l\|\}  \\hline\n";    #LaTex table Header
print TEX "\\multicolumn\{3\}\{\|c\|\}\{$initial_table_header\}\\\\ \\hline\\hline\n";
print TEX "  TYPE \& FEATURES\/CONSTRAINTS \& IST\\\\ \\hline\n";


while (chop($line = <TYPES>)) {

$printAVM = 1;

if($line =~ /$table_boundry_marker/) {      # If the line contains a boundry marker then end the current table and start a new one.
	$line =~ s/.*$table_boundry_marker//g;       # using whatever follows the marker as the table heading.
		
	print TEX "\\end\{tabular\}\n\n";               #Table Footer

	print TEX "\\begin\{tabular\}\[t\]\{\|l\|l\|l\|\}  \\hline\n";    #LaTex table Header
	print TEX "\\multicolumn\{3\}\{\|c\|\}\{$line\}\\\\ \\hline\\hline\n";	
	print TEX "  TYPE \& FEATURES\/CONSTRAINTS \& IST\\\\ \\hline\n";
}

if($line =~ /:=/) {                     ## if the line contains the start of a definition
	($type, $supertypes) = split(/:=/, $line);
		print TEX "\n\{\\it $type \}\&\n";           ## FIRST, print the type.

		if($line =~ /\./) { $printAVM = 0;	}									## if there's no feature structure definition 
																												                   ## set the variable not to build an AVM

  if($printAVM) {
	  print TEX "\n\\begin{avm}\n";          ## THEN, print the AVM
			while($printAVM) {                   ## until the definition ends.
                            chop($line = <TYPES>);                        
                            if($line =~ /\]\./) { $printAVM = 0; }
   
                         
                            $line =~ s/^(\s*)//g;            ## Removes leading whitespace.
                            $line =~ s/\&//g;            ## Removes "&" so that it's not interpreted as a tab car.
                            $line =~ s/\[/\\\[/g;               ## Changes "[" to "\[" for AVM format.
                            $line =~ s/\]/\\\]/g;               ## Changes "]" to "\]" for AVM format.

                            if($line =~ /\<|\>/) {             ## If the line contains a  list, then  
                     	 							$line =~ s/,\s*$/\\\\/g;         ## just change the ending comma to a newline "\\".
                            } else {                           ## Otherwise,
                            		$line =~ s/,/\\\\/g;             ## Change all ","s to "\\"s for AVM format.
                            }
                            $line =~ s/\]\./\]/g;               ## Changes "]." to "]" for AVM format.
                            $line =~ s/\./\$\|\$/g;              ## Changes remaining "."s to "$|$"s for AVM format.
                            $line =~ s/\</\\q\</g;               ## Changes "<" to "\q<" for AVM format.
                            $line =~ s/\>/\\q\>/g;               ## Changes ">" to "\q>" for AVM format.


                            $line =~ s/\/l/\//g;               ## Changes "/l" to "/" for consitency with the textbook notation.                          
                            $line =~ s/([A-Z]+)\s+(\*?\w+)/\1 & \2/g;	## adds a "&" (tab char) type names which have non-fs values following them
                            $line =~ s/\#(\w*)/\\\@\{\1\}/g;                 ## changes a "#tag" to a "\@{tag}".

                            print TEX "$line\n";
                        }
	  print TEX "\\end{avm}\n\n";
      }
		$supertypes =~ s/\s*\& (\w+)/,\n\1/g;      ## Changes "&" to ",\n" if a word follows it.
		$supertypes =~ s/\&//g;                ## Deletes "&" otherwise.
		$supertypes =~ s/\.//g;                ## Deletes "." if there is one.

		print TEX "\&\{\\it $supertypes \}\\\\ \\hline\n\n";
    }
}


print TEX "\\end\{tabular\}\n\n";               #Table Footer
print TEX "\\end{document}\n\n"; 														 #LaTex Footer

close(TEX);
close(TYPES);
 
