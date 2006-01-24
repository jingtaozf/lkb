#!/usr/bin/perl

# (Ben Waldron 23-01-2006)
# Usage: sent-split.pl INPUT_FILENAME
#
# Sample INPUT_FILENAME:
#<a>Here is a semtence. And <i>here</i><nothing id='n3'/> is another</a>
#
# And another?
#
# Sample output:
#<!DOCTYPE maf SYSTEM 'saf.dtd'>
#<saf document='sample-xml-doc' addressing='char'>
#<sentence id='s0' from='0' to='22'/>
#<sentence id='s1' from='22' to='73'/>
#<sentence id='s2' from='73' to='86'/>
#</saf>

@boundary_chars = (".", "!", "?", ";");

$file = $ARGV[0];

# relative pathname -> absolute pathname
if ($file !~ m/^\//) 
{
    open(X, "pwd|");
    $pwd = <X>;
    close(X);
    chomp $pwd;
    $file="${pwd}/${file}";
}
 
# open file in UTF-8 mode
open(INFO, "<:utf8", $file);

print "<!DOCTYPE maf SYSTEM 'saf.dtd'>\n";
print "<saf document='$file' addressing='char'>\n";

$id=0; $i=0; $from=$i; $i++; $sent="";
$c = getc INFO;
while ($c ne undef )
{
    # no sentence breaks inside markup
    if ($c eq "<") {$markup=1}
    if ($c eq ">") {$markup=0}
    $sent="$sent$c";
    $flag=0;
    foreach $bchar (@boundary_chars)
    {
	if ($c eq $bchar) 
	{
	    $flag=1;
	}
    }
    if (($last eq "\n") and ($c eq "\n")) {$flag=1}
    if ($flag and not $markup)
    {
	if ($from > -1)
	{
	    $to=$i;
	    print "<sentence id='s$id' from='$from' to='$to'";
	    # include sentence text in value attribute
	    $sent =~ s/\&/&amp;/g;
	    $sent =~ s/\>/&gt;/g;
	    $sent =~ s/\</&lt;/g;
	    $sent =~ s/\"/&quot;/g;
	    $sent =~ s/\'/&apos;/g;
	    print " value='$sent'";
	    print "/>\n";
	    $from=$i; $to=-1; $id++; $sent="";
	}
    } 
    $last=$c; $i++;
    $c = getc INFO;
}

$to=$i-1;
if ((1+$from) < $to) 
{
    print "<sentence id='s$id' from='$from' to='$to'/>\n";
}
$from=-1; $to=-1; $id++;

print "</saf>\n";

close(INFO);

