#!/usr/bin/perl

# (Ben Waldron 24-01-2006)
# Usage: sent-split.pl INPUT_FILENAME > OUTPUT_FILENAME
#
# Notes:
# - this is a TOY proof-of-concept script
# - assumes UTF-8 I/O
#
# Sample INPUT_FILENAME:
#
#<a>Here is a semtence. And <i>here</i><nothing id='n3'/> is another</a>
#
# And another?
#
# Sample output:
#
#<!DOCTYPE maf SYSTEM 'saf.dtd'>
#<saf document='sample-xml-doc' addressing='char'>
#<sentence id='s0' from='0' to='22'/>
#<sentence id='s1' from='22' to='73'/>
#<sentence id='s2' from='73' to='86'/>
#</saf>

binmode(STDOUT, ":utf8");

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

print "<?xml version='1.0' encoding='UTF8'?>";
print "<!DOCTYPE maf SYSTEM 'saf.dtd'>\n";
print "<saf document='".xml_escape($file)."' addressing='char'>\n";

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
	    print " value='".xml_escape($sent)."'";
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

sub xml_escape {
    my $str = shift(@_);
    $str =~ s/\&/&amp;/g;
    $str =~ s/\>/&gt;/g;
    $str =~ s/\</&lt;/g;
    $str =~ s/\"/&quot;/g;
    $str =~ s/\'/&apos;/g;
    return $str;
}
