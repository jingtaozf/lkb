#!/usr/bin/perl -w

#Script for simple text preprocessing.
#Usage: scrub.pl textfile > new-file

# \d digit
# \D anything but a digit
# \w letter or digit
# \W anything but a letter or digit
# \b word boundary space/puncutation

$| = 1;

while (<>) {

    #0) remove ampersand-colon'ed escapes, eg, &quote;
    #(used e.g in the BNC)    
    s/(&\w*;)//g;

    #1) fix postfixed quotes properly
    s/(\w)([.,!?]?)(\")/$1 $2 $3/g; 

    #various "explosions" / space insertions:

    #2) postfix commas 
    s/(.)(\,)([\s\"])/$1 $2 $3/g;
    #must come before 5) to handle e.g "(this), stuff" correctly.

    #3) non-letter prefix 
    s/([\"\(\{\[\\\/&\$\£])([\w\"])/$1 $2/g;

    #4) non-letter infix
    s/(\w)([-\\\/])(\w)/$1 $2 $3/g;

    #4b) treat genetive "'s" as a separate token:
    s/(\')(\w+)(\')/$1 $2 $3/g; #single quote apostrophy. fails on quotes of longer words..
    s/(\w)(\'s)/$1 $2/g; #genetive s apostrophy

    #5) non-letter postfix 
    s/([\w\"])([\)\}\]\'\\\/&\=\$\£\%;:])([\s.?!,:;])/$1 $2 $3/g;

    #6) sentence-final punctuation
    s/(.)([.?!,:;])$/$1 $2/g;

    #7) misc.

    #map numbers to token 'digitersatz'
    s/(\D*)(\d+[.,:]?\d+)+(\D*)/$1 $2 $3/g;
    s/(\d+)/digitersatz/g;

    #fold everything to lower case:
    print lc($_);
}

