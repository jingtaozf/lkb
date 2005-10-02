#!/usr/bin/perl -w


#aux-function used to strip of the dots in abbreviations
sub kill_dot { $_ = shift(@_); $_ =~ s/\.//g; return $_;}

$| = 1;

while (<>) {    
    
    #remove ampersand-colon'ed escapes, eg, &quote;
    $_ =~ s/(&\w*;)//g;
    
    #treat genetive "'s" as a separate token
    $_ =~ s/(\w)(\'s)/$1 $2/g;
    
    #remove no-letter chars and insert space, 
#    $_ =~ s/[\{\}\[\]\(\)\/\`\'\~\@\*\&\+\=\#\@]/ /g;
    $_ =~ s/[\{\}\[\]\(\)\/\`\~\@\*\&\+\=\#\@]/ /g;


#    #insert context cue </s> to mark end-of-sentence.
#    $_ =~ s/([?!\#:;.]*)([\t\ ]*)([\r\n])/ \<\\s\>$3/g;

    
    #remove sentence-final punctuation
  #  $_ =~ s/([?!\#:;.]*)([\t\ ]*)([\r\n])/ $3/g;

    #implode abbreviations
    $_ =~ s/([\D\w]\.(?:[\D\w]\.)*)/kill_dot($1)/eg; 
    #will map both 'P.O' and 'P.O.' to 'PO'


    #explode hyphens
    $_ =~ s/(\w)(-)(\w)/$1 $2 $3/g; 
    #e.g. 'slug-like' ==> 'slug - like'
    
    
    #remove remaining punctuation, except in numerical expressions
#   $_ =~ s/(\D)[.,:;]+([\D\S])/$1 $2/g;
    
    
    #map numbers to token 'digitersatz'
    $_ =~ s/(\d+)/digitersatz/g;
    # 35,00 ==> digitersatz,digitersatz
    # 3.5%  ==> digitersatz.digitersatz%
    # 7:30  ==> digitersatz:digitersatzam
    # 2004  ==> digitersatz
    
    
    #remove post-fixed punctuation
    $_ =~ s/(.)([.,:;?!])(\s)/$1 $3/g;
    
    
    #insert </s> to mark end-of-sentence
    $_ =~ s/([\n\r])$/ <\/s>$1/; 
    
    #insert context cue <s> to mark sentence start,
    #convert everything to lower case, and print
    print "<s> ", lc($_);
}
