BEGIN {FS="\\t"}
// {$0=gensub(/^([^\t]*\t[^\t]*)\t[^\t]*\t(.*)/,"\\1\t\\2",$0); print}
