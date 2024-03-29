

#lkbdir=/home/bond/delphin/lkb


lkbdir=${LOGONROOT}/lingo/lkb

case ${grm} in
    jacy)
	grammardir=${LOGONROOT}/dfki/jacy;
	grammarurl=http://www.delph-in.net/jacy/ 
	ltypes=("${grammardir}/letypes.tdl")
	;;
    gg)
	grammardir=${LOGONROOT}/dfki/gg; 
	grammarurl=http://www.delph-in.net/gg/
	ltypes=("${grammardir}/le-types.tdl")
	;;
    srg)
	grammardir=${LOGONROOT}/upf/srg; 
	grammarurl=http://www.delph-in.net/srg/
	ltypes=("${grammardir}/letypes.tdl")
	;;
    erg)
	grammardir=${LOGONROOT}/lingo/erg; 
	grammarurl=http://www.delph-in.net/erg/
	ltypes=("$grammardir/letypes.tdl" "$grammardir/lextypes.tdl")
	;;
    krg)
	grammardir=${LOGONROOT}/khu/krg; 
	ltypes=("$grammardir/letypes.tdl")
	;;
    norsource)
	grammardir=${LOGONROOT}/ntnu/norsource; 
	ltypes=("$grammardir/letypes.tdl")
	;;
    *)
	echo "ltdb: grammar \`${grm}' not supported yet; exit.";
	exit 1;
	;;
esac

#ltypes=`ls -d ${grammardir}/*.tdl` ### check them all!
treebanks=`ls -d ${grammardir}/tsdb/gold/*`
now=`date --rfc-3339=date`

### Constants
LTDB_FILE="lt.db"
LINGUISTICS_FILE="linguistics.xml"
TYPES_FILE="types.xml"
LEXICON_FILE="lex.tab"
TB_FILE="result"



### I really don't want to do this!
if [ -f  $grammardir/Version.lsp ]; then
    versionfile=$grammardir/Version.lsp
else
    versionfile=$grammardir/Version.lisp
fi

version=`perl -ne 'if (/^\(defparameter\s+\*grammar-version\*\s+\"(.*)\s+\((.*)\)\"/) {print "$1_$2"}' $versionfile`
if [ -z "$version" ]; then
    echo "Don't know the version, will use 'something'"
    version=something
fi


if [ -z "${LOGONTMP}" ]; then
  export LOGONTMP=/tmp
fi
outdir=${LOGONTMP}/$version

HTML_DIR=$HOME/public_html/ltdb/$version
CGI_DIR=$HOME/public_html/cgi-bin/$version