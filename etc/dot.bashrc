/bin/chmod 755 $HOME
/bin/mkdir $HOME/tmp > /dev/null 2>&1

export CVSROOT=/home/oe/CVSROOT
if [ ! -d $CVSROOT ]; then
  export CVSROOT=glahn.hf.ntnu.no:$CVSROOT
  export CVS_RSH=ssh
fi

alias lkbdoc="/usr/local/bin/acroread -geometry 930x600 \
                /home/lingo/lkb/doc/lkb.pdf &"
