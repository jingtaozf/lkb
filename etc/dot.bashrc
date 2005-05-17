#
# make sure this file is included from your personal `~/.bashrc', e.g. put the
# following towards the end of `~/.bashrc' (and uncomment these, of course):
#
#   LOGONROOT=~/logon
#   if [ -f ${LOGONROOT}/dot.bashrc ]; then
#     . ${LOGONROOT}/dot.bashrc
#   fi
#
# if you decide to keep your DELPH-IN source tree in a different location, say
# `~/src/delphin', instead, then adjust the above accordingly.  it should not 
# be necessary to make changes to _this_ file, however.
#

# -----------------------------------------------------------------------------
#        file: .bashrc
#      module: DELPH-IN initialization file for BaSH
#     version: 0.0 -- 18-may-05
#  written by: oe, uio
# last update: 
#  updated by: 
# -----------------------------------------------------------------------------
# 
# -----------------------------------------------------------------------------

#
# make global variable for DELPH-IN tree accessible to other processes.
#
export LOGONROOT

#
# first, work out the current operating system, one of `linux' (x86), `solaris'
# (sparc), or `windows' (x86); anything else will require manual installation.
#
if [ "$OSTYPE" = "linux" -o "$OSTYPE" = "linux-gnu" ]; then
  #
  # apparently, (some) Debian installations come with an older uname(1), where
  # `-i' is not available :-{.                                 (30-apr-05; oe)
  #
  if uname -i > /dev/null 2>&1; then
    cpu=$(uname -i)
    if [ "${cpu}" = "unknown" ]; then cpu=$(uname -m); fi
  else
    cpu=$(uname -m)
  fi
  case "${cpu}" in
    i?86)
      os="linux.x86.32"
      ;;
    x86_64)
      os="linux.x86.64"
      ;;
    *)
      echo "dot.bashrc: unknown Linux variant (check \`uname -i')."
      os="linux.x86.32";
  esac
elif [ "$OSTYPE" = "solaris" -o "${OSTYPE%%?.?}" = "solaris" ]; then
  os="solaris";
elif [ "$OSTYPE" = "cygwin" ]; then
  os="windows";
fi

#
# even on systems that have no Motif installed, make sure the LKB can find its
# Motif libraries (this is somewhat of a hack, really).  also, add the path for
# XLE Sicstus libraries, so dynamic loading will work.
#
if [ "$OSTYPE" = "linux" -o "$OSTYPE" = "linux-gnu" ]; then
  if [ -z "${LD_LIBRARY_PATH}" ]; then
    LD_LIBRARY_PATH=${LOGONROOT}/lingo/lkb/lib/${os}
  else
    LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${LOGONROOT}/lingo/lkb/lib/linux
  fi
  LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${LOGONROOT}/parc/xle/lib
  export LD_LIBRARY_PATH
fi

#
# make sure there is a directory `~/tmp' for the LKB to store temporary data;
# also, make sure to install a couple of dummy `dot' files, unless they exist
# already.
#
/bin/mkdir $HOME/tmp > /dev/null 2>&1
if [ ! -f ${HOME}/.cvsrc ]; then
  /bin/cp $LOGONROOT/dot.cvsrc ${HOME}/.cvsrc;
fi
if [ ! -f ${HOME}/.pvm_hosts ]; then
  /bin/cp $LOGONROOT/dot.pvm_hosts ${HOME}/.pvm_hosts;
fi
if [ ! -f ${HOME}/.tsdbrc ]; then
  /bin/cp $LOGONROOT/dot.tsdbrc ${HOME}/.tsdbrc;
fi

#
# for ChaSen, we need to expand $LOGONROOT when we create a user-specific file;
# we will have to hope people do not move around their LOGON trees ...
#
if [ ! -f ${HOME}/.chasenrc ]; then
  sed "s@_LOGONROOT_@$LOGONROOT@" $LOGONROOT/dot.chasenrc > ${HOME}/.chasenrc;
fi

#
# even though most LOGON users go through :pserver:, set this propertly too.
#
export CVS_RSH=ssh

#
# request emacs(1) as the standard editor, e.g. for CVS comments.
#
export EDITOR=emacs

#
# by default, use standard cvs(1) binary; developers who manipulate more than
# a single CVS repository (e.g. paul and oe) may set this to `lvs' instead.
#
if [ -z "${CVS}" ]; then
  export CVS=cvs
fi

#
# make some of the `gold' profiles visible in the default [incr tsdb()] home
#
{
  mkdir ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold;
  if [ -a ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold/norgram ]; then
    if [ -h ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold/norgram ]; then
      rm ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold/norgram;
      ln -s ${LOGONROOT}/parc/pargram/norwegian/bokmal/gold \
        ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold/norgram;
    fi
  else
    ln -s ${LOGONROOT}/parc/pargram/norwegian/bokmal/gold \
      ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold/norgram;
  fi
  if [ -a ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold/erg ]; then
    :
  else
    ln -s ${LOGONROOT}/lingo/erg/gold \
      ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold/erg;
  fi
  if [ -a ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold/logon ]; then
    :
  else
    ln -s ${LOGONROOT}/gold ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold/logon;
  fi
  if [ -a ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold/noen ]; then
    if [ -h ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold/noen ]; then
      rm ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold/noen;
      ln -s ${LOGONROOT}/ntnu/noen/gold \
        ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold/noen;
    fi
  else
    ln -s ${LOGONROOT}/ntnu/noen/gold \
      ${LOGONROOT}/lingo/lkb/src/tsdb/home/gold/noen;
  fi
} > /dev/null 2>&1
/bin/chmod 755 $HOME
/bin/mkdir $HOME/tmp > /dev/null 2>&1

export CVSROOT=/home/oe/CVSROOT
if [ ! -d $CVSROOT ]; then
  export CVSROOT=glahn.hf.ntnu.no:$CVSROOT
  export CVS_RSH=ssh
fi

alias lkbdoc="/usr/local/bin/acroread -geometry 930x600 \
                /home/lingo/lkb/doc/lkb.pdf &"
