#
# make sure this file is included from your personal `~/.bashrc', e.g. put the
# following towards the end of `~/.bashrc' (and uncomment these, of course):
#
#   DELPHINHOME=~/delphin
#   if [ -f ${DELPHINHOME}/dot.bashrc ]; then
#     . ${DELPHINHOME}/dot.bashrc
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
export DELPHINHOME

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
    LD_LIBRARY_PATH=${DELPHINHOME}/lkb/lib/${os}
  else
    LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${DELPHINHOME}/lkb/lib/${os}
  fi
  export LD_LIBRARY_PATH
fi

#
# make sure there is a directory `~/tmp' for the LKB to store temporary data;
# also, make sure to install a couple of dummy `dot' files, unless they exist
# already.
#
/bin/mkdir $HOME/tmp > /dev/null 2>&1

#
# request emacs(1) as the standard editor, e.g. for CVS comments.
#
export EDITOR=emacs
