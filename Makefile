ROOT = /lingo/build/
SROOT = ${HOME}/class/src
LROOT = ${HOME}/src/delphin
WROOT = e:
WACLROOT = c:/program\ files/acl70
WLOGIN = ${LINGO_LOGIN}
DATE = `date "+%Y-%m-%d"`
TARGET = /lingo/www/lingo/ftp
 
LINKS = lkb_data.tgz lkb_linux.x86.32.tgz lkb_solaris.tgz \
        lkb_source.tgz lkb_windows.tgz lkb_windows.zip \
        itsdb_data.tgz itsdb_documentation.tgz itsdb_libraries.tgz \
        itsdb_linux.x86.32.tgz itsdb_solaris.tgz itsdb_source.tgz \
        erg.tgz matrix.tgz spanish.tgz

CP=cp
RM=rm
MKDIR=mkdir
CHGRP=chgrp
CHMOD=chmod
LN=ln -s
CVS=cvs -q -z9
TAR=tar
MAKE=make
TEE=tee

update:
	( \
	  cd ${ROOT}/yzlui; \
	  ${CVS} update -P -d -R; \
	  ${CVS} rtag -F latest yzlui; \
	  cd ${ROOT}/erg; \
	  ${CVS} update -P -d -R; \
	  ${CVS} rtag -F latest erg; \
	  cd ${ROOT}/matrix; \
	  ${CVS} update -P -d -R; \
	  ${CVS} rtag -F latest matrix; \
	  cd ${ROOT}/spanish; \
	  ${CVS} update -P -d -R; \
	  ${CVS} rtag -F latest spanish; \
	  cd ${ROOT}/lkb; \
	  ${CVS} update -P -d -R; \
	  ${CVS} commit -f -m "auto-update for build" ./src/version.lsp; \
	  ${CVS} rtag -F latest lkb; \
	  $(MAKE) all; \
	) 2>&1 | ${TEE} ${ROOT}/lkb/log/build
	${CP} ${ROOT}/lkb/src/lkb.el ${TARGET}/etc
	${CP} ${ROOT}/lkb/src/tdl-mode.el ${TARGET}/etc
	${CP} ${ROOT}/lkb/etc/dot.emacs ${TARGET}/etc
	${CP} ${ROOT}/lkb/etc/lkb ${TARGET}/etc
	${CP} ${ROOT}/lkb/etc/install ${TARGET}/etc
	( \
	  cd ${ROOT}/lkb/log; \
          mail -s "automated LKB build (${DATE})" \
	    oe@ifi.uio.no \
	    ben.waldron@hf.ntnu.no \
            < build; \
	  cvs commit -m "" build; \
	)

latest:
	${CVS} update -P -d -R -r latest;

all: lkb yzlui erg spanish itsdb # matrix

windows: lkb_windows

solaris: lkb_solaris itsdb_solaris

linux_x86_64: lkb_linux@ar itsdb_linux_x86_64

#
# link structure on CSLI LinGO ftp(1) site
#
links: 
	( \
	  cd ${TARGET}; \
	  for i in ${LINKS}; do \
	    ${RM} $${i}; \
	    ${LN} ./${DATE}/$${i}; \
	  done \
	)

#
# LKB grammar development environment
#

lkb: lkb_source lkb_data lkb_binaries
	${RM} ${TARGET}/test;
	${LN} ${TARGET}/builds/${DATE} ${TARGET}/test;

lkb_source:
	( \
	  cd ${ROOT}/lkb; \
	  if [ ! -d ${TARGET}/builds/${DATE} ]; then \
            ${MKDIR} ${TARGET}/builds/${DATE}; \
          fi; \
	  ${CHMOD} 3775 ${TARGET}/builds/${DATE}; \
	  ${CHGRP} build ${TARGET}/builds/${DATE}; \
	  ${TAR} Svczf ${TARGET}/builds/${DATE}/lkb_source.tgz \
	      --exclude=Makefile \
	      --exclude="*~" --exclude="CVS*" --exclude="*/CVS*" \
	      --exclude=".nfs*" --exclude=".#*" --exclude="#*#"\
	      --exclude="src/.????*" --exclude="src/fasl*" \
	      --exclude="*.fasl" --exclude="./lexdb*" \
	      --exclude="src/doc*" --exclude="src/data*" \
	      --exclude="linux*" --exclude="solaris*" --exclude="windows*" \
	      --exclude="bin*" --exclude="include*" --exclude="lib*" \
	      --exclude="man*" --exclude="doc*" --exclude="log*" \
	      --exclude="etc*" --exclude=src/general/itsdb.lisp \
	      --exclude="src/tsdb*" --exclude="src/pvm*" \
	      --exclude=src/systems/tsdb.system \
	      --exclude=src/systems/pvm.system \
	      .; \
	)

lkb_data:
	( \
	  cd ${ROOT}/lkb; \
	  ${TAR} Svczf ${TARGET}/builds/${DATE}/lkb_data.tgz \
	      --exclude="*~" --exclude="CVS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" --exclude="#*#"\
	      --exclude="src/data/spanish*" \
	      --exclude="src/data/interrogatives*" \
	      --exclude="src/data/aline2*" \
	      src/data src/*.el lexdb etc; \
	)

lkb_binaries: lkb_linux lkb_macos # lkb_solaris

lkb_linux: lkb_linux_x86_32

lkb_linux_x86_32:
	${RM} -f ${ROOT}/.yes;
	( cd ${ROOT}/lkb && ${MAKE} lkb_linux@cypriot; )
	( \
	  if [ ! -f ${ROOT}/.yes ]; then exit 1; fi; \
	  cd ${ROOT}/lkb; \
	  ${TAR} Svczf ${TARGET}/builds/${DATE}/lkb_linux.x86.32.tgz \
              --exclude=".nfs*" \
	      linux.x86.32 bin/linux.x86.32/yzlui; \
	)
	  
lkb_linux@cypriot:
	( \
	  echo "(load \"${ROOT}/lkb/src/general/loadup.lisp\")"; \
	  echo "(load \"${ROOT}/lkb/src/ACL_specific/deliver.lsp\")"; \
	  echo "(pushnew :lkb *features*)"; \
	  echo "(pushnew :psql *features*)"; \
	  echo "(pushnew :lui *features*)"; \
	  echo "(pushnew :mrs *features*)"; \
	  echo "(pushnew :mt *features*)"; \
	  echo "(setf make::*building-image-p* t)"; \
	  echo "(setf (system:getenv \"DISPLAY\") nil)"; \
	  echo "(compile-system \"tsdb\" :force t)"; \
	  echo "(excl:exit)"; \
	) | ( ACL_LOCALE=C \
              LD_LIBRARY_PATH=${ROOT}/lkb/lib/linux.x86.32 \
                cd /lingo/local/acl; \
                alisp -I clim -qq && touch ${ROOT}/.yes; )

lkb_linux@ar:
	${RM} -f ${LROOT}/.yes;
	( cd ${LROOT}/lkb && ${MAKE} latest; )
	( \
	  echo "(load \"${LROOT}/lkb/src/general/loadup.lisp\")"; \
	  echo "(load \"${LROOT}/lkb/src/ACL_specific/deliver.lsp\")"; \
	  echo "(pushnew :lkb *features*)"; \
	  echo "(pushnew :psql *features*)"; \
	  echo "(pushnew :lui *features*)"; \
	  echo "(pushnew :mrs *features*)"; \
	  echo "(pushnew :mt *features*)"; \
	  echo "(setf make::*building-image-p* t)"; \
	  echo "(setf (system:getenv \"DISPLAY\") nil)"; \
	  echo "(compile-system \"tsdb\" :force t)"; \
	  echo "(excl:exit)"; \
	) | ( cd /logon/oe/src/logon/franz/linux.x86.64; \
              ACL_LOCALE=C ./alisp -I clim -qq && touch ${LROOT}/.yes; )
	( \
	  if [ ! -f ${LROOT}/.yes ]; then exit 1; fi; \
	  cd ${LROOT}/lkb; \
	  ${TAR} Svczf /tmp/lkb_linux.x86.64.tgz \
              --exclude=".nfs*" \
	      linux.x86.64 bin/linux.x86.64/yzlui; \
	  scp /tmp/lkb_linux.x86.64.tgz \
            oe@lingo.stanford.edu:${TARGET}/test; \
	)

lkb_solaris:
	${RM} -f ${HOME}/tmp/.yes;
	( cd ${SROOT}/lkb && ${MAKE} latest; )
	( \
	  echo "(load \"${SROOT}/lkb/src/general/loadup.lisp\")"; \
	  echo "(load \"${SROOT}/lkb/src/ACL_specific/deliver.lsp\")"; \
	  echo "(pushnew :lkb *features*)"; \
	  echo "(pushnew :lui *features*)"; \
	  echo "(pushnew :mrs *features*)"; \
	  echo "(setf make::*building-image-p* t)"; \
	  echo "(setf (system:getenv \"DISPLAY\") nil)"; \
	  echo "(compile-system \"tsdb\" :force t)"; \
	  echo "(excl:exit)"; \
	) | ( cd ${SROOT}/acl; ./clim -qq && touch ${HOME}/tmp/.yes; )
	( \
	  if [ ! -f ${HOME}/tmp/.yes ]; then exit 1; fi; \
	  cd ${SROOT}/lkb; \
	  ${TAR} Svczf /tmp/lkb_solaris.tgz \
              --exclude=".nfs*" \
	      solaris; \
	  /usr/pubsw/bin/scp /tmp/lkb_solaris.tgz \
            oe@lingo:${TARGET}/test; \
	)

lkb_macos: lkb_macos_ppc_32

lkb_macos_ppc_32:
	( \
	  cd ${ROOT}/lkb; \
	  ${TAR} Svczf ${TARGET}/builds/${DATE}/lkb_macos.ppc.32.tgz \
              --exclude=".nfs*" \
	      bin/macos.ppc.32/yzlui.app; \
	)

lkb_windows_clean:
	${RM} -f ${WROOT}/.yes;
	${RM} -f c:/tmp/build.lisp
	${RM} -f /cygdrive/c/tmp/lkb_windows.tgz
	${RM} -f /cygdrive/c/tmp/lkb_windows.zip

lkb_windows: lkb_windows_clean
	( cd ${WROOT}/lkb && ${MAKE} latest; )
	( \
	  echo "(load \"${WROOT}/lkb/src/general/loadup.lisp\")"; \
	  echo "(setf *default-pathname-defaults* "; \
          echo "  (translate-logical-pathname \"sys:\"))"; \
	  echo "(excl:chdir *default-pathname-defaults*)"; \
	  echo "(load \"${WROOT}/lkb/src/ACL_specific/deliver.lsp\")"; \
          echo "(excl:exit)"; \
	) > c:/tmp/build.lisp
	( cd ${WACLROOT}; ./clim.exe -qq -L c:/tmp/build.lisp \
          && touch ${WROOT}/.yes; )
	( \
	  if [ ! -f ${WROOT}/.yes ]; then exit 1; fi; \
	  cd ${WROOT}/lkb; \
	  ${TAR} Svczf /cygdrive/c/tmp/lkb_windows.tgz \
              --exclude=".nfs*" \
	      windows; \
	  ${RM} -f /cygdrive/c/tmp/lkb_windows.zip; \
          zip -r /cygdrive/c/tmp/lkb_windows.zip windows; )
	scp /cygdrive/c/tmp/lkb_windows.tgz /cygdrive/c/tmp/lkb_windows.zip \
          ${WLOGIN}@lingo.stanford.edu:/lingo/www/lingo/ftp/test;

lkb_documentation:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/builds/${DATE}/lkb_documentation.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      doc/lkb.pdf; \
	)

#
# YZ implementation of Linguistic User Interface (Woodley Packard)
#

yzlui:
	( \
	  cd ${ROOT}; \
	  ${TAR} Svczf ${TARGET}/builds/${DATE}/yzlui.tgz \
	      --exclude="*~" --exclude="CVS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" --exclude="#*#"\
	      yzlui; \
	)

#
# LinGO English Resource Grammar (Dan Flickinger)
#

erg:
	( \
	  cd ${ROOT}; \
	  ${TAR} Svczf ${TARGET}/builds/${DATE}/erg.tgz \
	      --exclude="*~" --exclude="CVS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" --exclude="#*#"\
	      --exclude="tsdb*" --exclude="*.fasl" \
	      erg; \
	)


#
# MatriX Grammar Starter-Kit (Emily M. Bender et al.)
#

matrix:
	( \
	  cd ${ROOT}; \
	  ${TAR} Svczf ${TARGET}/builds/${DATE}/matrix.tgz \
	      --exclude="*~" --exclude="CVS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" --exclude="#*#"\
	      --exclude="matrix/doc*" --exclude="*.fasl" \
	      matrix; \
	)

#
# Spanish HPSG (Ana Paula Quirino Simoes)
#

spanish:
	( \
	  cd ${ROOT}; \
	  ${TAR} Svczf ${TARGET}/builds/${DATE}/spanish.tgz \
	      --exclude="*~" --exclude="CVS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" --exclude="#*#"\
	      --exclude="pet*" --exclude="tsdb*" \
	      --exclude="*.fasl" \
	      spanish; \
	)

#
# [incr tsdb()]
#

itsdb: itsdb_binaries itsdb_libraries itsdb_source itsdb_capi itsdb_tsdb \
       itsdb_data itsdb_documentation

itsdb_binaries: itsdb_linux # itsdb_solaris

itsdb_linux: itsdb_linux_x86_32

itsdb_linux_x86_32:
	( \
	  cd ${ROOT}/lkb; \
	  find src/.l8cl -type f -exec touch {} \; ; \
	  tar Svczf ${TARGET}/builds/${DATE}/itsdb_linux.x86.32.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      bin/linux.x86.32/tsdb bin/linux.x86.32/swish++ \
	      bin/linux.x86.32/pvmd3 bin/linux.x86.32/pvm \
	      src/pvm/linux.x86.32/*.so src/tsdb/linux.x86.32/*.so \
	      src/.l8cl/pvm src/.l8cl/tsdb src/.l8cl/fad; \
	)

itsdb_linux_x86_64:
	( \
	  cd ${LROOT}/lkb; \
	  find src/.l8c4 -type f -exec touch {} \; ; \
	  tar Svczf /tmp/itsdb_linux.x86.64.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      bin/linux.x86.64/tsdb bin/linux.x86.64/swish++ \
	      bin/linux.x86.64/pvmd3 bin/linux.x86.64/pvm \
	      src/pvm/linux.x86.64/*.so src/tsdb/linux.x86.64/*.so \
	      src/.l8c4/pvm src/.l8c4/tsdb src/.l8c4/fad; \
	  scp /tmp/itsdb_linux.x86.64.tgz \
	    oe@lingo.stanford.edu:${TARGET}/test \
	)

itsdb_solaris:
	( \
	  cd ${SROOT}/lkb; \
	  find src/.s7cl -type f -exec touch {} \; ; \
	  tar Svczf /tmp/itsdb_solaris.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      bin/solaris/tsdb bin/solaris/swish++ \
	      bin/solaris/pvmd3 bin/solaris/pvm \
	      src/pvm/solaris/*.so src/tsdb/solaris/*.so \
	      src/.s7cl/pvm src/.s7cl/tsdb src/.s7cl/fad; \
	  /usr/pubsw/bin/scp /tmp/itsdb_solaris.tgz \
	    oe@lingo.stanford.edu:${TARGET}/test; \
	)

itsdb_libraries:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/builds/${DATE}/itsdb_libraries.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" \
	      etc include lib man src/general/itsdb.lisp; \
	)

itsdb_source:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/builds/${DATE}/itsdb_source.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" \
	      src/systems/tsdb.system src/systems/pvm.system \
	      src/systems/fad.system  \
	      src/fad src/pvm/*.lisp \
	      src/tsdb/lisp src/tsdb/tcl \
              src/tsdb/ToDo src/tsdb/Registration \
	      src/tsdb/TeX; \
	)

itsdb_capi:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/builds/${DATE}/itsdb_capi.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" \
	      src/tsdb/capi; \
	)

itsdb_tsdb:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/builds/${DATE}/itsdb_tsdb.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" \
	      src/tsdb/c; \
	)

itsdb_data:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/builds/${DATE}/itsdb_data.tgz \
	      --exclude=src/tsdb/skeletons/english/vm97 \
	      --exclude=src/tsdb/skeletons/english/vm97p \
	      --exclude=src/tsdb/skeletons/english/vm98 \
	      --exclude=src/tsdb/skeletons/english/wsj00 \
	      --exclude=src/tsdb/skeletons/english/parc \
	      --exclude=src/tsdb/skeletons/yy \
	      --exclude=src/tsdb/home/redwoods --exclude=src/tsdb/home/log \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      src/tsdb/skeletons src/tsdb/home; \
	)

itsdb_documentation:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/builds/${DATE}/itsdb_documentation.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      doc/itsdb.ps doc/tsnlp.ps doc/profiling.ps doc/parsing.ps; \
	)


itsdb_trees: itsdb_vm32

itsdb_vm32:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/builds/${DATE}/itsdb_vm32.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      src/tsdb/home/trees/vm32; \
	)
