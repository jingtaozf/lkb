ROOT = /eo/e7/apache/htdocs/src
WROOT = c:/src
DATE=`date "+%Y-%m-%d"`
TARGET=/usr/local/apache/htdocs/lingo/ftp

RM=rm
MKDIR=mkdir
CVS=cvs
TAR=tar
MAKE=make
TEE=tee

update:
	( \
	  cd ${ROOT}/matrix; \
	  ${CVS} update -P -d -R; \
	  cd ${ROOT}/spanish; \
	  ${CVS} update -P -d -R; \
	  cd ${ROOT}/lkb; \
	  ${CVS} update -P -d -R; \
	  ${CVS} commit -f -m "auto-update for build" \
            ${ROOT}/lkb/src/version.lsp; \
	  $(MAKE) all; \
	) 2>&1 | ${TEE} ${ROOT}/lkb/log/build
	( \
	  cd ${ROOT}/lkb/log; \
	  cvs commit -m "" build; \
	)

all: lkb erg matrix spanish itsdb

#
# LKB grammar development environment
#

lkb: lkb_source lkb_data lkb_binaries

lkb_source:
	( \
	  cd ${ROOT}/lkb; \
	  if [ ! -d ${TARGET}/${DATE} ]; then ${MKDIR} ${TARGET}/${DATE}; fi; \
	  ${TAR} Svczf ${TARGET}/${DATE}/lkb_source.tgz \
	      --exclude=Makefile \
	      --exclude="*~" --exclude="CVS*" --exclude="*/CVS*" \
	      --exclude=".nfs*" --exclude=".#*" --exclude="#*#"\
	      --exclude="src/.???l*" --exclude="src/fasl*" \
	      --exclude="*.fasl" \
	      --exclude="src/doc*" --exclude="src/data*" \
	      --exclude="linux*" --exclude="solaris*" --exclude="windows*" \
	      --exclude="src/www*" --exclude=src/systems/www.system \
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
	  ${TAR} Svczf ${TARGET}/${DATE}/lkb_data.tgz \
	      --exclude="*~" --exclude="CVS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" --exclude="#*#"\
	      --exclude="src/data/spanish*" \
	      --exclude="src/data/interrogatives*" \
	      --exclude="src/data/aline2*" \
	      src/data; \
	)

lkb_binaries: lkb_linux lkb_solaris

lkb_linux: lkb_linux_om lkb_linux_ml

lkb_linux_om:
	${RM} -f ${ROOT}/.yes;
	rsh lineara "cd ${ROOT}/lkb && make lkb_linux_om@lineara";
	( \
	  if [ ! -f ${ROOT}/.yes ]; then exit 1; fi; \
	  cd ${ROOT}/lkb; \
	  ${TAR} Svczf ${TARGET}/${DATE}/lkb_linux_om.tgz \
              --exclude=".nfs*" \
	      linux; \
	)
	  
lkb_linux_om@lineara:
	( \
	  echo "(load \"${ROOT}/lkb/src/general/loadup.lisp\")"; \
	  echo "(load \"${ROOT}/lkb/src/ACL_specific/deliver.lsp\")"; \
	  echo "(pushnew :lkb *features*)"; \
	  echo "(setf make::*building-image-p* t)"; \
	  echo "(setf (system:getenv \"DISPLAY\") nil)"; \
	  echo "(compile-system \"tsdb\" :force t)"; \
	  echo "(excl:exit)"; \
	) | ( cd /usr/local/nacl; ./clim -qq && touch ${ROOT}/.yes; )

lkb_linux_ml:
	${RM} -f ${ROOT}/.yes;
	rsh lineara "cd ${ROOT}/lkb && make lkb_linux_ml@lineara;
	( \
	  if [ ! -f ${ROOT}/.yes ]; then exit 1; fi; \
	  cd ${ROOT}/lkb; \
	  ${TAR} Svczf ${TARGET}/${DATE}/lkb_linux_ml.tgz \
              --exclude=".nfs*" \
	      linux; \
	)
	  
lkb_linux_ml@lineara:
	( \
	  echo "(load \"${ROOT}/lkb/src/general/loadup.lisp\")"; \
	  echo "(load \"${ROOT}/lkb/src/ACL_specific/deliver.lsp\")"; \
	  echo "(excl:exit)"; \
	) | ( cd /usr/local/macl; ./clim -qq && touch ${ROOT}/.yes; )

lkb_solaris:
	${RM} -f ${ROOT}/.yes;
	( \
	  echo "(load \"${ROOT}/lkb/src/general/loadup.lisp\")"; \
	  echo "(load \"${ROOT}/lkb/src/ACL_specific/deliver.lsp\")"; \
	  echo "(pushnew :lkb *features*)"; \
	  echo "(setf make::*building-image-p* t)"; \
	  echo "(setf (system:getenv \"DISPLAY\") nil)"; \
	  echo "(compile-system \"tsdb\" :force t)"; \
	  echo "(excl:exit)"; \
	) | ( cd ${ROOT}/acl; ./clim -qq && touch ${ROOT}/.yes; )
	( \
	  if [ ! -f ${ROOT}/.yes ]; then exit 1; fi; \
	  cd ${ROOT}/lkb; \
	  ${TAR} Svczf ${TARGET}/${DATE}/lkb_solaris.tgz \
              --exclude=".nfs*" \
	      solaris; \
	)

lkb_windows:
	${RM} -f ${WROOT}/.yes;
	( \
	  echo "(load \"${WROOT}/lkb/src/general/loadup.lisp\")"; \
	  echo "(load \"${WROOT}/lkb/src/ACL_specific/deliver.lsp\")"; \
          echo "(excl:exit)"; \
	) > c:/tmp/build.lisp
	( cd c:/program\ files/acl61; ./clim.exe -qq -L c:/tmp/build.lisp \
          && touch ${WROOT}/.yes; )
	( \
	  if [ ! -f ${WROOT}/.yes ]; then exit 1; fi; \
	  cd ${WROOT}/lkb; \
	  ${TAR} Svczf /c/tmp/lkb_windows.tgz \
              --exclude=".nfs*" \
	      windows; \
	  ${RM} -f /c/tmp/lkb_windows.zip; \
          zip -r /c/tmp/lkb_windows.zip windows; )
	scp /c/tmp/lkb_windows.tgz /c/tmp/lkb_windows.zip eo:${TARGET}/${DATE};

lkb_documentation:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/${DATE}/lkb_documentation.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      doc/lkb.pdf; \
	)

#
# LinGO English Resource Grammar (Dan Flickinger)
#

erg:
	( \
	  cd ${ROOT}; \
	  ${TAR} Svczf ${TARGET}/${DATE}/erg.tgz \
	      --exclude="*~" --exclude="CVS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" --exclude="#*#"\
	      --exclude="pet*" --exclude="tsdb*" \
	      --exclude="*.fasl" \
	      erg; \
	)


#
# MatriX Grammar Starter-Kit (Emily M. Bender et al.)
#

matrix:
	( \
	  cd ${ROOT}; \
	  ${TAR} Svczf ${TARGET}/${DATE}/matrix.tgz \
	      --exclude="*~" --exclude="CVS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" --exclude="#*#"\
	      --exclude="pet*" --exclude="tsdb*" \
	      --exclude="*.fasl" \
	      matrix; \
	)

#
# Spanish HPSG (Ana Paula Quirino Simoes)
#

spanish:
	( \
	  cd ${ROOT}; \
	  ${TAR} Svczf ${TARGET}/${DATE}/spanish.tgz \
	      --exclude="*~" --exclude="CVS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" --exclude="#*#"\
	      --exclude="pet*" --exclude="tsdb*" \
	      --exclude="*.fasl" \
	      spanish; \
	)

#
# [incr tsdb()]
#

itsdb: itsdb_binaries itsdb_libraries itsdb_source \
       itsdb_data itsdb_documentation

itsdb_binaries:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/${DATE}/itsdb_solaris.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      bin/solaris/tsdb bin/solaris/swish++ bin/solaris/.swish++ \
	      bin/solaris/pvmd3 bin/solaris/pvm \
	      src/pvm/solaris/*.so src/tsdb/solaris/*.so \
	      src/.sacl/pvm src/.sacl/tsdb; \
	)
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/${DATE}/itsdb_linux.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      bin/linux/tsdb bin/linux/swish++ bin/linux/.swish++ \
	      bin/linux/pvmd3 bin/linux/pvm \
	      src/pvm/linux/*.so src/tsdb/linux/*.so \
	      src/.l6cl/pvm src/.l6cl/tsdb; \
	)

itsdb_libraries:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/${DATE}/itsdb_libraries.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      etc include lib man src/general/itsdb.lisp; \
	)

itsdb_source:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/${DATE}/itsdb_source.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      src/systems/tsdb.system src/systems/pvm.system \
	      src/pvm/*.lisp \
	      src/tsdb/lisp src/tsdb/tcl \
              src/tsdb/ToDo src/tsdb/Registration \
	      src/tsdb/TeX; \
	)

itsdb_data:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/${DATE}/itsdb_data.tgz \
	      --exclude=src/tsdb/skeletons/english/vm97 \
	      --exclude=src/tsdb/skeletons/english/vm97p \
	      --exclude=src/tsdb/skeletons/english/vm98 \
	      --exclude=src/tsdb/skeletons/english/vm6 \
	      --exclude=src/tsdb/skeletons/english/vm13 \
	      --exclude=src/tsdb/skeletons/english/vm31 \
	      --exclude=src/tsdb/skeletons/english/vm32 \
	      --exclude=src/tsdb/skeletons/english/wsj00 \
	      --exclude=src/tsdb/skeletons/yy \
	      --exclude=src/tsdb/home/trees \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      src/tsdb/skeletons src/tsdb/home; \
	)

itsdb_trees: itsdb_vm32

itsdb_vm32:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/${DATE}/itsdb_vm32.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      src/tsdb/home/trees/vm32; \
	)


itsdb_documentation:
	( \
	  cd ${ROOT}/lkb; \
	  tar Svczf ${TARGET}/${DATE}/itsdb_documentation.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      doc/itsdb.ps doc/tsnlp.ps doc/profiling.ps doc/parsing.ps; \
	)
