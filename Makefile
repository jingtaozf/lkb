ROOT = /eo/e7/apache/htdocs/src
DATE=`/bin/date "+%Y-%m-%d"`
TARGET=/usr/local/apache/htdocs/lingo/ftp

RM=/bin/rm
MKDIR=/bin/mkdir
CVS=/usr/pubsw/bin/cvs
TAR=/usr/local/bin/tar
MAKE=/usr/ccs/bin/make

update:
	( \
	  cd ${ROOT}/lkb; \
	  ${CVS} update -P -d -R; \
	  ${MAKE} lkb; \
	)

lkb: source data binaries grammar

source:
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
	      --exclude=bin* --exclude=include* --exclude=lib* \
	      --exclude=src/tsdb* --exclude=src/pvm* \
	      --exclude=src/systems/tsdb.system \
	      --exclude=src/systems/pvm.system \
	      .; \
	)

data:
	( \
	  cd ${ROOT}/lkb; \
	  ${TAR} Svczf ${TARGET}/${DATE}/lkb_data.tgz \
	      --exclude="*~" --exclude="CVS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" --exclude="#*#"\
	      --exclude="src/data/spanish*" \
	      src/data; \
	)

binaries: linux linux solaris windows

linux: linux_om linux_ml

linux_om:
	${RM} -f ${ROOT}/.yes;
	rsh lineara "cd ${ROOT}/lkb && make linux_om@lineara";
	( \
	  if [ ! -f ${ROOT}/.yes ]; then exit 1; fi; \
	  cd ${ROOT}/lkb; \
	  ${TAR} Svczf ${TARGET}/${DATE}/lkb_linux_om.tgz \
              --exclude=".nfs*" \
	      linux; \
	)
	  
linux_om@lineara:
	( \
	  echo "(load \"${ROOT}/lkb/src/general/loadup.lisp\")"; \
	  echo "(load \"${ROOT}/lkb/src/ACL_specific/deliver.lsp\")"; \
	  echo "(excl:exit)"; \
	) | ( cd /usr/local/nacl; ./clim -qq && touch ${ROOT}/.yes; )

linux_ml:
	${RM} -f ${ROOT}/.yes;
	rsh lineara "cd ${ROOT}/lkb && make linux_ml@lineara;
	( \
	  if [ ! -f ${ROOT}/.yes ]; then exit 1; fi; \
	  cd ${ROOT}/lkb; \
	  ${TAR} Svczf ${TARGET}/${DATE}/lkb_linux_ml.tgz \
              --exclude=".nfs*" \
	      linux; \
	)
	  
linux_ml@lineara:
	( \
	  echo "(load \"${ROOT}/lkb/src/general/loadup.lisp\")"; \
	  echo "(load \"${ROOT}/lkb/src/ACL_specific/deliver.lsp\")"; \
	  echo "(excl:exit)"; \
	) | ( cd /usr/local/macl; ./clim -qq && touch ${ROOT}/.yes; )

solaris:
	${RM} -f ${ROOT}/.yes;
	( \
	  echo "(load \"${ROOT}/lkb/src/general/loadup.lisp\")"; \
	  echo "(load \"${ROOT}/lkb/src/ACL_specific/deliver.lsp\")"; \
	  echo "(excl:exit)"; \
	) | ( cd ${ROOT}/acl; ./clim -qq && touch ${ROOT}/.yes; )
	( \
	  if [ ! -f ${ROOT}/.yes ]; then exit 1; fi; \
	  cd ${ROOT}/lkb; \
	  ${TAR} Svczf ${TARGET}/${DATE}/lkb_solaris.tgz \
              --exclude=".nfs*" \
	      solaris; \
	)

windows:

grammar:
	( \
	  cd ${ROOT}/grammar; \
	  ${TAR} Svczf ${TARGET}/${DATE}/erg.tgz \
	      --exclude="*~" --exclude="CVS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" --exclude="#*#"\
	      --exclude="pet*" --exclude="tsdb*" \
	      --exclude="*.fasl" \
	      .; \
	)
