ROOT = /eo/e7/apache/htdocs/src
TARGET=/usr/local/apache/htdocs/lingo/ftp
CVS=/usr/pubsw/bin/cvs

release: source binaries grammar

binaries: linux_om linux_ml solaris windows

source:
	( \
	  cd ${ROOT}/lkb; \
	  ${CVS} update -P -d -R; \
	  tar Svczf ${TARGET}/lkb_source.tgz \
	      --exclude=Makefile
	      --exclude="*~" --exclude="CVS*" --exclude="*/CVS*" \
              --exclude=".nfs*" --exclude=".#*" --exclude="#*#"\
	      --exclude="src/.??sl*" \
	      --exclude="src/www*" --exclude=src/systems/www.system \
	      --exclude=bin* --exclude=include* --exclude=lib* \
	      --exclude=src/tsdb* --exclude=src/pvm* \
	      --exclude=src/systems/tsdb.system \
	      --exclude=src/systems/pvm.system \
	      .; \
	)

linux_om:
	rm touch ${ROOT}/.yes;
	rsh lineara "cd ${ROOT} && make linux_om@lineara;
	( \
	  cd ${ROOT}; \
	  tar Svczf ${TARGET}/lkb_linux_om.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      linux; \
	)
	  
linux_om@lineara:
	( \
	  echo "(load \"${ROOT}/src/general/loadup.lisp\")"; \
	  echo "(load \"${ROOT}/src/ACL_specific/deliver.lsp\")"; \
	  echo "(excl:exit)"; \
	) | ( cd /usr/local/nacl ; ./clim -qq && touch ${ROOT}/.yes; )

linux_ml:
	rm touch ${ROOT}/.yes;
	rsh lineara "cd /home/oe/src/lkb && make linux_ml@lineara;
	( \
	  cd ${ROOT}; \
	  tar Svczf ${TARGET}/lkb_linux_ml.tgz \
	      --exclude="*~" --exclude="*/RCS*" --exclude="*/CVS*" \
              --exclude=".nfs*" \
	      linux; \
	)
	  
linux_ml@lineara:
	( \
	  echo "(load \"${ROOT}/src/general/loadup.lisp\")"; \
	  echo "(load \"${ROOT}/src/ACL_specific/deliver.lsp\")"; \
	  echo "(excl:exit)"; \
	) | ( cd /usr/local/macl ; ./clim -qq  && touch ${ROOT}/.yes; )

solaris:

windows:

grammar:
