#!/bin/sh

 ( \
   echo "(load \"$1/src/general/loadup.lisp\")"; \
   echo "(pushnew :pxml *features*)"; \
   echo "(compile-system \"rmrs\" :force t)"; \
   echo "(in-package :mrs)";\
   echo "(load \"$1/src/rmrs/rasp3/standalone-server.lsp\")"; \
   echo "(load \"$1/src/rmrs/rasp3/standalone-image.lsp\")"; \
   echo "(excl:exit)"; \
 ) | ( ACL_LOCALE=C  /usr/opt/acl80.64/alisp -I /usr/opt/acl80.64/alisp.dxl -qq; )

mv $1/rasp3-rmrs/libacli8010.so $1/rasp3-rmrs/libacli8010.so_orig;
ln -s /usr/opt/acl80.64/libacli8010.so $1/rasp3-rmrs;



