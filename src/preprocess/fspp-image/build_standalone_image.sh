#!/bin/sh

 ( \
   echo "(load \"$1/src/general/loadup.lisp\")"; \
   echo "(load \"$1/src/preprocess/fspp-image/standalone-image.lsp\")"; \
   echo "(excl:exit)"; \
 ) | ( ACL_LOCALE=C  /anfs/repl/acl81.64/alisp -I /anfs/repl/acl81.64/alisp.dxl -qq; )



