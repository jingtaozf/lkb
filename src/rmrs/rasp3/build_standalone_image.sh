#!/bin/sh

## Modify as necessary:
acl_home=/anfs/repl/acl81.64

 ( \
   echo "(load \"$1/src/general/loadup.lisp\")"; \
   echo "(load \"$1/src/rmrs/rasp3/standalone-image.lsp\")"; \
   echo "(excl:exit)"; \
 ) | ( ACL_LOCALE=C $acl_home/alisp -I $acl_home/alisp.dxl -qq; )




