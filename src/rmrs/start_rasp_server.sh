#!/bin/sh

tmp=/tmp/$USER;
## Modify if necessary:
lkb_home=/local/scratch/$USER;
erg_home=$lkb_home/tools/parsing/erg;

echo "(read-script-file-aux \"$erg_home/lkb/script\")(load \"/tmp/$USER/test.cl\")(mrs::run-rasp-server)" | $lkb_home/lkb/linux.x86.64/lkb;
