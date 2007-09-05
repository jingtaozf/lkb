#!/bin/sh

## Modify as necessary:
## lkb_home=/home/bmw20/lkb;
lkb_home=/home/cr351/work/lkb

tmp=/tmp/$USER;

rmrs_gram_file=$lkb_home/src/rmrs/rasp3/gram15.rmrs;
rmrs_tag_file=$lkb_home/src/rmrs/rasp3/lex15.rmrs;

## Use an exec command, so that no child process is created and Java can clean
## up more easily: killing the shell kills the LKB image.

if [ -e $lkb_home/rasp3-rmrs/rasp3-rmrs ]
then
    exec $lkb_home/rasp3-rmrs/rasp3-rmrs -e "(mrs::init-rasp-server \"$rmrs_gram_file\" \"$rmrs_tag_file\")" -e "(mrs::run-rasp-server)";
else
    echo "First run \"$lkb_home/src/rmrs/rasp3/build_standalone_image.sh $lkb_home\" !"
fi

