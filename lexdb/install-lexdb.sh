# Copyright (c) 2004 
# Benjamin Waldron;
# see `licence.txt' for conditions.

if [ -z "$2" ]; then 
    echo usage: $0 DBNAME FIELDS-FILE
    exit
fi

export LEXDB=$1

function drop_lexdb {
    echo "dropping lexdb..."
    dropdb -U lexdb $LEXDB;
    echo "ABORTING..."
    exit;
}  

createdb -U lexdb $LEXDB
if [ $? != 0 ] ; then exit; fi

createlang -U postgres plpgsql $LEXDB
if [ $? != 0 ] ; then drop_lexdb; fi

if [ ! -f su-setup.sql ]; then echo "cannot find file su-setup.sql"; drop_lexdb; fi
psql -f su-setup.sql -U postgres $LEXDB
if [ $? != 0 ] ; then drop_lexdb; fi

if [ ! -f load.sql ]; then echo "cannot find file load.sql"; drop_lexdb; fi
psql -f load.sql -U lexdb $LEXDB
if [ $? != 0 ] ; then drop_lexdb; fi

if [ -f $2 ]; then 
    echo "taking field defns from file $2";
    psql -c 'delete from fields' -U lexdb $LEXDB; 
    if [ $? != 0 ] ; then drop_lexdb; fi

    psql -c "\copy fields from $2" -U lexdb $LEXDB; 
    if [ $? != 0 ] ; then drop_lexdb; fi

else
    echo "cannot find file $2";
    for x in `seq 1 10`; do echo wait $x; done  
    drop_lexdb;
fi

if [ ! -f init.sql ]; then echo "cannot find file init.sql"; drop_lexdb; fi
psql -f init.sql -U lexdb $LEXDB
if [ $? != 0 ] ; then drop_lexdb; fi


