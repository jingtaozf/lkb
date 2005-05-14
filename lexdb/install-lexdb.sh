# Copyright (c) 2004 - 2005 
# Benjamin Waldron;
# see `licence.txt' for conditions.

## script usage
if [ -z "$2" ]; then 
    echo usage: $0 DBNAME FLD-FILE [CREATEDB-OPTIONS]
    exit
fi

## grab script parameters
export LEXDB=$1
export FLD_FILE=$2
export CREATEDB_OPTIONS=$3

function abort {
    echo "ABORTING..."
    exit;
}  

## default settings
if [ -n "$CREATEDB_OPTIONS" ]; then 
    export CREATEDB_OPTIONS="-E UNICODE"; 
    echo 'using default CREATEDB_OPTIONS="-E UNICODE"';
fi

## check for files
if [ ! -f $FLD_FILE ]; then
    echo "cannot find file $FLD_FILE";
    #for x in `seq 1 10`; do echo wait $x; done  
    abort;
fi
if [ ! -f su-setup.sql ]; then
    echo "cannot find file $PWD/su-setup.sql"; abort; 
fi
if [ ! -f load.sql ]; then 
    echo "cannot find file $PWD/load.sql"; abort; 
fi
if [ ! -f init.sql ]; then 
    echo "cannot find file $PWD/init.sql"; abort; 
fi

## create PSQL DB to hold LexDB
echo "createdb $CREATEDB_OPTIONS -U lexdb $LEXDB"
createdb $CREATEDB_OPTIONS -U lexdb $LEXDB
if [ $? != 0 ] ; then abort; fi

## ensure plpgsql DB language is available
echo "createlang -U postgres plpgsql $LEXDB"
createlang -U postgres plpgsql $LEXDB
#if [ $? != 0 ] ; then abort; fi

## load DB superuser setup script
echo psql -f su-setup.sql -U postgres $LEXDB
psql -f su-setup.sql -U postgres $LEXDB
if [ $? != 0 ] ; then abort; fi

## load 'lexdb' DB user setup script (part 1)
echo "psql -f load.sql -U lexdb $LEXDB"
psql -f load.sql -U lexdb $LEXDB
if [ $? != 0 ] ; then abort; fi

## load field definitions
echo "taking field defns from file $FLD_FILE";
echo "psql -c 'delete from public.fld' -U lexdb $LEXDB"
psql -c 'delete from fld' -U lexdb $LEXDB; 
if [ $? != 0 ] ; then abort; fi

echo "psql -c \"copy public.fld from $FLD_FILE\" -U lexdb $LEXDB"
psql -c "\copy fld from $FLD_FILE" -U lexdb $LEXDB; 
if [ $? != 0 ] ; then abort; fi

## load 'lexdb' DB user setup script (part 2)
echo "psql -f init.sql -U lexdb $LEXDB"
psql -f init.sql -U lexdb $LEXDB
if [ $? != 0 ] ; then abort; fi

