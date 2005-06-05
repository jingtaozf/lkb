# Copyright (c) 2004 - 2005 
# Benjamin Waldron;
# see `licence.txt' for conditions.

## script usage
if [ -z "$3" ]; then 
    echo usage: $0 DBNAME FLD-FILE DFN-FILE [PG-OPTIONS CREATEDB-OPTIONS]
    exit
fi

## grab script parameters
export LEXDB=$1
export FLD_FILE=$2
export DFN_FILE=$3
export PG_OPTIONS=$4
export CREATEDB_OPTIONS=$5

## default settings
if [ -z "$CREATEDB_OPTIONS" ]; then 
    export CREATEDB_OPTIONS="-E UNICODE"; 
    echo 'using default CREATEDB_OPTIONS="-E UNICODE"';
fi
if [ -z "$PG_OPTIONS" ]; then 
    export PG_OPTIONS="-h localhost"; 
    echo 'using default PG_OPTIONS="-h localhost"';
fi

## function defns
function abort {
    echo "ABORTING..."
    exit;
}  

## check for files
if [ ! -f $FLD_FILE ]; then
    echo "cannot find file $FLD_FILE"; abort;
fi
if [ ! -f $DFN_FILE ]; then
    echo "cannot find file $DFN_FILE"; abort;
fi
if [ ! -f load.sql ]; then 
    echo "cannot find file $PWD/load.sql"; abort; 
fi
if [ ! -f init.sql ]; then 
    echo "cannot find file $PWD/init.sql"; abort; 
fi

## create PSQL DB to hold LexDB
echo "createdb $PG_OPTIONS $CREATEDB_OPTIONS -U lexdb $LEXDB"
createdb $PG_OPTIONS $CREATEDB_OPTIONS -U lexdb $LEXDB
if [ $? != 0 ] ; then abort; fi

## ensure plpgsql DB language is available
echo "createlang $PG_OPTIONS -U postgres plpgsql $LEXDB"
createlang $PG_OPTIONS  -U postgres plpgsql $LEXDB
#if [ $? != 0 ] ; then abort; fi

## load 'lexdb' DB user setup script (part 1)
echo "psql $PG_OPTIONS -f load.sql -U lexdb $LEXDB"
psql $PG_OPTIONS -f load.sql -U lexdb $LEXDB
if [ $? != 0 ] ; then abort; fi

## load field definitions
echo "taking field defns from file $FLD_FILE";

echo "psql $PG_OPTIONS -c \"copy public.fld from $FLD_FILE\" -U lexdb $LEXDB"
psql $PG_OPTIONS -c "\copy public.fld from $FLD_FILE" -U lexdb $LEXDB; 
if [ $? != 0 ] ; then abort; fi

## load 'lexdb' DB user setup script (part 2)
echo "psql $PG_OPTIONS -f init.sql -U lexdb $LEXDB"
psql $PG_OPTIONS -f init.sql -U lexdb $LEXDB
if [ $? != 0 ] ; then abort; fi

echo "psql $PG_OPTIONS -c \"copy public.dfn from $DFN_FILE\" -U lexdb $LEXDB"
psql $PG_OPTIONS -c "\copy public.dfn from $DFN_FILE" -U lexdb $LEXDB; 
if [ $? != 0 ] ; then abort; fi


