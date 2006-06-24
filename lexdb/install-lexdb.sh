# Copyright (c) 2004 - 2005 
# Benjamin Waldron;
# see `licence.txt' for conditions.

## script usage
if [ -z "$3" ]; then 
    echo "usage: $0 DBNAME FLD-FILE DFN-FILE [CREATEDB-OPTIONS]"
    exit
fi

## grab script parameters
export LEXDB=$1
export FLD_FILE=$2
export DFN_FILE=$3
export CREATEDB_OPTIONS=$4

function abort {
    echo "ABORTING..."
    exit;
}  

## default settings
if [ -z "$CREATEDB_OPTIONS" ]; then 
    export CREATEDB_OPTIONS="-E UNICODE"; 
    echo 'using default CREATEDB_OPTIONS="-E UNICODE"';
fi

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
CMD="createdb $CREATEDB_OPTIONS -U lexdb $LEXDB"
echo $CMD; $CMD
if [ $? != 0 ] ; then abort; fi

## ensure plpgsql DB language is available
CMD="createlang -U postgres plpgsql $LEXDB"
echo $CMD; $CMD
#if [ $? != 0 ] ; then abort; fi

## load 'lexdb' DB user setup script (part 1)
CMD="psql -f load.sql -U lexdb $LEXDB"
echo $CMD; $CMD
if [ $? != 0 ] ; then abort; fi

## load field definitions
echo "taking field defns from file $FLD_FILE";

CMD="psql -c '\\copy public.fld from $FLD_FILE' -U lexdb $LEXDB"
echo $CMD; psql -c "\copy public.fld from $FLD_FILE" -U lexdb $LEXDB
if [ $? != 0 ] ; then abort; fi

## load 'lexdb' DB user setup script (part 2)
CMD="psql -f init.sql -U lexdb $LEXDB"
echo $CMD; $CMD
if [ $? != 0 ] ; then abort; fi

CMD="psql -c 'GRANT CREATE ON DATABASE $LEXDB TO PUBLIC' -U lexdb $LEXDB"
echo $CMD; psql -c "GRANT CREATE ON DATABASE $LEXDB TO PUBLIC" -U lexdb $LEXDB
if [ $? != 0 ] ; then abort; fi

CMD="psql -c '\\copy public.dfn from $DFN_FILE' -U lexdb $LEXDB"
echo $CMD; psql -c "\copy public.dfn from $DFN_FILE" -U lexdb $LEXDB
if [ $? != 0 ] ; then abort; fi


