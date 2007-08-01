# Copyright (c) 2004 - 2007
# Ben Waldron;
# see `licence.txt' for conditions.

## script usage
if [ -z "$3" ]; then 
    echo "usage: $0 DBNAME DIRECTORY BASE-FILENAME [CREATEDB-OPTIONS]"
    exit
fi

## grab script parameters
export LEXDB=$1
export DIR=$2
export BASE=$3
export DFN_FILE=$DIR/$BASE.dfn
export FLD_FILE=$DIR/$BASE.fld
export REV_FILE=$DIR/$BASE.rev
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
if [ $? != 0 ] ; then echo "!!!"; echo "* YOU MAY HAVE AN OBSOLETE DFN FILE ($DFN_FILE)"; echo "* Try removing the first field (eg. \"ergTAB\") from each entry in the file $DFN_FILE"; echo "!!!"; echo; abort; fi

## populate rev field
echo "populating database from file $REV_FILE";

CMD="psql -c '\\copy public.rev from $REV_FILE' -U lexdb $LEXDB"
echo $CMD; psql -c "\copy public.rev from $REV_FILE" -U lexdb $LEXDB
if [ $? != 0 ] ; then echo 'WARNING: unable to populate lexical database'; fi



