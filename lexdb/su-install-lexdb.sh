# Copyright (c) 2004 - 2006 
# Benjamin Waldron;
# see `licence.txt' for conditions.

## script usage
if [ -z "$1" ]; then 
    echo "usage: $0 DBNAME [USERNAME] [CREATEDB-OPTIONS]"
    exit
fi

## grab script parameters
export LEXDB=$1
export USERNAME=$2
export CREATEDB_OPTIONS=$3

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
if [ ! -f su-init.sql ]; then 
    echo "cannot find file $PWD/su-init.sql"; abort; 
fi

## create PSQL DB to hold LexDB
CMD="createdb $CREATEDB_OPTIONS -U $USERNAME $LEXDB"
echo $CMD
$CMD
if [ $? != 0 ] ; then abort; fi

## ensure plpgsql DB language is available
CMD="createlang -U postgres plpgsql $LEXDB"
echo $CMD
$CMD

## load SQL init commands
CMD="psql -f su-init.sql -U $USERNAME $LEXDB"
echo $CMD
$CMD
if [ $? != 0 ] ; then abort; fi

##
echo "DONE!"
echo
echo You must now populate the database
echo "e.g. \COPY dfn FROM lexdb.dfn"
echo "     \COPY lex FROM lexdb.lex" 