# Copyright (c) 2004-2005 
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

## default settings
if ! [ $CREATEDB_OPTIONS ]; then 
    export CREATEDB_OPTIONS="-E UNICODE"; 
    echo 'using default CREATEDB_OPTIONS="-E UNICODE"';
fi

## called if commands below fail
function drop_lexdb {
#    echo "dropping lexdb..."
#    dropdb -U lexdb $LEXDB;
    echo "ABORTING..."
    exit;
}  

## create PSQL DB to hold LexDB
echo "createdb $CREATEDB_OPTIONS -U lexdb $LEXDB"
createdb $CREATEDB_OPTIONS -U lexdb $LEXDB
if [ $? != 0 ] ; then exit; fi

## ensure plpgsql DB language is available
echo "createlang -U postgres plpgsql $LEXDB"
createlang -U postgres plpgsql $LEXDB
#if [ $? != 0 ] ; then drop_lexdb; fi

## load DB superuser setup script
if [ ! -f su-setup.sql ]; then echo "cannot find file su-setup.sql"; 
    echo "drop_lexdb"
    drop_lexdb; 
fi
echo psql -f su-setup.sql -U postgres $LEXDB
psql -f su-setup.sql -U postgres $LEXDB
if [ $? != 0 ] ; then drop_lexdb; fi

## load 'lexdb' DB user setup script
if [ ! -f load.sql ]; then echo "cannot find file load.sql"; drop_lexdb; fi
echo "psql -f load.sql -U lexdb $LEXDB"
psql -f load.sql -U lexdb $LEXDB
if [ $? != 0 ] ; then 
    echo "drop_lexdb"
    drop_lexdb; 
fi

## load field definitions
if [ -f $FLD_FILE ]; then 
    echo "taking field defns from file $FLD_FILE";
    echo "psql -c 'delete from fields' -U lexdb $LEXDB"
    psql -c 'delete from fields' -U lexdb $LEXDB; 
    if [ $? != 0 ] ; then 
	echo "drop_lexdb"
	drop_lexdb; 
    fi
    echo "psql -c \"copy fields from $FLD_FILE\" -U lexdb $LEXDB"
    psql -c "\copy fields from $FLD_FILE" -U lexdb $LEXDB; 
    if [ $? != 0 ] ; then 
	echo "drop_lexdb"
	drop_lexdb; 
    fi
else
    echo "cannot find file $FLD_FILE";
    for x in `seq 1 10`; do echo wait $x; done  
    echo "drop_lexdb"
    drop_lexdb;
fi

## load initialization DB script
if [ ! -f init.sql ]; then echo "cannot find file init.sql"; 
    echo "drop_lexdb"
    drop_lexdb; 
fi
echo "psql -f init.sql -U lexdb $LEXDB"
psql -f init.sql -U lexdb $LEXDB
if [ $? != 0 ] ; then 
    echo "drop_lexdb"
    drop_lexdb; 
fi
    
## set lexdb tmp dir (IS THIS NECESSARY??)
if [ "$OSTYPE" = "cygwin" ]; then
    echo "psql -U lexdb -c \"delete from public.meta where var='tmp-dir'; insert into public.meta values ('tmp-dir','C:\tmp')\" $LEXDB"
    psql -U lexdb -c "delete from public.meta where var='tmp-dir'; insert into public.meta values ('tmp-dir','C:\tmp')" $LEXDB
else
    echo "psql -U lexdb -c \"delete from public.meta where var='tmp-dir'; insert into public.meta values ('tmp-dir','/tmp')\" $LEXDB"
    psql -U lexdb -c "delete from public.meta where var='tmp-dir'; insert into public.meta values ('tmp-dir','/tmp')" $LEXDB
fi
