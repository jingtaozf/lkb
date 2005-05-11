# Copyright (c) 2004-2005 
# Benjamin Waldron;
# see `licence.txt' for conditions.

if [ -z "$2" ]; then 
    echo usage: $0 DBNAME FLD-FILE [CREATEDB-OPTIONS]
    exit
fi

export LEXDB=$1
export FIELDS_FILE=$2
export CREATEDB_OPTIONS=$3

function drop_lexdb {
#    echo "dropping lexdb..."
#    dropdb -U lexdb $LEXDB;
    echo "ABORTING..."
    exit;
}  

echo "createdb $CREATEDB_OPTIONS -U lexdb $LEXDB"
createdb $CREATEDB_OPTIONS -U lexdb $LEXDB
if [ $? != 0 ] ; then exit; fi

echo "createlang -U postgres plpgsql $LEXDB"
createlang -U postgres plpgsql $LEXDB
#if [ $? != 0 ] ; then drop_lexdb; fi

if [ ! -f su-setup.sql ]; then echo "cannot find file su-setup.sql"; 
    echo "drop_lexdb"
    drop_lexdb; 
fi
echo psql -f su-setup.sql -U postgres $LEXDB
psql -f su-setup.sql -U postgres $LEXDB
if [ $? != 0 ] ; then drop_lexdb; fi

if [ ! -f load.sql ]; then echo "cannot find file load.sql"; drop_lexdb; fi
echo "psql -f load.sql -U lexdb $LEXDB"
psql -f load.sql -U lexdb $LEXDB
if [ $? != 0 ] ; then 
    echo "drop_lexdb"
    drop_lexdb; 
fi

if [ -f $FIELDS_FILE ]; then 
    echo "taking field defns from file $FIELDS_FILE";
    echo "psql -c 'delete from fields' -U lexdb $LEXDB"
    psql -c 'delete from fields' -U lexdb $LEXDB; 
    if [ $? != 0 ] ; then 
	echo "drop_lexdb"
	drop_lexdb; 
    fi

    echo "psql -c \"copy fields from $FIELDS_FILE\" -U lexdb $LEXDB"
    psql -c "\copy fields from $FIELDS_FILE" -U lexdb $LEXDB; 
    if [ $? != 0 ] ; then 
	echo "drop_lexdb"
	drop_lexdb; 
    fi

else
    echo "cannot find file $FIELDS_FILE";
    for x in `seq 1 10`; do echo wait $x; done  
    echo "drop_lexdb"
    drop_lexdb;
fi

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
    
# set lexdb tmp dir
if [ "$OSTYPE" = "cygwin" ]; then
    echo "psql -U lexdb -c \"delete from public.meta where var='tmp-dir'; insert into public.meta values ('tmp-dir','C:\tmp')\" $LEXDB"
    psql -U lexdb -c "delete from public.meta where var='tmp-dir'; insert into public.meta values ('tmp-dir','C:\tmp')" $LEXDB
else
    echo "psql -U lexdb -c \"delete from public.meta where var='tmp-dir'; insert into public.meta values ('tmp-dir','/tmp')\" $LEXDB"
    psql -U lexdb -c "delete from public.meta where var='tmp-dir'; insert into public.meta values ('tmp-dir','/tmp')" $LEXDB
fi
