# Copyright (c) 2005
# Benjamin Waldron;
# see `licence.txt' for conditions.

if [ -z "$1" ]; then 
    echo usage: $0 DBNAME
    exit
fi

export LEXDB=$1

function abort {
    echo "ABORTING..."
    exit;
}  

if [ ! -f load.sql ]; then echo "cannot find file load.sql"; abort; fi
echo "psql -U lexdb -c \"\i load.sql\" $LEXDB"
psql -U lexdb -c "\i load.sql" $LEXDB
if [ $? != 0 ]; then
    echo "command failed!"
    abort;
fi

if [ ! -f init.sql ]; then echo "cannot find file init.sql"; abort; fi
echo "psql -U lexdb -c \"\i init.sql\" $LEXDB"
psql -U lexdb -c "\i init.sql" $LEXDB
if [ $? != 0 ]; then
    echo "command failed!"
    abort;
fi

# set lexdb tmp dir
if [ "$OSTYPE" = "cygwin" ]; then
    echo "psql -U lexdb -c \"delete from public.meta where var='tmp-dir'; insert into public.meta values ('tmp-dir','C:\tmp')\" $LEXDB"
    psql -U lexdb -c "delete from public.meta where var='tmp-dir'; insert into public.meta values ('tmp-dir','C:\tmp')" $LEXDB
else
    echo "psql -U lexdb -c \"delete from public.meta where var='tmp-dir'; insert into public.meta values ('tmp-dir','/tmp')\" $LEXDB"
    psql -U lexdb -c "delete from public.meta where var='tmp-dir'; insert into public.meta values ('tmp-dir','/tmp')" $LEXDB
fi

    