# Copyright (c) 2004 
# Benjamin Waldron;
# see `licence.txt' for conditions.

# USAGE: install-lexdb DBNAME FIELDS-FILE

cd $2
createdb -U lexdb $1
createlang -U postgres plpgsql $1
psql -f su-setup.sql -U postgres $1
psql -f load.sql -U lexdb $1
if [ -f fields.tsv ]; then 
    echo "taking field defns from file $2";
    psql -c 'delete from fields' -U lexdb $1; 
    psql -c "\copy fields from $2" -U lexdb $1; 
else
    echo "ERROR: no field defns file given!";
    echo "ABORTING..."
    dropdb -U lexdb $1
    exit 5;
fi
psql -f init.sql -U lexdb $1

