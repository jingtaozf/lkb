# Copyright (c) 2004 
# Benjamin Waldron;
# see `licence.txt' for conditions.

# USAGE: install-lexdb DBNAME LEXDB-DIR

cd $2
createdb -U lexdb $1
createlang -U postgres plpgsql $1
psql -f su-setup.sql -U postgres $1
psql -f load.sql -U lexdb $1
psql -c '\copy fields from default-fields.tsv' -U lexdb $1
if [ -f fields.tsv ]; then 
    echo 'taking field defns from file fields.tsv';
    psql -c 'delete from fields' -U lexdb $1; 
    psql -c '\copy fields from fields.tsv' -U lexdb $1; 
fi
psql -f init.sql -U lexdb $1

