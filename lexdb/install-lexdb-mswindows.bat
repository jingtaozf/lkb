:: Copyright (c) 2004 - 2005 
:: Benjamin Waldron;
:: see `licence.txt' for conditions.

::@ECHO OFF

::# script usage
IF "%3" == "" echo usage: install-lexdb-mswindows DBNAME FLD-FILE DFN-FILE [PG-OPTIONS CREATEDB-OPTIONS] & GOTO end

::# grab script parameters
set LEXDB=%1%
set FLD_FILE=%2%
set DFN_FILE=%3%
set PG_OPTIONS=%4%
set CREATEDB_OPTIONS=%5%

::# default settings
if "%CREATEDB_OPTIONS%" == "" set CREATEDB_OPTIONS="-E UNICODE" & echo using default CREATEDB_OPTIONS="-E UNICODE"

::# check for files

IF NOT EXIST %FLD_FILE% echo cannot find file %FLD_FILE%
IF NOT EXIST %DFN_FILE% echo cannot find file %DFN_FILE%
IF NOT EXIST load.sql echo cannot find file load.sql
IF NOT EXIST init.sql echo cannot find file init.sql

::# create PSQL DB to hold LexDB
createdb %PG_OPTIONS% %CREATEDB_OPTIONS% -U lexdb %LEXDB%

::# ensure plpgsql DB language is available
createlang %PG_OPTIONS%  -U postgres plpgsql %LEXDB%

::# load 'lexdb' DB user setup script (part 1)
psql %PG_OPTIONS% -f load.sql -U lexdb %LEXDB%

::# load fld
psql %PG_OPTIONS% -c "\copy public.fld from %FLD_FILE%" -U lexdb %LEXDB%

::# load 'lexdb' DB user setup script (part 2)
psql %PG_OPTIONS% -f init.sql -U lexdb %LEXDB%

::# load dfn
psql %PG_OPTIONS% -c "\copy public.dfn from %DFN_FILE%" -U lexdb %LEXDB%

:end