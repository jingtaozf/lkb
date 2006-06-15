--- Copyright (c) 2003 - 2006
--- Benjamin Waldron, Fabre Lambeau, Stephan Oepen;
--- see `licence.txt' for conditions.

-- as superuser, run: CREATE LANGUAGE plpgsql;

SET client_min_messages TO warning;

\i util.sql

-- DFN --
CREATE TABLE dfn (
		mode TEXT,
		slot TEXT,
		field TEXT,
		path TEXT,
		type TEXT,
	PRIMARY KEY (mode,slot,field));

-- LEX --
CREATE TABLE lex (
-- BUILT-IN FIELDS (do not change!!!)
name TEXT NOT NULL,
userid TEXT DEFAULT user NOT NULL,
modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
dead BOOLEAN DEFAULT '0' NOT NULL,
-- USER-DEFINED FIELDS (you can change these)
type TEXT,
orthography TEXT,
keyrel TEXT,
altkey TEXT,
alt2key TEXT,
keytag TEXT,
altkeytag TEXT,
compkey TEXT,
ocompkey TEXT,
pronunciation TEXT,
complete TEXT,
semclasses TEXT,
preferences TEXT,
classifier TEXT,
selectrest TEXT,
jlink TEXT,
comments TEXT,
exemplars TEXT,
usages TEXT,
lang TEXT,
country TEXT,
dialect TEXT,
domains TEXT,
genres TEXT,
register TEXT,
confidence real DEFAULT 1,
source TEXT,
	PRIMARY KEY (name));
CREATE INDEX lex_name_userid_modstamp ON lex (name, userid, modstamp);

-- LEX_KEY --
CREATE TABLE lex_key (
name TEXT NOT NULL,
userid TEXT DEFAULT user NOT NULL,
modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
key text NOT NULL
);
CREATE INDEX lex_key_key ON lex_key (key)