--- Copyright (c) 2003 - 2006
--- Benjamin Waldron, Fabre Lambeau, Stephan Oepen;
--- see `licence.txt' for conditions.

SET client_min_messages TO warning;

-- DFN -- don't change this!
CREATE TABLE dfn (
		slot TEXT,
		field TEXT,
		path TEXT,
		type TEXT,
	PRIMARY KEY (slot,field));

-- LEX -- customize this...
CREATE TABLE lex (
-- BUILT-IN FIELD (do not change!!!)
name TEXT NOT NULL,
-- USER-DEFINED FIELDS
userid TEXT DEFAULT user NOT NULL,
modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
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