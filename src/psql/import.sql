DROP TABLE erg;
CREATE TABLE erg (
  id INTEGER,
  name VARCHAR(95),
  type VARCHAR(95),
  orthography VARCHAR(200),
  orthkey VARCHAR(200),
  pronunciation VARCHAR(200),
  keyrel VARCHAR(50),
  altkey VARCHAR(50),
  alt2key VARCHAR(50),
  keytag VARCHAR(50),
  compkey VARCHAR(50),
  ocompkey VARCHAR(50),
  complete VARCHAR(200),
  semclasses VARCHAR(100),
  preferences VARCHAR(200),
  classifier VARCHAR(25),
  selectrest VARCHAR(50),
  jlink VARCHAR(50),
  comments VARCHAR(255),
  exemplars VARCHAR(50),
  usages VARCHAR(50),
  lang VARCHAR(8) DEFAULT 'EN',
  country VARCHAR(2),
  dialect VARCHAR(25),
  domains VARCHAR(15),
  genres VARCHAR(25),
  register VARCHAR(50),
  confidence VARCHAR(50),
  userid VARCHAR(25),
  moddate DATE,
  version INTEGER DEFAULT 0,
  source VARCHAR(50),
  flags INTEGER DEFAULT 0 NOT NULL,
PRIMARY KEY (name,version)
);

CREATE INDEX id
ON erg (id); 

CREATE INDEX altkey
ON erg (altkey); 

CREATE INDEX alt2key
ON erg (alt2key); 

CREATE INDEX compkey
ON erg (compkey); 

CREATE INDEX keyrel
ON erg (keyrel); 

CREATE INDEX keytag
ON erg (keytag); 

CREATE INDEX name
ON erg (name); 

CREATE INDEX ocompkey
ON erg (ocompkey); 

CREATE INDEX orthography
ON erg (orthography); 

CREATE UNIQUE INDEX primarykey
ON erg (name,version); 

DROP TABLE ergd;
CREATE TABLE ergd (
  slot VARCHAR(50),
  field VARCHAR(50),
  path VARCHAR(255),
  type VARCHAR(20),
PRIMARY KEY (slot, path)
);

CREATE INDEX slot
ON ergd (slot); 

INSERT INTO ergd VALUES ( 'id', 'name', '', 'symbol' );
INSERT INTO ergd VALUES ( 'sense-id', 'name', '', 'symbol' );
INSERT INTO ergd VALUES ( 'orth', 'orthography', '', 'string-list' );
INSERT INTO ergd VALUES ( 'unifs', 'type', 'nil', 'symbol' );
INSERT INTO ergd VALUES ( 'unifs', 'orthography', '(stem)', 'string-fs' );
INSERT INTO ergd VALUES ( 'unifs', 'keyrel', '(synsem local keys key)', 'symbol' );
INSERT INTO ergd VALUES ( 'unifs', 'keytag', '(synsem local keys key carg)', 'string' );
INSERT INTO ergd VALUES ( 'unifs', 'altkey', '(synsem local keys altkey)', 'symbol' );
INSERT INTO ergd VALUES ( 'unifs', 'alt2key', '(synsem local keys alt2key)', 'symbol' );
INSERT INTO ergd VALUES ( 'unifs', 'compkey', '(synsem lkeys --compkey)', 'symbol' );
INSERT INTO ergd VALUES ( 'unifs', 'ocompkey', '(synsem lkeys --ocompkey)', 'symbol' );

DROP VIEW max_version;
CREATE VIEW max_version 
	AS SELECT erg.name AS "key", max(erg."version") AS max 
	FROM erg
	WHERE erg.flags=1 
	GROUP BY erg.name;

DROP VIEW erg_max_version;
CREATE VIEW erg_max_version 
	AS SELECT erg.* 
	FROM (erg JOIN max_version ON 
	(((erg.name = max_version."key") AND (erg."version" = max_version.max))));

DROP FUNCTION orthography_set();
CREATE FUNCTION orthography_set () RETURNS SETOF text 
	AS 'SELECT DISTINCT orthography FROM erg_max_version' LANGUAGE SQL;

DROP FUNCTION psort_id_set();
CREATE FUNCTION psort_id_set() RETURNS SETOF text 
	AS 'SELECT DISTINCT name FROM erg_max_version' LANGUAGE SQL;

DROP FUNCTION lookup_word(text);
CREATE FUNCTION lookup_word (text) RETURNS SETOF text 
	AS 'SELECT name FROM erg_max_version WHERE (orthography LIKE $1 OR orthography LIKE $1 || \' %\' OR orthography LIKE \'% \' || $1 || \' %\' OR orthography LIKE \'% \' || $1);' LANGUAGE SQL;

DROP FUNCTION next_version(text);
CREATE FUNCTION next_version (text) RETURNS int 
	AS 'SELECT 1 + max(version) FROM erg WHERE name = $1;' LANGUAGE SQL;

DROP FUNCTION next_id();
CREATE FUNCTION next_id () RETURNS int 
	AS 'SELECT 1 + max(id) FROM erg;' LANGUAGE SQL;
