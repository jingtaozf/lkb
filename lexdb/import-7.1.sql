CREATE TABLE erg_version (
  version_str VARCHAR(50)
);
INSERT INTO erg_version VALUES ('1.1');

CREATE TABLE erg (
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
  comments VARCHAR(255),
  exemplars VARCHAR(50),
  usages VARCHAR(50),
  lang VARCHAR(8) DEFAULT 'EN',
  country VARCHAR(2),
  dialect VARCHAR(25),
  domains VARCHAR(15),
  genres VARCHAR(25),
  register VARCHAR(50),
  confidence real,
  version INTEGER DEFAULT 0,
  source VARCHAR(50),
  flags INTEGER DEFAULT 0 NOT NULL,
  userid VARCHAR(25) DEFAULT user,
  id SERIAL UNIQUE,
  modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
PRIMARY KEY (name,version)
);

CREATE INDEX orthkey
ON erg (orthkey); 

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

CREATE VIEW erg_max_version 
	AS SELECT erg.* 
	FROM 
		(erg 
		JOIN 
		(SELECT name AS nm, max(version) AS vn FROM erg GROUP BY name) AS tmp
		 ON (erg.name, erg.version) = (nm, vn) ); 

CREATE TABLE ergq (
  fn VARCHAR(50),
  arity int,
  sql_code VARCHAR(255),
PRIMARY KEY (fn)
);

INSERT INTO ergq VALUES ( 'orthography-set', 0, 'SELECT DISTINCT orthography FROM ((SELECT name, version, orthography FROM erg) AS tmp1 JOIN (SELECT name, max(version) AS version FROM erg GROUP BY name) AS tmp2 USING (name, version))' );
INSERT INTO ergq VALUES ( 'psort-id-set', 0, 'SELECT DISTINCT name FROM erg');
INSERT INTO ergq VALUES ( 'lookup-word', 1, 'SELECT name FROM ((SELECT name, version FROM erg WHERE orthkey=$0) AS tmp1 JOIN (SELECT name, max(version) AS version FROM erg GROUP BY name) AS tmp2 USING (name, version))' );
INSERT INTO ergq VALUES ( 'next-version', 1, 'SELECT COALESCE(1 + max(version),0) FROM erg WHERE name = $0');
INSERT INTO ergq VALUES ( 'retrieve-entries', 2, 'SELECT $0 FROM ((SELECT version, $0 FROM erg WHERE orthkey=$1) AS tmp JOIN (SELECT name, max(version) AS version FROM erg GROUP BY name) AS tmp2 USING (name,version))' );
INSERT INTO ergq VALUES ( 'retrieve-entry', 2, 'SELECT $0 FROM erg WHERE (name,version) = ($1, (SELECT max(version) FROM erg WHERE name=$1))' );
INSERT INTO ergq VALUES ( 'test', 1, '$0' );

CREATE TABLE ergqa (
  fn VARCHAR(50),
  arg int,
  type VARCHAR(50),
PRIMARY KEY (fn,arg)
);

INSERT INTO ergqa VALUES ( 'lookup-word', 0, 'text' );
INSERT INTO ergqa VALUES ( 'next-version', 0, 'text');
INSERT INTO ergqa VALUES ( 'retrieve-entries', 0, 'select-list' );
INSERT INTO ergqa VALUES ( 'retrieve-entries', 1, 'text' );
INSERT INTO ergqa VALUES ( 'retrieve-entry', 0, 'select-list' );
INSERT INTO ergqa VALUES ( 'retrieve-entry', 1, 'text' );
INSERT INTO ergqa VALUES ( 'test', 0, 'select-list' );
