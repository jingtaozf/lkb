--- postgres optimization os poor...
ALTER DATABASE lingo SET enable_seqscan TO off;

--- versioning mechanism for db structure
DROP TABLE ergm CASCADE;
CREATE TABLE ergm (
  var varchar(50),
  val varchar(250)
);
INSERT INTO ergm VALUES ('db-version', '1.3');
INSERT INTO ergm VALUES ('filter', 'TRUE');

---
--- main table
---
DROP TABLE erg CASCADE;
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
  confidence real,
  version INTEGER DEFAULT 0,
  source VARCHAR(50),
  flags INTEGER DEFAULT 0 NOT NULL,
  userid VARCHAR(25) DEFAULT user,
--  id SERIAL,
  modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
 PRIMARY KEY (name,version,userid)
);

CREATE INDEX orthkey
ON erg (orthkey); 

CREATE INDEX erg_name_modstamp ON erg (name, modstamp);

---
--- table on which queries executed
---
DROP TABLE current_grammar CASCADE;
CREATE TABLE current_grammar (
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
  lang VARCHAR(8),
  country VARCHAR(2),
  dialect VARCHAR(25),
  domains VARCHAR(15),
  genres VARCHAR(25),
  register VARCHAR(50),
  confidence real,
  version INTEGER DEFAULT 0,
  source VARCHAR(50),
  flags INTEGER,
  userid VARCHAR(25),
--  id INTEGER,
  modstamp TIMESTAMP WITH TIME ZONE,
 PRIMARY KEY (name)
);

CREATE INDEX grammar_orthkey
ON grammar (orthkey); 

---
--- temporary table
---
DROP TABLE grammar_update CASCADE;
CREATE TABLE grammar_update (

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
  lang VARCHAR(8),
  country VARCHAR(2),
  dialect VARCHAR(25),
  domains VARCHAR(15),
  genres VARCHAR(25),
  register VARCHAR(50),
  confidence real,
  version INTEGER DEFAULT 0,

  source VARCHAR(50),
  flags INTEGER,
  userid VARCHAR(25),
--  id INTEGER,
  modstamp TIMESTAMP WITH TIME ZONE
);

---
--- definition table
---
DROP TABLE ergd CASCADE;
CREATE TABLE ergd (
  slot VARCHAR(50),
  field VARCHAR(50),
  path VARCHAR(255),
  type VARCHAR(20),
PRIMARY KEY (slot, path)
);

--- unnecessary???
CREATE INDEX ergd_slot
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

---
--- (default) view of active versions
---

DROP VIEW erg_current CASCADE;
CREATE VIEW erg_current
	AS SELECT erg.* 
	FROM 
		(erg 
		NATURAL JOIN 
		(SELECT name, max(modstamp) AS modstamp 
                        FROM erg
                        WHERE flags = 1
                        GROUP BY name) AS tmp
		 ); 

CREATE VIEW new_pkeys 
       AS SELECT t2.* from 
          ((SELECT name,userid,version FROM erg) t1 
           RIGHT OUTER JOIN 
           (SELECT name,userid,version FROM grammar_update) t2 
           USING (name,userid,version))
            where t1.name IS NULL;

---
--- sql code embedded in db
---
DROP TABLE ergq CASCADE;
CREATE TABLE ergq (
  fn VARCHAR(50),
  arity int,
  sql_code VARCHAR(1024),
PRIMARY KEY (fn)
);

INSERT INTO ergq VALUES 
       ( 'test', 1, 
         '$0' );

INSERT INTO ergq VALUES 
       ( 'next-version', 1, 
         'SELECT COALESCE(1 + max(version),0) FROM erg WHERE (name,user) = ($0,user)');

INSERT INTO ergq VALUES 
       ( 'orthography-set', 0, 
         'SELECT DISTINCT orthography FROM current_grammar' );
INSERT INTO ergq VALUES 
       ( 'psort-id-set', 0, 
         'SELECT DISTINCT name FROM current_grammar');
INSERT INTO ergq VALUES 
       ( 'lookup-word', 1, 
         'SELECT name FROM current_grammar WHERE orthkey=$0' );
INSERT INTO ergq VALUES 
       ( 'retrieve-entries-by-orthkey', 2, 
         'SELECT $0 FROM current_grammar WHERE orthkey=$1' );
INSERT INTO ergq VALUES 
       ( 'retrieve-entry', 2, 
         'SELECT $0 FROM current_grammar WHERE name=$1' );

INSERT INTO ergq VALUES 
       ( 'initialize-current-grammar', 0, 
         'VACUUM ANALYZE erg; 
          BEGIN; 
          DELETE FROM current_grammar; 
          INSERT INTO current_grammar SELECT * FROM erg_current; 
          COMMIT; 
          CLUSTER current_grammar_pkey ON current_grammar; 
          VACUUM ANALYZE current_grammar' );
INSERT INTO ergq VALUES 
       ( 'update-current-grammar', 0, 
       'BEGIN;
        DELETE FROM grammar_update;
        INSERT INTO grammar_update 
               SELECT * FROM erg_current 
                      WHERE modstamp >= 
                      (SELECT max(modstamp) FROM current_grammar); 
        DELETE FROM current_grammar 
               WHERE name IN 
               (SELECT name FROM erg_update); 
        INSERT INTO current_grammar 
               SELECT * FROM grammar_update; 
        COMMIT' );
INSERT INTO ergq VALUES 
       ( 'current-grammar-up-to-date-p', 0, 
       '(SELECT 
                (SELECT COALESCE(max(modstamp),''-infinity'') FROM current_grammar) 
                < 
                (SELECT COALESCE(max(modstamp),''infinity'') FROM erg))');
INSERT INTO ergq VALUES 
       ( 'update-entry', 3, 
       'INSERT INTO erg (name, $1) VALUES ($0, $2); 
       DELETE FROM current_grammar 
              WHERE name=$0; 
       INSERT INTO current_grammar 
              SELECT * FROM erg_current 
              	WHERE name = $0
		LIMIT 1' );
INSERT INTO ergq VALUES 
       ( 'set-current-view', 1, 
       'DROP VIEW erg_current;
        CREATE VIEW erg_current
	AS SELECT erg.* 
	FROM 
		(erg 
		NATURAL JOIN 
		(SELECT name, max(modstamp) AS modstamp 
                        FROM erg
                        WHERE flags = 1
                              AND $0
                        GROUP BY name) AS tmp
		 ); 
        UPDATE ergm SET val=''$0'' WHERE var=''filter'';
' );
INSERT INTO ergq VALUES 
       ( 'merge-into-db', 1, 
       '
       DELETE FROM grammar_update;
       COPY grammar_update FROM $0 DELIMITERS '','' NULL '''';
       INSERT INTO erg 
              (SELECT grammar_update.* FROM (new_pkeys NATURAL JOIN grammar_update));
       DELETE FROM grammar_update;
       ' );
INSERT INTO ergq VALUES 
       ( 'dump-db', 1, 
       '
       DELETE FROM grammar_update;       
       INSERT INTO grammar_update (SELECT * FROM erg ORDER BY modstamp, name, userid, version);
       COPY erg TO $0 DELIMITERS '','' NULL '''';
       DELETE FROM grammar_update;
' );



---
--- arities of sql 'virtual functions'
---
DROP TABLE ergqa CASCADE;
CREATE TABLE ergqa (
  fn VARCHAR(50),
  arg int,
  type VARCHAR(50),
PRIMARY KEY (fn,arg)
);

INSERT INTO ergqa VALUES ( 'test', 0, 'select-list' );

INSERT INTO ergqa VALUES ( 'next-version', 0, 'text');

INSERT INTO ergqa VALUES ( 'lookup-word', 0, 'text' );
INSERT INTO ergqa VALUES ( 'retrieve-entries-by-orthkey', 0, 'select-list' );
INSERT INTO ergqa VALUES ( 'retrieve-entries-by-orthkey', 1, 'text' );
INSERT INTO ergqa VALUES ( 'retrieve-entry', 0, 'select-list' );
INSERT INTO ergqa VALUES ( 'retrieve-entry', 1, 'text' );
INSERT INTO ergqa VALUES ( 'update-entry', 0, 'text' );
INSERT INTO ergqa VALUES ( 'update-entry', 1, 'select-list' );
INSERT INTO ergqa VALUES ( 'update-entry', 2, 'value-list' );
INSERT INTO ergqa VALUES ( 'set-current-view', 0, 'where-subcls' );
INSERT INTO ergqa VALUES ( 'merge-into-db', 0, 'text' );
INSERT INTO ergqa VALUES ( 'dump-db', 0, 'text' );
