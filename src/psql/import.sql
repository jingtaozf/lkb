-- DROP TABLE meta CASCADE;
-- DROP TABLE revision CASCADE;
-- DROP TABLE multi CASCADE;
-- DROP TABLE current_grammar CASCADE;
-- DROP TABLE temp CASCADE;
-- DROP TABLE multi_temp CASCADE;
-- DROP TABLE qry CASCADE;
-- DROP TABLE qrya CASCADE;

-- DROP VIEW revision_active CASCADE;
-- DROP VIEW multi_revision_active CASCADE;
-- DROP VIEW active CASCADE;

--- postgres optimization is poor...
ALTER DATABASE lingo SET enable_seqscan TO off;


CREATE TABLE meta (
  var varchar(50),
  val varchar(250)
);
INSERT INTO meta VALUES ('db-version', '1.8');
INSERT INTO meta VALUES ('filter', 'TRUE');

---
--- main table
---
CREATE TABLE revision (
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
  modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
 PRIMARY KEY (name,version,userid)
);

CREATE INDEX orthkey
ON revision (orthkey); 

CREATE INDEX revision_name_modstamp ON revision (name, modstamp);

---
--- multi word entries
---

CREATE TABLE multi (
  name VARCHAR(95),
  verb_id VARCHAR(95),
  particle VARCHAR(95),
  type VARCHAR(200),
  keyrel VARCHAR(200),
PRIMARY KEY (name)
);

CREATE VIEW multi_revision AS 
	SELECT 
  multi.name,
  COALESCE(multi.type,rev.type) AS type,
  rev.orthography,
  rev.orthkey,
  rev.pronunciation,
  COALESCE(multi.keyrel,rev.keyrel) AS keyrel,
  rev.altkey,
  rev.alt2key,
  rev.keytag,
  COALESCE(multi.particle,rev.compkey) AS compkey,
  rev.ocompkey,
  rev.complete,
  rev.semclasses,
  rev.preferences,

  rev.classifier,
  rev.selectrest,
  rev.jlink,
  rev.comments,
  rev.exemplars,
  rev.usages,
  rev.lang,
  rev.country,
  rev.dialect,
  rev.domains,
  rev.genres,
  rev.register,
  rev.confidence,
  rev.version,

  rev.source,
  rev.flags,
  rev.userid,
  rev.modstamp

	FROM multi LEFT JOIN revision AS rev 
		ON rev.name = multi.verb_id;

---
--- table on which queries executed
---

CREATE TABLE current_grammar AS SELECT * FROM revision WHERE NULL;

CREATE UNIQUE INDEX current_grammar_name
ON current_grammar (name); 

CREATE INDEX current_grammar_orthkey
ON current_grammar (orthkey); 

---
--- scratch tables
---
CREATE TABLE temp AS SELECT * FROM revision WHERE NULL;
CREATE TABLE multi_temp AS SELECT * FROM multi WHERE NULL;

---
--- sql queries embedded in db
---
CREATE TABLE qry (
  fn VARCHAR(50),
  arity int,
  sql_code VARCHAR(4096),
PRIMARY KEY (fn)
);

-- arities
CREATE TABLE qrya (
  fn VARCHAR(50),
  arg int,
  type VARCHAR(50),
PRIMARY KEY (fn,arg)
);

INSERT INTO qrya VALUES ( 'test', 0, 'select-list' );
INSERT INTO qry VALUES 
       ( 'test', 1, 
         '$0' );

INSERT INTO qrya VALUES ( 'next-version', 0, 'text');
INSERT INTO qry VALUES 
       ( 'next-version', 1, 
         'SELECT COALESCE(1 + max(version),0) FROM revision WHERE (name,user) = ($0,user)');

INSERT INTO qry VALUES 
       ( 'orthography-set', 0, 
         'SELECT DISTINCT orthography FROM current_grammar' );

INSERT INTO qry VALUES 
       ( 'psort-id-set', 0, 
         'SELECT DISTINCT name FROM current_grammar');

INSERT INTO qrya VALUES ( 'lookup-word', 0, 'text' );
INSERT INTO qry VALUES 
       ( 'lookup-word', 1, 
         'SELECT name FROM current_grammar WHERE orthkey=$0' );

INSERT INTO qrya VALUES ( 'retrieve-entries-by-orthkey', 0, 'select-list' );
INSERT INTO qrya VALUES ( 'retrieve-entries-by-orthkey', 1, 'text' );
INSERT INTO qry VALUES 
       ( 'retrieve-entries-by-orthkey', 2, 
         'SELECT $0 FROM current_grammar WHERE orthkey=$1' );

INSERT INTO qrya VALUES ( 'retrieve-entry', 0, 'select-list' );
INSERT INTO qrya VALUES ( 'retrieve-entry', 1, 'text' );
INSERT INTO qry VALUES 
       ( 'retrieve-entry', 2, 
         'SELECT $0 FROM current_grammar WHERE name=$1' );

INSERT INTO qry VALUES 
       ( 'initialize-current-grammar', 0, 
'VACUUM ANALYZE revision; 
BEGIN; 
DELETE FROM current_grammar; 
INSERT INTO current_grammar SELECT * FROM active; 
COMMIT; 
CLUSTER current_grammar_name ON current_grammar; 
VACUUM ANALYZE current_grammar;
' );

-- see fn defn
INSERT INTO qry VALUES 
       ( 'update-current-grammar', 0, 
	'BEGIN;
DELETE FROM temp;
INSERT INTO temp 
 (SELECT * FROM active 
   WHERE modstamp >= 
    (SELECT max(modstamp) FROM current_grammar)); 
DELETE FROM current_grammar 
 WHERE name IN 
  (SELECT name FROM temp); 
INSERT INTO current_grammar
 (SELECT * FROM temp); 
COMMIT;');

INSERT INTO qrya VALUES ( 'update-entry', 0, 'text' );
INSERT INTO qrya VALUES ( 'update-entry', 1, 'select-list' );
INSERT INTO qrya VALUES ( 'update-entry', 2, 'value-list' );
INSERT INTO qry VALUES 
       ( 'update-entry', 3, 
       '
INSERT INTO revision (name, $1) VALUES ($0, $2); 
DELETE FROM current_grammar 
 WHERE name=$0; 
INSERT INTO current_grammar 
 SELECT * FROM active
  WHERE name = $0
   LIMIT 1;' );

INSERT INTO qrya VALUES ( 'set-current-view', 0, 'where-subcls' );
INSERT INTO qrya VALUES ( 'set-current-view', 1, 'text' );
INSERT INTO qry VALUES 
       ( 'set-current-view', 2, 
       '
DROP VIEW active;
DROP VIEW revision_active;
DROP VIEW multi_revision_active;

CREATE VIEW revision_active
 AS SELECT revision.* 
 FROM 
  (revision 
  NATURAL JOIN 
   (SELECT name, max(modstamp) AS modstamp 
     FROM revision
     WHERE flags = 1
      AND $0
     GROUP BY name) AS t1
); 

CREATE VIEW multi_revision_active
 AS SELECT rev.* 
 FROM 
  (multi_revision AS rev 
  NATURAL JOIN 
   (SELECT name, max(modstamp) AS modstamp 
     FROM multi_revision
      WHERE flags = 1
       AND $0
      GROUP BY name) AS t1
); 

CREATE VIEW active
 AS SELECT * FROM revision_active UNION SELECT * FROM multi_revision_active;
UPDATE meta SET val=$1 WHERE var=''filter'';
' );

INSERT INTO qrya VALUES ( 'merge-into-db', 0, 'text' );
INSERT INTO qry VALUES 
       ( 'merge-into-db', 1, 
       '
DELETE FROM temp;
COPY temp FROM $0 DELIMITERS '','' WITH NULL AS '''';
INSERT INTO revision 
 (SELECT temp.* FROM (new_pkeys NATURAL JOIN temp));
       ' );

INSERT INTO qrya VALUES ( 'merge-multi-into-db', 0, 'text' );
INSERT INTO qry VALUES 
       ( 'merge-multi-into-db', 1, 
       '
DELETE FROM multi_temp;
COPY multi_temp FROM $0 DELIMITERS '','';
DELETE FROM multi WHERE name IN (SELECT name FROM multi_temp);
INSERT INTO multi
 (SELECT * FROM multi_temp);
       ' );

INSERT INTO qrya VALUES ( 'dump-db', 0, 'text' );
INSERT INTO qry VALUES 
       ( 'dump-db', 1, 
       '
DELETE FROM temp;
INSERT INTO temp
 (SELECT * FROM revision ORDER BY modstamp, name, userid, version);
COPY temp TO $0 DELIMITERS '','' WITH NULL AS '''';
' );

INSERT INTO qrya VALUES ( 'dump-multi-db', 0, 'text' );
INSERT INTO qry VALUES 
       ( 'dump-multi-db', 1, 
       '
DELETE FROM multi_temp;
INSERT INTO multi_temp
 (SELECT * FROM multi ORDER BY name);
COPY multi_temp TO $0 DELIMITERS '','';
' );

---
--- views
---

CREATE VIEW revision_active
	AS SELECT revision.* 
	FROM 
		(revision 
		NATURAL JOIN 
		(SELECT name, max(modstamp) AS modstamp 
                        FROM revision
                        WHERE flags = 1
                        GROUP BY name) AS t1
		 ); 

CREATE VIEW multi_revision_active
	AS SELECT rev.* 
	FROM 
		(multi_revision AS rev 
		NATURAL JOIN 
		(SELECT name, max(modstamp) AS modstamp 
                        FROM multi_revision
                        WHERE flags = 1
                        GROUP BY name) AS t1
		 ); 

CREATE VIEW active
	AS SELECT * FROM revision_active UNION SELECT * FROM multi_revision_active;


CREATE VIEW new_pkeys 
       AS SELECT t2.* from 
          ((SELECT name,userid,version FROM revision) t1 
           RIGHT OUTER JOIN 
           (SELECT name,userid,version FROM temp) t2 
           USING (name,userid,version))
            where t1.name IS NULL;

---
--- definition table
---

-- DROP TABLE defn CASCADE;

CREATE TABLE defn (
  mode VARCHAR(50),
  slot VARCHAR(50),
  field VARCHAR(50),
  path VARCHAR(255),
  type VARCHAR(20),
PRIMARY KEY (mode,slot, field)
);

DELETE FROM defn WHERE mode = 'erg';
INSERT INTO defn VALUES ( 'erg', 'id', 'name', '', 'symbol' );
-- INSERT INTO defn VALUES ( 'erg', 'sense-id', 'name', '', 'symbol' );
INSERT INTO defn VALUES ( 'erg', 'orth', 'orthography', '', 'string-list' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'type', 'nil', 'symbol' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'orthography', '(stem)', 'string-fs' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'keyrel', '(synsem local keys key)', 'symbol' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'keytag', '(synsem local keys key carg)', 'string' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'altkey', '(synsem local keys altkey)', 'symbol' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'alt2key', '(synsem local keys alt2key)', 'symbol' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'compkey', '(synsem lkeys --compkey)', 'symbol' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'ocompkey', '(synsem lkeys --ocompkey)', 'symbol' );


DELETE FROM defn WHERE mode = 'mwe';
INSERT INTO defn VALUES ( 'mwe', 'id', 'name', '', 'symbol' );
-- INSERT INTO defn VALUES ( 'mwe', 'sense-id', 'name', '', 'symbol' );
INSERT INTO defn VALUES ( 'mwe', 'orth', 'orthography', '', 'string-list' ); 
INSERT INTO defn VALUES ( 'mwe', 'unifs', 'type', 'nil', 'symbol' );
INSERT INTO defn VALUES ( 'mwe', 'unifs', 'orthography', '(orth)', 'string-diff-fs' ); -- DIFF LIST
INSERT INTO defn VALUES ( 'mwe', 'unifs', 'keyrel', '(sem keys key1)', 'string' );
INSERT INTO defn VALUES ( 'mwe', 'unifs', 'keytag', '(sem keys key carg)', 'string' );
INSERT INTO defn VALUES ( 'mwe', 'unifs', 'altkey', '(sem keys altkey)', 'symbol' );
INSERT INTO defn VALUES ( 'mwe', 'unifs', 'alt2key', '(sem keys alt2key)', 'symbol' );
INSERT INTO defn VALUES ( 'mwe', 'unifs', 'compkey', '(sem keys --compkey)', 'symbol' );
INSERT INTO defn VALUES ( 'mwe', 'unifs', 'ocompkey', '(sem keys --ocompkey)', 'symbol' );
