---
--- definition table
---

CREATE TABLE defn (
  mode VARCHAR(50),
  slot VARCHAR(50),
  field VARCHAR(50),
  path VARCHAR(255),
  type VARCHAR(20),
PRIMARY KEY (mode,slot, field)
);

DELETE FROM defn WHERE mode = 'mwe';
INSERT INTO defn VALUES ( 'mwe', 'id', 'name', '', 'symbol' );
INSERT INTO defn VALUES ( 'mwe', 'orth', 'orthography', '', 'string-list' ); 
INSERT INTO defn VALUES ( 'mwe', 'unifs', 'type', 'nil', 'symbol' );
INSERT INTO defn VALUES ( 'mwe', 'unifs', 'orthography', '(orth)', 'string-diff-fs' ); -- DIFF LIST
INSERT INTO defn VALUES ( 'mwe', 'unifs', 'keyrel', '(sem hook keypred)', 'mixed' );
INSERT INTO defn VALUES ( 'mwe', 'unifs', 'compkey', '(sem keys --compkey)', 'symbol' );
INSERT INTO defn VALUES ( 'mwe', 'unifs', 'ocompkey', '(sem keys --ocompkey)', 'symbol' );

DELETE FROM defn WHERE mode = 'erg2';
INSERT INTO defn VALUES ( 'erg2', 'id', 'name', '', 'symbol' );
INSERT INTO defn VALUES ( 'erg2', 'orth', 'orthography', '', 'string-list' );
INSERT INTO defn VALUES ( 'erg2', 'unifs', 'type', 'nil', 'symbol' );
INSERT INTO defn VALUES ( 'erg2', 'unifs', 'orthography', '(stem)', 'string-fs' );
INSERT INTO defn VALUES ( 'erg2', 'unifs', 'keyrel', '(synsem lkeys keyrel pred)', 'mixed' );
INSERT INTO defn VALUES ( 'erg2', 'unifs', 'keytag', '(synsem lkeys keyrel carg)', 'string' ); 
INSERT INTO defn VALUES ( 'erg2', 'unifs', 'altkey', '(synsem lkeys altkeyrel pred)', 'mixed' );
INSERT INTO defn VALUES ( 'erg2', 'unifs', 'altkeytag', '(synsem lkeys altkeyrel carg)', 'string' );
INSERT INTO defn VALUES ( 'erg2', 'unifs', 'alt2key', '(synsem lkeys alt2keyrel pred)', 'mixed' );
INSERT INTO defn VALUES ( 'erg2', 'unifs', 'compkey', '(synsem lkeys --compkey)', 'symbol' );
INSERT INTO defn VALUES ( 'erg2', 'unifs', 'ocompkey', '(synsem lkeys --ocompkey)', 'symbol' );

