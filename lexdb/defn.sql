--- Copyright (c) 2003-2004 
--- Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
--- see `licence.txt' for conditions.

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

DELETE FROM defn WHERE mode = 'erg';
INSERT INTO defn VALUES ( 'erg', 'id', 'name', '', 'symbol' );
INSERT INTO defn VALUES ( 'erg', 'orth', 'orthography', '', 'string-list' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'type', 'nil', 'symbol' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'orthography', '(stem)', 'string-fs' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'keyrel', '(synsem lkeys keyrel pred)', 'mixed' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'keytag', '(synsem lkeys keyrel carg)', 'string' ); 
INSERT INTO defn VALUES ( 'erg', 'unifs', 'altkey', '(synsem lkeys altkeyrel pred)', 'mixed' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'altkeytag', '(synsem lkeys altkeyrel carg)', 'string' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'alt2key', '(synsem lkeys alt2keyrel pred)', 'mixed' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'compkey', '(synsem lkeys --compkey)', 'symbol' );
INSERT INTO defn VALUES ( 'erg', 'unifs', 'ocompkey', '(synsem lkeys --ocompkey)', 'symbol' );

