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
  modstamp TIMESTAMP WITH TIME ZONE,
 PRIMARY KEY (name)
);

CREATE INDEX current_grammar_orthkey
ON current_grammar (orthkey); 

CREATE UNIQUE INDEX current_grammar_name
ON current_grammar (name); 

---
--- temporary table
---
CREATE TABLE temp (

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
--
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
 modstamp TIMESTAMP WITH TIME ZONE
);

CREATE TABLE multi_temp (
 name VARCHAR(95),
 verb_id VARCHAR(95),
 particle VARCHAR(95),
 type VARCHAR(200),
 keyrel VARCHAR(200),
PRIMARY KEY (name)
);
