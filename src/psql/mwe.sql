---
--- public tables
---

CREATE TABLE public.multi (
  name VARCHAR,
  verb_id VARCHAR,
  particle VARCHAR,
  type VARCHAR,
  keyrel VARCHAR,
  paraphrase text,
PRIMARY KEY (name)
);

CREATE TABLE public.mwe_components (
  mwe_id VARCHAR,
  slot integer,
  keyrel VARCHAR,
  pred VARCHAR,
  optional boolean,
PRIMARY KEY (mwe_id, slot)
);

CREATE TABLE public.mwe_type (
  mwe_id VARCHAR,
  type VARCHAR,
PRIMARY KEY (mwe_id)
);

---
--- embedded code
---

DELETE FROM qry WHERE fn ILIKE 'mwe-%';
DELETE FROM qrya WHERE fn ILIKE 'mwe-%';

INSERT INTO qry VALUES 
       ( 'mwe-retrieve-id-set', 0, 
         'SELECT mwe_id FROM mwe_type_all');

INSERT INTO qrya VALUES ( 'mwe-retrieve-type', 0, 'like-text' );
INSERT INTO qry VALUES 
       ( 'mwe-retrieve-type', 1, 
         'SELECT type FROM mwe_type_all WHERE mwe_id ILIKE $0');

INSERT INTO qrya VALUES ( 'mwe-retrieve-keyrels', 0, 'like-text' );
INSERT INTO qry VALUES 
       ( 'mwe-retrieve-keyrels', 1, 
         'SELECT keyrel, slot FROM mwe_components_all WHERE mwe_id ILIKE $0');

INSERT INTO qry VALUES 
       ( 'mwe-initialize-schema', 0, 
       '
---
--- scratch tables
---

CREATE TABLE multi AS (SELECT * FROM public.multi);
-- constraints on table multi
ALTER TABLE revision ADD CONSTRAINT revision_name UNIQUE (name);
ALTER TABLE multi ADD FOREIGN KEY (verb_id) REFERENCES revision (name);

ALTER TABLE multi ADD FOREIGN KEY (paraphrase) REFERENCES revision (name);

-- (cannot use foreign key here)
CREATE OR REPLACE FUNCTION multi_check_pred() RETURNS trigger AS ''
     DECLARE
      predv varchar;
      flag boolean;
     BEGIN
         predv := btrim(NEW.keyrel,''''\\"'''');
         flag :=  NOT(predv LIKE ''''i\\\\\\\\_%'''' OR predv IN (SELECT distinct pred FROM prd));
         -- Check 
         IF flag THEN
             RAISE EXCEPTION ''''pred % must be of form i_... or \\"i_...\\" or be an existing simplex pred'''', predv;
         END IF;
         RETURN NEW;
     END;
 '' LANGUAGE plpgsql;
 
 CREATE TRIGGER check_pred BEFORE INSERT OR UPDATE ON multi
     FOR EACH ROW EXECUTE PROCEDURE multi_check_pred();

CREATE TABLE mwe_components AS (SELECT * FROM public.mwe_components);
-- constraints on table mwe_components
CREATE OR REPLACE FUNCTION mwe_components_check_pred() RETURNS trigger AS ''
     BEGIN
         -- Check 
         IF  (btrim(NEW.keyrel,''''\\"'''') = ''''prep-rel'''') THEN
             RETURN NEW;
         ELSE 
           IF NOT(btrim(NEW.keyrel,''''\\"'''') IN (SELECT distinct pred FROM prd)) THEN
             RAISE EXCEPTION ''''pred % must be an existing pred'''', NEW.keyrel;
           END IF;
        END IF;
         RETURN NEW;
     END;
 '' LANGUAGE plpgsql;


 CREATE TRIGGER check_pred BEFORE INSERT OR UPDATE ON mwe_components
     FOR EACH ROW EXECUTE PROCEDURE mwe_components_check_pred();

CREATE TABLE mwe_type AS (SELECT * FROM public.mwe_type);

---
--- temp tables
---

CREATE TABLE temp_multi AS SELECT * FROM public.multi WHERE NULL;

---
--- MWE views
---

CREATE VIEW revision_all_non_multi
AS SELECT * FROM public.revision 
   UNION 
   SELECT * FROM revision;

CREATE VIEW multi_all AS 
	SELECT * FROM public.multi
	UNION
	SELECT * FROM multi;

CREATE VIEW revision_multi AS
	SELECT 
  multi.name,
  rev.userid,
  rev.version,
  rev.modstamp,
  COALESCE(multi.type,rev.type) AS type,
  rev.orthography,
  rev.orthkey,
  rev.pronunciation,
  COALESCE(multi.keyrel,rev.keyrel) AS keyrel,
  rev.altkey,
  rev.alt2key,
  rev.keytag,
  rev.altkeytag,
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

  rev.source,
  rev.flags

	FROM multi_all AS multi LEFT JOIN revision_all_non_multi AS rev 
		ON rev.name = multi.verb_id;

CREATE VIEW mwe_components_all
AS SELECT * FROM public.mwe_components
   UNION 
   SELECT * FROM mwe_components;

CREATE VIEW mwe_type_all
AS SELECT * FROM public.mwe_type
   UNION 
   SELECT * FROM mwe_type;

---
--- standard views
---

CREATE OR REPLACE VIEW revision_all
AS SELECT * FROM public.revision 
   UNION 
   SELECT * FROM revision
   	UNION 
	SELECT * FROM revision_multi;

' );

