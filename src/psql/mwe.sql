---
--- public tables
---

CREATE TABLE public.multi (
  name VARCHAR,
  verb_id VARCHAR,
  particle VARCHAR,
  type VARCHAR,
  keyrel VARCHAR,
PRIMARY KEY (name)
);

CREATE TABLE public.mwe_components (
  mwe_id VARCHAR,
  slot integer,
  keyrel VARCHAR,
  pred VARCHAR,
  flag boolean,
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
CREATE TABLE mwe_components AS (SELECT * FROM public.mwe_components);
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

