---
--- UPDATE INSTRUCTIONS
---
--- enter psql and evaluate:

INSERT INTO qrya VALUES ( 'merge-into-db', 0, 'text' );
INSERT INTO qrya VALUES ( 'merge-into-db', 1, 'text' );
INSERT INTO qry VALUES
       ( 'merge-into-db', 2,
       '
DROP INDEX public_orthkey;
DROP INDEX name_modstamp;
DROP INDEX public_revision_name_modstamp;
alter table public.revision drop constraint revision_pkey;

DELETE FROM temp;
COPY temp FROM $0 DELIMITERS '','' WITH NULL AS '''';
DELETE FROM revision_new;
INSERT INTO revision_new
      select * from (select distinct name,userid,version from temp except sele
ct name,userid,version from public.revision) as t1 natural join temp;
INSERT INTO public.revision SELECT * FROM revision_new;

alter table public.revision add primary key (name,version,userid);
CREATE INDEX public_orthkey ON public.revision (orthkey);
CREATE UNIQUE INDEX name_modstamp ON public.revision (name,modstamp);
CREATE INDEX public_revision_name_modstamp ON public.revision (name, modstamp)
;

COPY revision_new TO $1 DELIMITERS '','' WITH NULL AS '''';
SELECT count(*) FROM revision_new;
      ' );

---
--- now evaluate
---

select * from public.meta where var='user';

-- for each '$user' (!!!)

CREATE TABLE $user.revision_new AS SELECT * FROM public.revision WHERE NULL;

---
--- now set db-version
---

UPDATE public.meta SET val='3.01' WHERE var='db-version';

