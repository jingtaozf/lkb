---
--- UPDATE INSTRUCTIONS (db-version=3.00)
---

--- enter psql and evaluate:

\i embedded-code.sql

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

