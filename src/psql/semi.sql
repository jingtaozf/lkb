---
-- under devlopment
-- at present must load this file from psql
---


DROP TABLE semi_pred CASCADE;
DROP TABLE semi_frame CASCADE;
DROP TABLE semi_role CASCADE;
DROP TABLE semi_val CASCADE;
DROP TABLE semi_var CASCADE;
DROP TABLE semi_extra CASCADE;




CREATE TABLE semi_pred (
 pred_id text NOT NULL,
 frame_id int UNIQUE NOT NULL
);

CREATE TABLE semi_frame (
 frame_id int NOT NULL REFERENCES semi_pred (frame_id),
 role_id int UNIQUE NOT NULL
);

CREATE TABLE semi_role (
 role_id int NOT NULL REFERENCES semi_frame (role_id),
 slot text NOT NULL,
 val_id int UNIQUE NOT NULL
);

CREATE TABLE semi_val (
 val_id int UNIQUE NOT NULL REFERENCES semi_role (val_id),
 str text,
 symb text,
 var_id int UNIQUE
);

CREATE INDEX semi_val_var_id ON semi_val (var_id);

CREATE TABLE semi_var (
 var_id int NOT NULL,
 type text NOT NULL,
 extra_id int UNIQUE NOT NULL
);

CREATE TABLE semi_extra (
 extra_id int NOT NULL REFERENCES semi_var (extra_id),
 feat text NOT NULL,
 val text NOT NULL
);


DELETE FROM semi_frame;
DELETE FROM semi_pred;
DELETE FROM semi_role;
DELETE FROM semi_val;
DELETE FROM semi_var;
DELETE FROM semi_extra;


\copy semi_pred from '/home/bmw20/tmp/semi.obj.pred'
\copy semi_frame from '/home/bmw20/tmp/semi.obj.frame'
\copy semi_role from '/home/bmw20/tmp/semi.obj.role'
\copy semi_val from '/home/bmw20/tmp/semi.obj.slot-val'
\copy semi_var from '/home/bmw20/tmp/semi.obj.var'
\copy semi_extra from '/home/bmw20/tmp/semi.obj.extra'

---
-- merge join appears far faster
---
SET ENABLE_HASHJOIN TO false;

CREATE OR REPLACE VIEW semi_obj AS
 SELECT * FROM
  semi_pred NATURAL JOIN
  semi_frame NATURAL JOIN
  semi_role NATURAL JOIN
  semi_val NATURAL LEFT JOIN
  semi_var NATURAL LEFT JOIN
  semi_extra;
