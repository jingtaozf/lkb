---
-- under devlopment
-- at present must load this file from psql
---


DROP TABLE semi_pred CASCADE;
DROP TABLE semi_frame CASCADE;
DROP TABLE semi_var CASCADE;
DROP TABLE semi_extra CASCADE;


CREATE TABLE semi_pred (
 lex_id text NOT NULL,
 pred_id text NOT NULL,
 frame_id int PRIMARY KEY
);

CREATE TABLE semi_frame (
 frame_id int NOT NULL REFERENCES semi_pred (frame_id),
 slot text NOT NULL,
 str text,
 symb text,
 var_id int UNIQUE,
 type text
);

CREATE TABLE semi_var (
 var_id int NOT NULL,
 extra_id int PRIMARY KEY
);

CREATE TABLE semi_extra (
 extra_id int NOT NULL REFERENCES semi_var (extra_id),
 feat text NOT NULL,
 val text NOT NULL
);


DELETE FROM semi_frame;
DELETE FROM semi_pred;
DELETE FROM semi_var;
DELETE FROM semi_extra;


\copy semi_pred from '/home/bmw20/tmp/semi.obj.pred'
\copy semi_frame from '/home/bmw20/tmp/semi.obj.frame'
\copy semi_var from '/home/bmw20/tmp/semi.obj.var'
\copy semi_extra from '/home/bmw20/tmp/semi.obj.extra'

CREATE INDEX semi_pred_lex_id ON semi_pred (lex_id);
CREATE INDEX semi_pred_pred_id ON semi_pred (pred_id);
CREATE INDEX semi_frame_frame_id ON semi_frame (frame_id);
CREATE INDEX semi_frame_var_id ON semi_frame (var_id);
CREATE INDEX semi_var_var_id ON semi_var (var_id);
CREATE INDEX semi_extra_extra_id ON semi_extra (extra_id);

---
-- merge join appears far faster
---
SET ENABLE_HASHJOIN TO false;

CREATE OR REPLACE VIEW semi_obj AS
 SELECT * FROM
  semi_pred NATURAL JOIN
  semi_frame NATURAL JOIN
  semi_var NATURAL LEFT JOIN
  semi_extra;
