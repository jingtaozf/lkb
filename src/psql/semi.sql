
CREATE OR REPLACE FUNCTION keyrel_lexeme (text) RETURNS text AS
'
SELECT split_part(btrim($1,''"''),''_'',2);
' 
LANGUAGE SQL;


CREATE OR REPLACE FUNCTION keyrel_pos (text) RETURNS text AS
'
SELECT split_part(btrim($1,''"''),''_'',3);
' 
LANGUAGE SQL;

CREATE OR REPLACE FUNCTION keyrel_sense (text) RETURNS text AS
'
SELECT 
	CASE
	WHEN split_part(btrim($1,''"''),''_'',4)=''rel'' THEN NULL
	ELSE split_part(btrim($1,''"''),''_'',4)
	END;
' 
LANGUAGE SQL;

DELETE FROM qry WHERE fn='dump-obj-semi-main';
DELETE FROM qrya WHERE fn='dump-obj-semi-main';
INSERT INTO qrya VALUES ( 'dump-obj-semi-main', 0, 'text' );
INSERT INTO qry VALUES 
       ( 'dump-obj-semi-main', 1, 
       '
CREATE TABLE obj_semi_main_temp AS
  SELECT * FROM obj_semi_main
  ORDER BY name;
update obj_semi_main_temp set preds = null where preds = '''';
COPY obj_semi_main_temp TO $0;
DROP TABLE obj_semi_main_temp;
' );

