
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

CREATE OR REPLACE VIEW obj_semi_main AS SELECT name, keyrel_lexeme(keyrel) as lexeme, keyrel_pos(keyrel) as pos, keyrel_sense(keyrel) as sense, keytag, comments, exemplars from current_grammar where btrim(keyrel,'"') like '\\_%';

CREATE TABLE obj_semi_main_temp();

DELETE FROM qry WHERE fn='dump-obj-semi-main';
DELETE FROM qrya WHERE fn='dump-obj-semi-main';
INSERT INTO qrya VALUES ( 'dump-obj-semi-main', 0, 'text' );
INSERT INTO qry VALUES 
       ( 'dump-obj-semi-main', 1, 
       '
DROP TABLE obj_semi_main_temp;
CREATE TABLE obj_semi_main_temp AS
  SELECT * FROM obj_semi_main
  ORDER BY name;
COPY obj_semi_main_temp TO $0;
' );
