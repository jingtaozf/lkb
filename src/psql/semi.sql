DROP TABLE prd;
CREATE TABLE prd 
	(
	lexid text,
	predkey integer,
	pred text,
	lex text,
	pos text,
	sense text,
	carg text,
PRIMARY KEY (predkey)
	);

CREATE INDEX prd_lexid on prd (lexid);

DROP TABLE arg;
CREATE TABLE arg
	(
	predkey int,
	argkey int,
	arg int,
PRIMARY KEY (argkey)
	);

CREATE INDEX arg_predkey on arg (predkey);

DROP TABLE ddd;
CREATE TABLE ddd
	(
	argkey int,
	feat text,
	val text
	);

CREATE INDEX ddd_argkey on ddd (argkey);

copy prd from '/home/bmw20/tmp/semi.obj.prd';
copy arg from '/home/bmw20/tmp/semi.obj.arg';
copy ddd from '/home/bmw20/tmp/semi.obj.ddd';

create table obj_semi as select lexid, pred, lex, pos, sense, carg, arg, 
	(select val from ddd where argkey=t1.argkey and feat='gen') as gen,
	(select val from ddd where argkey=t1.argkey and feat='pn') as pn, 
	(select val from ddd where argkey=t1.argkey and feat='aspect.perf') as aspect_perf, 
	(select val from ddd where argkey=t1.argkey and feat='aspect.progr') as aspect_progr, 
	(select val from ddd where argkey=t1.argkey and feat='mood') as mood, 
	(select val from ddd where argkey=t1.argkey and feat='tense') as tense
	from (arg natural join prd) as t1;
