---
---
---

create table revision_all as select * from revision where null;
CREATE OR REPLACE FUNCTION public.revision_new() RETURNS SETOF revision AS '
	SELECT * FROM revision_new
' LANGUAGE sql;
drop table revision_all;

create table filtered as select * from revision where null;
CREATE OR REPLACE FUNCTION public.retrieve_head_entry(text) RETURNS SETOF revision AS '
	SELECT * FROM filtered WHERE name LIKE $1 AND modstamp=(SELECT max(modstamp) FROM filtered WHERE name LIKE $1) LIMIT 1
' LANGUAGE sql;
drop table filtered;

create table current_grammar as select * from revision where null;
CREATE OR REPLACE FUNCTION public.retrieve_all_entries() RETURNS SETOF revision AS '
	SELECT * FROM current_grammar
' LANGUAGE sql;
drop table current_grammar;

create table current_grammar as select * from revision where null;
CREATE OR REPLACE FUNCTION public.retrieve_entries_by_orthkey(text) RETURNS SETOF revision AS '
	SELECT * FROM current_grammar WHERE orthkey LIKE $1
' LANGUAGE sql;
drop table current_grammar;

create table current_grammar as select * from revision where null;
CREATE OR REPLACE FUNCTION public.retrieve_entry(text) RETURNS SETOF revision AS '
	SELECT * FROM current_grammar WHERE name LIKE $1
' LANGUAGE sql;
drop table current_grammar;

