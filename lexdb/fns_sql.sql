CREATE OR REPLACE FUNCTION public.next_version(text) RETURNS integer AS '
    SELECT COALESCE(1 + max(version),0) FROM revision_all 
	       WHERE name LIKE $1 AND user=user;
 ' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.orthography_set() RETURNS SETOF text AS '
    SELECT DISTINCT orthography FROM current_grammar;
 ' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.lex_id_set() RETURNS SETOF text AS '
    SELECT DISTINCT name FROM current_grammar;
 ' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.lookup_word(text) RETURNS SETOF text AS '
SELECT name FROM current_grammar WHERE orthkey LIKE $1;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.show_scratch() RETURNS SETOF text AS '
SELECT distinct name FROM revision;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.get_filter() RETURNS text AS '
SELECT val FROM meta WHERE var=''filter'' LIMIT 1;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.test_user(text) RETURNS SETOF text AS '
SELECT val FROM public.meta WHERE var=''user'' AND val = $1;
' LANGUAGE SQL;
