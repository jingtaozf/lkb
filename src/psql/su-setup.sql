CREATE FUNCTION "plpgsql_call_handler" () RETURNS LANGUAGE_HANDLER AS '$libdir/plpgsql' LANGUAGE C;
CREATE TRUSTED LANGUAGE "plpgsql" HANDLER "plpgsql_call_handler";
