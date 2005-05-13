--- Copyright (c) 2003 - 2005 
--- Benjamin Waldron, Fabre Lambeau, Stephan Oepen;
--- see `licence.txt' for conditions.

CREATE OR REPLACE FUNCTION public.set_permissions() RETURNS boolean AS '
BEGIN

GRANT SELECT ON public.dfn TO PUBLIC;
GRANT SELECT ON public.meta TO PUBLIC;
GRANT SELECT ON public.fld TO PUBLIC;
GRANT SELECT ON public.rev TO PUBLIC;
GRANT INSERT ON public.rev TO PUBLIC;
GRANT SELECT ON public.rev_new TO PUBLIC;

EXECUTE ''GRANT CREATE ON DATABASE '' || current_database() || '' TO PUBLIC'';

GRANT INSERT ON public.meta TO PUBLIC;

RETURN true;
END;
' LANGUAGE plpgsql SECURITY DEFINER;
GRANT ALL ON public.tmp TO PUBLIC;

SELECT public.set_permissions();
