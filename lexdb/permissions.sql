--- Copyright (c) 2003-2004 
--- Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
--- see `licence.txt' for conditions.

CREATE OR REPLACE FUNCTION public.set_permissions() RETURNS boolean AS '
BEGIN

GRANT SELECT ON public.defn TO PUBLIC;
GRANT SELECT ON public.meta TO PUBLIC;
GRANT SELECT ON public.revision TO PUBLIC;
GRANT INSERT ON public.revision TO PUBLIC;
GRANT SELECT ON public.revision_new TO PUBLIC;

EXECUTE ''GRANT CREATE ON DATABASE '' || current_database() || '' TO PUBLIC'';

GRANT INSERT ON public.meta TO PUBLIC;

RETURN true;
END;
' LANGUAGE plpgsql SECURITY DEFINER;
GRANT ALL ON public.temp TO PUBLIC;

SELECT public.set_permissions();
