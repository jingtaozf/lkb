CREATE OR REPLACE FUNCTION public.set_permissions() RETURNS boolean AS '
BEGIN
GRANT SELECT ON public.defn TO PUBLIC;
GRANT SELECT ON public.meta TO PUBLIC;
GRANT SELECT ON public.multi TO PUBLIC;
GRANT SELECT ON public.mwe_components TO PUBLIC;
GRANT SELECT ON public.mwe_type TO PUBLIC;
GRANT SELECT ON public.qry TO PUBLIC;
GRANT SELECT ON public.qrya TO PUBLIC;
GRANT SELECT ON public.revision TO PUBLIC;
EXECUTE ''GRANT CREATE ON DATABASE '' || current_database() || '' TO PUBLIC'';
GRANT INSERT ON public.meta TO PUBLIC;
RETURN true;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

SELECT public.set_permissions();
