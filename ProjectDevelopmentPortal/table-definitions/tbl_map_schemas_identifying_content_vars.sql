
CREATE SEQUENCE IF NOT EXISTS tbl_map_schemas_identifying_content_vars_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_map_schemas_identifying_content_vars
(
    oidx integer NOT NULL DEFAULT nextval('tbl_map_schemas_identifying_content_vars_oidx_seq'::regclass),
    id_schema character varying(32) COLLATE pg_catalog."default" NOT NULL,
    field_key character varying(255) COLLATE pg_catalog."default" NOT NULL,
    field_position_in_id smallint NOT NULL,
    CONSTRAINT tbl_map_schemas_identifying_content_vars_pkey PRIMARY KEY (id_schema, field_key)
);

ALTER SEQUENCE tbl_map_schemas_identifying_content_vars_oidx_seq
OWNED BY tbl_map_schemas_identifying_content_vars.oidx;
