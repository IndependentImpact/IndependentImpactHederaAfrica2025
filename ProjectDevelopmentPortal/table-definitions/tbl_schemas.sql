
CREATE SEQUENCE IF NOT EXISTS tbl_schemas_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_schemas
(
	oidx integer NOT NULL DEFAULT nextval('tbl_schemas_oidx_seq'),
	id character(32) COLLATE pg_catalog."default" NOT NULL,
    title character varying(255) COLLATE pg_catalog."default" NOT NULL,
    tag_version character varying(32) COLLATE pg_catalog."default",
    description text COLLATE pg_catalog."default",
	status character varying(255) COLLATE pg_catalog."default" NOT NULL,
	iri character varying(64) COLLATE pg_catalog."default" NOT NULL,
	uri_ipfs text NOT NULL,
    id_topic_h character varying(255) COLLATE pg_catalog."default",
	handler_r character varying(255) COLLATE pg_catalog."default",
	hash character(64) COLLATE pg_catalog."default" NOT NULL,
	id_version_prev character(32) COLLATE pg_catalog."default",
	b_encrypt boolean NOT NULL,
    CONSTRAINT tbl_schemas_pkey PRIMARY KEY (id)
);

ALTER SEQUENCE tbl_schemas_oidx_seq
OWNED BY tbl_schemas.oidx;
