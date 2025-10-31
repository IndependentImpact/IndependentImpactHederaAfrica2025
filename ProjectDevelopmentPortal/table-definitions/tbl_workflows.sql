
CREATE SEQUENCE IF NOT EXISTS tbl_workflows_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_workflows
(
	oidx integer NOT NULL DEFAULT nextval('tbl_workflows_oidx_seq'),
	id character(32) COLLATE pg_catalog."default" NOT NULL,
    title character varying(255) COLLATE pg_catalog."default" NOT NULL,
    tag_version character varying(32) COLLATE pg_catalog."default",
    description text COLLATE pg_catalog."default",
	subject character varying(255) COLLATE pg_catalog."default" NOT NULL,
    handler_r character varying(32) COLLATE pg_catalog."default",
	status character varying(255) COLLATE pg_catalog."default" NOT NULL,
	id_topic_h character varying(255) COLLATE pg_catalog."default",
	hash character(64) COLLATE pg_catalog."default" NOT NULL,
	id_version_prev character(32) COLLATE pg_catalog."default",
    CONSTRAINT tbl_workflows_pkey PRIMARY KEY (id)
);

ALTER SEQUENCE tbl_workflows_oidx_seq
OWNED BY tbl_workflows.oidx;
