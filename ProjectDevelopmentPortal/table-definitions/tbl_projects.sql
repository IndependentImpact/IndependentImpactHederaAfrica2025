
CREATE SEQUENCE IF NOT EXISTS tbl_projects_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_projects
(
    oidx integer NOT NULL DEFAULT nextval('tbl_projects_oidx_seq'),
    id character(32) COLLATE pg_catalog."default" NOT NULL,
    title character varying(255) COLLATE pg_catalog."default" NOT NULL,
    date_created timestamp without time zone NOT NULL,
    created_by character(32) COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT tbl_projects_pkey PRIMARY KEY (id)
);

ALTER SEQUENCE tbl_projects_oidx_seq
OWNED BY tbl_projects.oidx;
