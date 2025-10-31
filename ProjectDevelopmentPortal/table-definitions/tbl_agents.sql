
CREATE SEQUENCE IF NOT EXISTS tbl_agents_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_agents
(
    oidx integer NOT NULL DEFAULT nextval('tbl_agents_oidx_seq'),
    id character(32) COLLATE pg_catalog."default" NOT NULL,
    date_registered timestamp without time zone NOT NULL,
	type_user character varying(32) NOT NULL,
	CONSTRAINT tbl_agents_pkey PRIMARY KEY (id)
);

ALTER SEQUENCE tbl_agents_oidx_seq
OWNED BY tbl_agents.oidx;
