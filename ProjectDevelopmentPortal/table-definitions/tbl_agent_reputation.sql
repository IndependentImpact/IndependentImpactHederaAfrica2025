CREATE SEQUENCE IF NOT EXISTS tbl_agent_reputation_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_agent_reputation
(
	oidx integer NOT NULL DEFAULT nextval('tbl_agent_reputation_oidx_seq'),
    id_agent character(32) COLLATE pg_catalog."default" NOT NULL,
    domain character varying(32) COLLATE pg_catalog."default" NOT NULL,
    score numeric NOT NULL,
    CONSTRAINT tbl_agent_reputation_pkey PRIMARY KEY (id_agent, domain)
);

ALTER SEQUENCE tbl_agent_reputation_oidx_seq
OWNED BY tbl_agent_reputation.oidx;
