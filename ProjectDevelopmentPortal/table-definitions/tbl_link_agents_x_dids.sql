
CREATE SEQUENCE IF NOT EXISTS tbl_link_agents_x_dids_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_link_agents_x_dids
(
    oidx integer NOT NULL DEFAULT nextval('tbl_link_agents_x_dids_oidx_seq'),
    id_agent character(32) COLLATE pg_catalog."default" NOT NULL,
    did text COLLATE pg_catalog."default" NOT NULL,
    uri_ipfs_doc_did text COLLATE pg_catalog."default",
    CONSTRAINT tbl_link_agents_x_dids_pkey PRIMARY KEY (id_agent, did)
);

ALTER SEQUENCE tbl_link_agents_x_dids_oidx_seq
OWNED BY tbl_link_agents_x_dids.oidx;
