
CREATE SEQUENCE IF NOT EXISTS tbl_link_tokens_x_workflows_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_link_tokens_x_workflows
(
    oidx integer NOT NULL DEFAULT nextval('tbl_link_tokens_x_workflows_oidx_seq'),
    id_token_h character varying(255) COLLATE pg_catalog."default" NOT NULL,
    id_workflow character (32) COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT tbl_link_tokens_x_workflows_pkey PRIMARY KEY (id_token_h, id_workflow)
);

ALTER SEQUENCE tbl_link_tokens_x_workflows_oidx_seq
OWNED BY tbl_link_tokens_x_workflows.oidx;
