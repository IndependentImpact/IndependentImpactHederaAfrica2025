
CREATE SEQUENCE IF NOT EXISTS tbl_link_agents_x_tokens_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_link_agents_x_tokens
(
    oidx integer NOT NULL DEFAULT nextval('tbl_link_agents_x_tokens_oidx_seq'),
    id_agent character(32) COLLATE pg_catalog."default" NOT NULL,
    id_token character(32) COLLATE pg_catalog."default" NOT NULL,
    id_msg_policy character varying(255) COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT tbl_link_agents_x_tokens_pkey PRIMARY KEY (id_agent, id_token, id_msg_policy)
);

ALTER SEQUENCE tbl_link_agents_x_tokens_oidx_seq
OWNED BY tbl_link_agents_x_tokens.oidx;
