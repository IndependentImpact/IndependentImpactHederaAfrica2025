
CREATE SEQUENCE IF NOT EXISTS tbl_link_agents_x_hedera_accounts_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_link_agents_x_hedera_accounts
(
    oidx integer NOT NULL DEFAULT nextval('tbl_link_agents_x_hedera_accounts_oidx_seq'),
    id_agent character(32) COLLATE pg_catalog."default" NOT NULL,
    id_acc_h character varying(255) COLLATE pg_catalog."default" NOT NULL,
	label_acc_h character varying(64) COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT tbl_link_agents_x_hedera_accounts_pkey PRIMARY KEY (id_agent, id_acc_h)
);

ALTER SEQUENCE tbl_link_agents_x_hedera_accounts_oidx_seq
OWNED BY tbl_link_agents_x_hedera_accounts.oidx;
