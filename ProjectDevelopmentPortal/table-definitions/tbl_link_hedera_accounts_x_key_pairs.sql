
CREATE SEQUENCE IF NOT EXISTS tbl_link_hedera_accounts_x_key_pairs_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_link_hedera_accounts_x_key_pairs
(
    oidx integer NOT NULL DEFAULT nextval('tbl_link_hedera_accounts_x_key_pairs_oidx_seq'),
    id_account_h character varying(32) COLLATE pg_catalog."default" NOT NULL,
	id_key_pair character(32) COLLATE pg_catalog."default" NOT NULL,
	label_key_pair character varying(64) COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT tbl_link_hedera_accounts_x_key_pairs_pkey PRIMARY KEY (id_account_h, id_key_pair, label_key_pair)
);

ALTER SEQUENCE tbl_link_hedera_accounts_x_key_pairs_oidx_seq
OWNED BY tbl_link_hedera_accounts_x_key_pairs.oidx;
