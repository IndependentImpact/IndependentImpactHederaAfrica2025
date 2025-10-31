
CREATE SEQUENCE IF NOT EXISTS tbl_tokens_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_tokens
(
    oidx integer NOT NULL DEFAULT nextval('tbl_tokens_oidx_seq'),
	id_token_h character varying(255) COLLATE pg_catalog."default" NOT NULL,
    name character varying(255) COLLATE pg_catalog."default" NOT NULL,
    symbol character varying(24) COLLATE pg_catalog."default" NOT NULL,
    type_token character varying(16) COLLATE pg_catalog."default" NOT NULL,
	decimals smallint NOT NULL,
    date_created timestamp without time zone NOT NULL,
	id_account_h_treasury character varying(255) COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT tbl_tokens_pkey PRIMARY KEY (id_token_h)
);

ALTER SEQUENCE tbl_tokens_oidx_seq
OWNED BY tbl_tokens.oidx;
