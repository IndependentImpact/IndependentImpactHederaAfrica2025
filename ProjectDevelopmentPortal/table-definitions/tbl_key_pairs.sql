
CREATE SEQUENCE IF NOT EXISTS tbl_key_pairs_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_key_pairs
(
    oidx integer NOT NULL DEFAULT nextval('tbl_key_pairs_oidx_seq'),
	id character(32) COLLATE pg_catalog."default" NOT NULL,
    public_key character(64) COLLATE pg_catalog."default" NOT NULL,
    private_key_encr text COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT tbl_key_pairs_pkey PRIMARY KEY (public_key)
);

ALTER SEQUENCE tbl_key_pairs_oidx_seq
OWNED BY tbl_key_pairs.oidx;
