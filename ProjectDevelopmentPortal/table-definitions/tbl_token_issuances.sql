
CREATE SEQUENCE IF NOT EXISTS tbl_token_issuances_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_token_issuances
(
    oidx integer NOT NULL DEFAULT nextval('tbl_token_issuances_oidx_seq'),
	id character(32) NOT NULL,
	id_token_h character varying(255) COLLATE pg_catalog."default" NOT NULL,
	id_workflow character(32) NOT NULL,
	step_workflow character varying(255) COLLATE pg_catalog."default" NOT NULL,
	id_document character(32) NOT NULL,
	date_started timestamp without time zone NOT NULL,
	status character varying(32),
	date_completed timestamp without time zone,
	serials_token integer[],

    CONSTRAINT tbl_token_issuances_pkey PRIMARY KEY (id_token_h, id_document)
);

ALTER SEQUENCE tbl_token_issuances_oidx_seq
OWNED BY tbl_token_issuances.oidx;
