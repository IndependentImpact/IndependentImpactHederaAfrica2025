
CREATE SEQUENCE IF NOT EXISTS tbl_agent_licenses_oidx_seq;

CREATE TABLE IF NOT EXISTS tbl_agent_licenses
(
    oidx integer NOT NULL DEFAULT nextval('tbl_agent_licenses_oidx_seq'),
    id character(32) COLLATE pg_catalog."default" NOT NULL,
    did_holder text COLLATE pg_catalog."default" NOT NULL,
    scope character varying(32) COLLATE pg_catalog."default" NOT NULL,
    date_issued timestamp without time zone NOT NULL,
    id_message_h character varying(32) COLLATE pg_catalog."default" NOT NULL,
    uri_ipfs text COLLATE pg_catalog."default" NOT NULL,
    status character varying(16) COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT tbl_agent_licenses_pkey PRIMARY KEY (id)
);

ALTER SEQUENCE tbl_agent_licenses_oidx_seq
OWNED BY tbl_agent_licenses.oidx;
