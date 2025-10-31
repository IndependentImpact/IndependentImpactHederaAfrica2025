
CREATE SEQUENCE IF NOT EXISTS tbl_impact_certificates_oidx_seq;

CREATE TABLE IF NOT EXISTS tbl_impact_certificates
(
    oidx integer NOT NULL DEFAULT nextval('tbl_impact_certificates_oidx_seq'),
    id character varying(64) COLLATE pg_catalog."default" NOT NULL,
    id_entity text COLLATE pg_catalog."default" NOT NULL,
    date_issued timestamp without time zone NOT NULL,
    id_message_h character varying(32) COLLATE pg_catalog."default" NOT NULL,
    uri_ipfs text COLLATE pg_catalog."default" NOT NULL,
    status character varying(16) COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT tbl_impact_certificates_pkey PRIMARY KEY (id)
);

ALTER SEQUENCE tbl_impact_certificates_oidx_seq
OWNED BY tbl_impact_certificates.oidx;
