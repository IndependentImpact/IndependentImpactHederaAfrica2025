
CREATE SEQUENCE IF NOT EXISTS tbl_document_metadata_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_document_metadata
(
    oidx integer NOT NULL DEFAULT nextval('tbl_document_metadata_oidx_seq'),
    id character(32) NOT NULL,
	id_entity character(32) NOT NULL,
    id_schema character varying(255) NOT NULL,
    date_created timestamp without time zone NOT NULL,
    date_modified timestamp without time zone NOT NULL,
	did_author text NOT NULL,
	id_workflow character(32) COLLATE pg_catalog."default" NOT NULL,
	step_workflow character varying(255) COLLATE pg_catalog."default" NOT NULL,
    uri_ipfs text,
	id_message_h character varying(32), 
	type_doc character varying(16),
	identifying_content json,
	id_msg_pred character varying(32), 
	status character varying(16) NOT NULL,
	outcome character varying(16),
	encrypted boolean,
    CONSTRAINT tbl_document_metadata_pkey PRIMARY KEY (id)
);

ALTER SEQUENCE tbl_document_metadata_oidx_seq
OWNED BY tbl_document_metadata.oidx;

CREATE INDEX IF NOT EXISTS idx_a_tbl_document_metadata ON tbl_document_metadata (id);
CREATE INDEX IF NOT EXISTS idx_b_tbl_document_metadata ON tbl_document_metadata (id, id_entity);
CREATE INDEX IF NOT EXISTS idx_c_tbl_document_metadata ON tbl_document_metadata (id_message_h);
CREATE INDEX IF NOT EXISTS idx_d_tbl_document_metadata ON tbl_document_metadata (id_entity);
CREATE INDEX IF NOT EXISTS idx_e_tbl_document_metadata ON tbl_document_metadata (id_workflow);
CREATE INDEX IF NOT EXISTS idx_f_tbl_document_metadata ON tbl_document_metadata (id_workflow, step_workflow);
CREATE INDEX IF NOT EXISTS idx_g_tbl_document_metadata ON tbl_document_metadata (uri_ipfs);
CREATE INDEX IF NOT EXISTS idx_h_tbl_document_metadata ON tbl_document_metadata (id_schema);
