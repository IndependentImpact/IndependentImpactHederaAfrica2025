
CREATE SEQUENCE IF NOT EXISTS tbl_token_issuance_tracker_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_token_issuance_tracker
(
    oidx integer NOT NULL DEFAULT nextval('tbl_token_issuance_tracker_oidx_seq'),
	id_issuance character(32) NOT NULL,
	id_tx_h character varying(255) NOT NULL,
	date_tx timestamp without time zone NOT NULL,
	status_tx character varying(32),
	serials_tx integer[],
	
    CONSTRAINT tbl_token_issuance_tracker_pkey PRIMARY KEY (id_issuance, id_tx_h)
);

ALTER SEQUENCE tbl_token_issuance_tracker_oidx_seq
OWNED BY tbl_token_issuance_tracker.oidx;
