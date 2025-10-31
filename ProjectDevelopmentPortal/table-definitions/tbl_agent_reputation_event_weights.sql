CREATE SEQUENCE IF NOT EXISTS tbl_agent_reputation_event_weights_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_agent_reputation_event_weights
(
    oidx integer NOT NULL DEFAULT nextval('tbl_agent_reputation_event_weights_oidx_seq'),
    event_type character varying(32) COLLATE pg_catalog."default" NOT NULL,
    agent_role character varying(32) COLLATE pg_catalog."default" NOT NULL,
	reputation_domain character varying(32) COLLATE pg_catalog."default" NOT NULL,
    weight integer NOT NULL,
    cap_pday integer,
    CONSTRAINT tbl_agent_reputation_event_weights_pkey PRIMARY KEY (event_type, agent_role, reputation_domain)
);

ALTER SEQUENCE tbl_agent_reputation_event_weights_oidx_seq
OWNED BY tbl_agent_reputation_event_weights.oidx;

INSERT INTO tbl_agent_reputation_event_weights (event_type, agent_role, reputation_domain, weight, cap_pday) VALUES
 ('project_registration_approved', 'PROJECT_DEVELOPER', 'PROJECT_DEVELOPMENT', 1, NULL),
 ('project_registration_approved', 'PDD_VALIDATOR', 'PDD_VALIDATION', 2, NULL),
 ('project_registration_rejected', 'PROJECT_DEVELOPER', 'PROJECT_DEVELOPMENT', -1, NULL),
 ('project_registration_rejected', 'PDD_VALIDATOR', 'PDD_VALIDATION', -2, NULL),
 ('vic_issuance_approved', 'PROJECT_DEVELOPER', 'PROJECT_DEVELOPMENT', 1, NULL),
 ('vic_issuance_approved', 'MR_VERIFIER', 'MR_VERIFICATION', 2, NULL),
 ('vic_issuance_rejected', 'PROJECT_DEVELOPER', 'PROJECT_DEVELOPMENT', -1, NULL),
 ('vic_issuance_rejected', 'MR_VERIFIER', 'MR_VERIFICATION', -2, NULL)
ON CONFLICT ON CONSTRAINT tbl_agent_reputation_event_weights_pkey DO UPDATE SET
    weight = EXCLUDED.weight,
    cap_pday = EXCLUDED.cap_pday;
