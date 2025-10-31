CREATE SEQUENCE IF NOT EXISTS tbl_agent_reputation_event_log_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_agent_reputation_event_log
(
    oidx bigint NOT NULL DEFAULT nextval('tbl_agent_reputation_event_log_oidx_seq'),
    id_agent character(32) COLLATE pg_catalog."default" NOT NULL,
    event_type character varying(32) COLLATE pg_catalog."default" NOT NULL,
    occurred_at timestamp without time zone NOT NULL,
    trigger text COLLATE pg_catalog."default" NOT NULL,
    reputation_domain character varying(32) COLLATE pg_catalog."default" NOT NULL,
    reputation_adjustment integer NOT NULL,
    CONSTRAINT tbl_agent_reputation_event_log_pkey PRIMARY KEY (id_agent, event_type, occurred_at, trigger, reputation_domain)
);

ALTER SEQUENCE tbl_agent_reputation_event_log_oidx_seq
OWNED BY tbl_agent_reputation_event_log.oidx;
