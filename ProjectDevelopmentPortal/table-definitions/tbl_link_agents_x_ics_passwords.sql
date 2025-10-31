
CREATE SEQUENCE IF NOT EXISTS tbl_link_agents_x_ics_passwords_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_link_agents_x_ics_passwords
(
    oidx integer NOT NULL DEFAULT nextval('tbl_link_agents_x_ics_passwords_oidx_seq'),
    id_agent character(32) COLLATE pg_catalog."default" NOT NULL,
    hash_pw_ics character(128) COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT tbl_link_agents_x_ics_passwords_pkey PRIMARY KEY (id_agent)
);

ALTER SEQUENCE tbl_link_agents_x_ics_passwords_oidx_seq
OWNED BY tbl_link_agents_x_ics_passwords.oidx;
