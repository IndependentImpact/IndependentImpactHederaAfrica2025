
CREATE SEQUENCE IF NOT EXISTS tbl_link_agents_x_email_addresses_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_link_agents_x_email_addresses
(
    oidx integer NOT NULL DEFAULT nextval('tbl_link_agents_x_email_addresses_oidx_seq'),
    id_agent character(32) COLLATE pg_catalog."default" NOT NULL,
    email_address text COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT tbl_link_agents_x_email_addresses_pkey PRIMARY KEY (id_agent, email_address)
);

ALTER SEQUENCE tbl_link_agents_x_email_addresses_oidx_seq
OWNED BY tbl_link_agents_x_email_addresses.oidx;
