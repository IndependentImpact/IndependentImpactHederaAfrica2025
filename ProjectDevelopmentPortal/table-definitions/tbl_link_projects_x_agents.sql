
CREATE SEQUENCE IF NOT EXISTS tbl_link_project_x_agent_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_link_projects_x_agents
(
    oidx integer NOT NULL DEFAULT nextval('tbl_link_project_x_agent_oidx_seq'),
    id_project character(32) COLLATE pg_catalog."default" NOT NULL,
    id_workflow character (32) COLLATE pg_catalog."default" NOT NULL,
    id_agent character(32) COLLATE pg_catalog."default" NOT NULL,
    role character varying(24) COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT tbl_link_project_x_agent_pkey PRIMARY KEY (id_project, id_workflow, id_agent)
);

ALTER SEQUENCE tbl_link_project_x_agent_oidx_seq
OWNED BY tbl_link_projects_x_agents.oidx;
