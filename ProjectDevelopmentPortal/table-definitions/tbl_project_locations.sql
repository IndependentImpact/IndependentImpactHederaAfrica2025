CREATE SEQUENCE IF NOT EXISTS tbl_project_locations_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_project_locations
(
    oidx integer NOT NULL DEFAULT nextval('tbl_project_locations_oidx_seq'),
    id_project character(32) NOT NULL,
    url_ipfs_location character varying NOT NULL,
    submitted character varying NOT NULL,
    approved character varying,
    verified character varying,
    PRIMARY KEY (id_project, url_ipfs_location)
);

ALTER SEQUENCE tbl_project_locations_oidx_seq
OWNED BY tbl_project_locations.oidx;
