
CREATE SEQUENCE IF NOT EXISTS tbl_project_impact_indicators_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_project_impact_indicators
(
    oidx integer NOT NULL DEFAULT nextval('tbl_project_impact_indicators_oidx_seq'),
    id_project character(32) NOT NULL,
    label_indicator character varying NOT NULL,
	obj_indicator json NOT NULL,
    submitted character varying NOT NULL,
    approved character varying,
    PRIMARY KEY (id_project, label_indicator)
);

ALTER SEQUENCE tbl_project_impact_indicators_oidx_seq
OWNED BY tbl_project_impact_indicators.oidx;
