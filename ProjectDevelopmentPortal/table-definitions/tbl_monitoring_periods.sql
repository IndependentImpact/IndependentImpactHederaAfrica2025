
CREATE SEQUENCE IF NOT EXISTS tbl_monitoring_periods_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_monitoring_periods
(
    oidx integer NOT NULL DEFAULT nextval('tbl_monitoring_periods_oidx_seq'),
    id_project character(32) COLLATE pg_catalog."default" NOT NULL,
    date_start timestamp without time zone NOT NULL,
    date_end timestamp without time zone NOT NULL,
    CONSTRAINT tbl_monitoring_periods_pkey PRIMARY KEY (id_project, date_start, date_end)
);

ALTER SEQUENCE tbl_monitoring_periods_oidx_seq
OWNED BY tbl_monitoring_periods.oidx;
