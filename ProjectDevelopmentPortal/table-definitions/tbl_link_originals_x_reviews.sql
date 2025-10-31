
CREATE SEQUENCE IF NOT EXISTS tbl_link_originals_x_reviews_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_link_originals_x_reviews
(
    oidx integer NOT NULL DEFAULT nextval('tbl_link_originals_x_reviews_oidx_seq'),
    id_original character(32) COLLATE pg_catalog."default" NOT NULL,
    id_review character(32) COLLATE pg_catalog."default" NOT NULL,
	outcome character varying(16),
    CONSTRAINT tbl_link_originals_x_reviews_pkey PRIMARY KEY (id_original, id_review)
);

ALTER SEQUENCE tbl_link_originals_x_reviews_oidx_seq
OWNED BY tbl_link_originals_x_reviews.oidx;
