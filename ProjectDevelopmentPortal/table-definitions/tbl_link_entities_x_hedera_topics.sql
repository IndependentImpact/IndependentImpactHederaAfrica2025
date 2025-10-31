
CREATE SEQUENCE IF NOT EXISTS tbl_link_entities_x_hedera_topics_oidx_seq;

CREATE TABLE IF NOT EXISTS public.tbl_link_entities_x_hedera_topics
(
    oidx integer NOT NULL DEFAULT nextval('tbl_link_entities_x_hedera_topics_oidx_seq'),
    id_topic_h character varying(32) COLLATE pg_catalog."default" NOT NULL,
	id_entity character(32) COLLATE pg_catalog."default" NOT NULL,
	label_topic_h character varying(64) COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT tbl_link_entities_x_hedera_topics_pkey PRIMARY KEY (id_topic_h, id_entity, label_topic_h)
);

ALTER SEQUENCE tbl_link_entities_x_hedera_topics_oidx_seq
OWNED BY tbl_link_entities_x_hedera_topics.oidx;
