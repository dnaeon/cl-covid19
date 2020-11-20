CREATE TABLE continent (
    id INTEGER PRIMARY KEY,
    iso_code CHARACTER VARYING(2) NOT NULL,
    name CHARACTER VARYING(255) NOT NULL,

    CONSTRAINT continent_iso_code_key UNIQUE (iso_code)
);
--;;
CREATE INDEX continent_name_idx ON continent (name);
