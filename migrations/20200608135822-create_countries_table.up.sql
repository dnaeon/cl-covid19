CREATE TABLE country (
    id INTEGER PRIMARY KEY,
    iso_code CHARACTER VARYING(3) NOT NULL,
    name CHARACTER VARYING(255) NOT NULL,

    CONSTRAINT country_iso_code_key UNIQUE (iso_code)
);
--;;
CREATE INDEX country_name_idx ON country (name);
