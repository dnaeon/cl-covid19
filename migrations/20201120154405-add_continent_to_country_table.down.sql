CREATE TEMPORARY TABLE country_backup (
    id INTEGER PRIMARY KEY,
    iso_code CHARACTER VARYING(2) NOT NULL,
    name CHARACTER VARYING(255) NOT NULL,
    slug CHARACTER VARYING(255) NOT NULL,

    CONSTRAINT country_iso_code_key UNIQUE (iso_code)
);
--;;
INSERT INTO country_backup SELECT id, iso_code, name, slug FROM country;
--;;
DROP TABLE country;
--;;
CREATE TABLE country (
    id INTEGER PRIMARY KEY,
    iso_code CHARACTER VARYING(2) NOT NULL,
    name CHARACTER VARYING(255) NOT NULL,
    slug CHARACTER VARYING(255) NOT NULL,

    CONSTRAINT country_iso_code_key UNIQUE (iso_code)
);
--;;
INSERT INTO country SELECT * FROM country_backup;
--;;
DROP TABLE country_backup;
--;;
CREATE INDEX country_name_idx ON country (name);
--;;
CREATE INDEX country_slug_idx ON country (slug);
