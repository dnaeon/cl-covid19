ALTER TABLE country ADD COLUMN slug CHARACTER VARYING(255) NOT NULL DEFAULT "";
--;;
CREATE INDEX country_slug_idx ON country (slug);
