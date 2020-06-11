CREATE TABLE location (
    id INTEGER PRIMARY KEY,
    country_id INTEGER NOT NULL,
    province CHARACTER VARYING(255) DEFAULT NULL,
    city_name CHARACTER VARYING(255) DEFAULT NULL,
    city_code INTEGER DEFAULT NULL,
    lat REAL NOT NULL,
    lon REAL NOT NULL,

    CONSTRAINT location_lat_lon_key UNIQUE (lat, lon),
    CONSTRAINT location_country_id_fkey FOREIGN KEY (country_id)
        REFERENCES country (id)
);
--;;
CREATE INDEX location_city_name_idx ON location (city_name);
--;;
CREATE INDEX location_city_code_idx ON location (city_code);
--;;
CREATE INDEX location_province_idx ON location (province);
