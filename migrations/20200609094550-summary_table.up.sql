CREATE TABLE summary (
    country_id INTEGER NOT NULL,
    total_recovered INTEGER NOT NULL,
    new_recovered INTEGER NOT NULL,
    total_deaths INTEGER NOT NULL,
    new_deaths INTEGER NOT NULL,
    total_confirmed INTEGER NOT NULL,
    new_confirmed INTEGER NOT NULL,
    timestamp TIMESTAMP NOT NULL,

    CONSTRAINT summary_country_id_fkey FOREIGN KEY (country_id)
        REFERENCES country (id)
);
--;;
CREATE UNIQUE INDEX summary_country_id_timestamp_key ON summary (country_id, timestamp);
