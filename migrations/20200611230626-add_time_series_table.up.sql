CREATE TABLE time_series (
    country_id INTEGER NOT NULL,
    confirmed INTEGER NOT NULL,
    deaths INTEGER NOT NULL,
    recovered INTEGER NOT NULL,
    active INTEGER NOT NULL,
    timestamp TIMESTAMP NOT NULL,

    CONSTRAINT time_series_country_id_timestamp_key UNIQUE (country_id, timestamp),
    CONSTRAINT time_series_country_id_fkey FOREIGN KEY (country_id)
        REFERENCES country (id)
);
