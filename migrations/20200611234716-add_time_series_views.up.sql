CREATE VIEW IF NOT EXISTS time_series_per_country
AS
    SELECT
        confirmed,
	deaths,
	recovered,
	active,
	timestamp,
	country_id,
	country.name AS country_name,
	country.slug AS country_slug,
	country.iso_code AS country_iso_code
    FROM time_series
    INNER JOIN country ON country.id = time_series.country_id;
--;;
CREATE VIEW IF NOT EXISTS time_series_global
AS
    SELECT
        sum(confirmed) AS confirmed,
	sum(deaths) AS deaths,
	sum(recovered) AS recovered,
	sum(active) AS active,
	timestamp
    FROM time_series
    GROUP BY timestamp;
--;;
CREATE VIEW IF NOT EXISTS time_series_per_country_latest
AS
    SELECT * FROM time_series_per_country
    WHERE timestamp = (SELECT max(timestamp) FROM time_series_per_country);
