CREATE VIEW time_series_per_country
AS
    SELECT
	timestamp,
        confirmed,
	deaths,
	recovered,
	active,
	confirmed - LAG(confirmed, 1, 0) OVER (
	    PARTITION BY country_id
	    ORDER BY timestamp
	) new_confirmed,
	deaths - LAG(deaths, 1, 0) OVER (
	    PARTITION BY country_id
	    ORDER BY timestamp
	) new_deaths,
	recovered - LAG(recovered, 1, 0) OVER (
	    PARTITION BY country_id
	    ORDER BY timestamp
	) new_recovered,
	active - LAG(active, 1, 0) OVER (
	    PARTITION BY country_id
	    ORDER BY timestamp
	) new_active,
	country_id,
	country.name AS country_name,
	country.slug AS country_slug,
	country.iso_code AS country_iso_code
    FROM time_series
    INNER JOIN country ON country.id = time_series.country_id;
--;;
CREATE VIEW time_series_global
AS
    SELECT
	timestamp,
        sum(confirmed) AS confirmed,
	sum(deaths) AS deaths,
	sum(recovered) AS recovered,
	sum(active) AS active,
	sum(new_confirmed) AS new_confirmed,
	sum(new_recovered) AS new_recovered,
	sum(new_deaths) AS new_deaths,
	sum(new_active) AS new_active
    FROM time_series_per_country
    GROUP BY timestamp;
--;;
CREATE VIEW time_series_per_country_latest
AS
    SELECT * FROM time_series_per_country
    WHERE timestamp = (SELECT max(timestamp) FROM time_series_per_country);
