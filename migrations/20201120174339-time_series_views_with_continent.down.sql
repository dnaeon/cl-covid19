DROP VIEW time_series_per_country;
--;;
DROP VIEW time_series_per_continent;
--;;
DROP VIEW time_series_per_continent_latest;
--;;
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
