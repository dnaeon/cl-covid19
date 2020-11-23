DROP VIEW time_series_per_continent;
--;;
CREATE VIEW time_series_per_continent
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
	sum(new_active) AS new_active,
	continent_id,
	continent_iso_code,
	continent_name
    FROM time_series_per_country
    GROUP BY timestamp, continent_id, continent_iso_code, continent_name;
