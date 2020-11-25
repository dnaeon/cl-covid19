CREATE VIEW time_series_per_country_per_million
AS
    SELECT
        ts.timestamp,
	ts.country_id,
	ts.country_name,
	ts.country_slug,
	ts.country_iso_code,
	ts.continent_id,
	ts.continent_name,
	ts.continent_iso_code,
	CAST(ts.confirmed AS float) * 1000000 / CAST(p.people AS FLOAT) AS confirmed,
	CAST(ts.deaths AS float) * 1000000 / CAST(p.people AS FLOAT) AS deaths,
	CAST(ts.recovered AS float) * 1000000 / CAST(p.people AS FLOAT) AS recovered,
	CAST(ts.active AS float) * 1000000 / CAST(p.people AS FLOAT) AS active,
	CAST(ts.new_confirmed AS float) * 1000000 / CAST(p.people AS FLOAT) AS new_confirmed,
	CAST(ts.new_deaths AS float) * 1000000 / CAST(p.people AS FLOAT) AS new_deaths,
	CAST(ts.new_recovered AS float) * 1000000 / CAST(p.people AS FLOAT) AS new_recovered,
	CAST(ts.new_active AS float) * 1000000 / CAST(p.people AS FLOAT) AS new_active
    FROM time_series_per_country ts
    INNER JOIN population_per_country_latest p ON ts.country_id = p.country_id;
