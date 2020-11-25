CREATE VIEW time_series_per_country_incidence
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
	CAST(ts.new_confirmed AS float) * 100000 / CAST(p.people AS FLOAT) AS incidence
    FROM time_series_per_country_weekly_avg ts
    INNER JOIN population_per_country_latest p ON ts.country_id = p.country_id;
