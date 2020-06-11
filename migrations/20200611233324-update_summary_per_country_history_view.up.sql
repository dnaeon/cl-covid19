DROP VIEW IF EXISTS summary_per_country_history;
--;;
CREATE VIEW IF NOT EXISTS summary_per_country_history
AS
    SELECT
        total_recovered,
	new_recovered,
	total_deaths,
	new_deaths,
	total_confirmed,
	new_confirmed,
	timestamp,
	country_id,
	country.name AS country_name,
	country.slug AS country_slug,
	country.iso_code AS country_iso_code
    FROM summary
    INNER JOIN country ON summary.country_id = country.id;
