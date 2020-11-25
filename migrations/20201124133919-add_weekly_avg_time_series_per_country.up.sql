CREATE VIEW time_series_per_country_weekly_avg
AS
    SELECT
        timestamp,
        country_id,
        country_name,
        country_slug,
        country_iso_code,
        continent_id,
        continent_name,
        continent_iso_code,
        avg(confirmed) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 6 FOLLOWING
        ) AS confirmed,
        avg(deaths) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 6 FOLLOWING
        ) AS deaths,
        avg(recovered) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 6 FOLLOWING
        ) AS recovered,
        avg(active) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 6 FOLLOWING
        ) AS active,
        avg(new_confirmed) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 6 FOLLOWING
        ) AS new_confirmed,
        avg(new_deaths) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 6 FOLLOWING
        ) AS new_deaths,
        avg(new_recovered) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 6 FOLLOWING
        ) AS new_recovered,
        avg(new_active) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 6 FOLLOWING
        ) AS new_active
    FROM time_series_per_country;
--;;
CREATE VIEW time_series_per_country_biweekly_avg
AS
    SELECT
        timestamp,
        country_id,
        country_name,
        country_slug,
        country_iso_code,
        continent_id,
        continent_name,
        continent_iso_code,
        avg(confirmed) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 13 FOLLOWING
        ) AS confirmed,
        avg(deaths) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 13 FOLLOWING
        ) AS deaths,
        avg(recovered) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 13 FOLLOWING
        ) AS recovered,
        avg(active) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 13 FOLLOWING
        ) AS active,
        avg(new_confirmed) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 13 FOLLOWING
        ) AS new_confirmed,
        avg(new_deaths) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 13 FOLLOWING
        ) AS new_deaths,
        avg(new_recovered) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 13 FOLLOWING
        ) AS new_recovered,
        avg(new_active) OVER (
            PARTITION BY country_id
            ORDER BY timestamp DESC
            ROWS BETWEEN CURRENT ROW AND 13 FOLLOWING
        ) AS new_active
    FROM time_series_per_country;
