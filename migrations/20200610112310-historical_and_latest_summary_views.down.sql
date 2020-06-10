DROP VIEW IF EXISTS summary_per_country_history;
--;;
DROP VIEW IF EXISTS summary_per_country_latest;
--;;
DROP VIEW IF EXISTS summary_global_history;
--;;
DROP VIEW IF EXISTS summary_global_latest;
--;;
CREATE VIEW IF NOT EXISTS summary_per_country
AS
    SELECT * FROM summary
    INNER JOIN country ON summary.country_id = country.id;
--;;
CREATE VIEW IF NOT EXISTS summary_global
AS
    SELECT
        timestamp,
        sum(total_recovered) AS total_recovered,
        sum(new_recovered) AS new_recovered,
        sum(total_deaths) AS total_deaths,
        sum(new_deaths) AS new_deaths,
        sum(total_confirmed) AS total_confirmed,
        sum(new_confirmed) AS new_confirmed
    FROM summary
    GROUP BY timestamp;
