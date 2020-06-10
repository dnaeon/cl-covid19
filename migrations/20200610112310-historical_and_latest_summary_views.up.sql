DROP VIEW IF EXISTS summary_per_country;
--;;
DROP VIEW IF EXISTS summary_global;
--;;
CREATE VIEW IF NOT EXISTS summary_per_country_history
AS
    SELECT * FROM summary
    INNER JOIN country ON summary.country_id = country.id;
--;;
CREATE VIEW IF NOT EXISTS summary_per_country_latest
AS
    SELECT * FROM summary_per_country_history
    WHERE timestamp = (SELECT max(timestamp) FROM summary_per_country_history);
--;;
CREATE VIEW IF NOT EXISTS summary_global_history
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
--;;
CREATE VIEW IF NOT EXISTS summary_global_latest
AS
    SELECT * FROM summary_global_history
    WHERE timestamp = (SELECT max(timestamp) FROM summary_global_history);
