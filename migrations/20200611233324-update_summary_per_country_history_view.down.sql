DROP VIEW IF EXISTS summary_per_country_history;
--;;
CREATE VIEW IF NOT EXISTS summary_per_country_history
AS
    SELECT * FROM summary
    INNER JOIN country ON summary.country_id = country.id;
