CREATE VIEW IF NOT EXISTS summary_per_country
AS
    SELECT * FROM summary
    INNER JOIN country ON summary.country_id = country.id;
