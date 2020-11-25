CREATE VIEW population_per_country_latest
AS
    SELECT * FROM population_per_country
    WHERE year = (SELECT max(year) FROM population_per_country);
