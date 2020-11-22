CREATE TABLE population (
    id INTEGER PRIMARY KEY,
    year INTEGER NOT NULL,
    people INTEGER NOT NULL,
    country_id INTEGER NOT NULL,

    CONSTRAINT population_year_country_id_key UNIQUE (year, country_id),
    CONSTRAINT population_country_id_fkey FOREIGN KEY (country_id)
        REFERENCES country (id)
);
--;;
CREATE VIEW population_per_country
AS
    SELECT
        year,
	people,
	people - LAG(people, 1, people) OVER (
	    PARTITION BY country_id
	    ORDER BY year
	) prev_year_diff,
	country_id,
	country.iso_code AS country_iso_code,
	country.slug AS country_slug,
	country.numeric_code AS country_numeric_code,
	country.name AS country_name,
	continent.id AS continent_id,
	continent.name AS continent_name,
	continent.iso_code AS continent_iso_code
    FROM population
    INNER JOIN country ON country.id = population.country_id
    INNER JOIN continent ON country.continent_id = continent.id;
