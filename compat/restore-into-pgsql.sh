#!/usr/bin/env bash
#
# Utility script for dumping the SQLite tables and
# restorig them into a PostgreSQL database.
#

set -e

SQLITE_DB=${SQLITE_DB:-"~/cl-covid19/databases/covid19.db"}
PGSQL_DB=${PGSQL_DB:-"covid19"}

# Tables to dump from the SQLite database
SQLITE_TABLES="migration continent country population population_per_country time_series time_series_per_country time_series_global time_series_per_continent time_series_per_continent_latest time_series_per_country_latest"

if [ ! -f "${SQLITE_DB}" ]; then
    echo "SQLite database does not exist: ${SQLITE_DB}"
    exit 64 # EX_USAGE
fi

for _table in ${SQLITE_TABLES}; do
    _dump_file="covid19_${_table}.sql"
    echo "Dumping table '${_table}' to ${_dump_file} ..."
    sqlite3 "${SQLITE_DB}" ".dump ${_table}" | sed '/PRAGMA/d' > "${_dump_file}"
    echo "Restoring '${_table}' into PostgreSQL database '${PGSQL_DB}' ..."
    psql -v ON_ERROR_STOP=1 "${PGSQL_DB}" < "${_dump_file}"
    rm -f "${_dump_file}"
done

echo "Done"
