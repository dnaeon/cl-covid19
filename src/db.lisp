;; Copyright (c) 2020 Marin Atanasov Nikolov <dnaeon@gmail.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer
;;     in this position and unchanged.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)
(defpackage :cl-covid19.db
  (:use :cl)
  (:nicknames :covid19.db)
  (:import-from :cl-dbi)
  (:import-from :cl-migratum)
  (:import-from :cl-migratum.provider.local-path)
  (:import-from :cl-migratum.driver.sql)
  (:import-from :log4cl)
  (:export
   :*migrations-path*
   :make-db-conn
   :disconnect-db-conn
   :migrate-db
   :db-execute
   :table-info
   :table-columns
   :persist-countries-data
   :persist-time-series-data
   :persist-continents-data
   :persist-population-data
   :link-countries-with-continents
   :set-numeric-code-for-countries))
(in-package :cl-covid19.db)

(defparameter *migrations-path*
  (asdf:system-relative-pathname :cl-covid19 "migrations/")
  "Path to the SQL migrations")

(defun make-db-conn (db-path)
  "Creates a new database connection to the given DB-PATH"
  (cl-dbi:connect :sqlite3 :database-name db-path))

(defun disconnect-db-conn (db-conn)
  "Disconnects from the database"
  (cl-dbi:disconnect db-conn))

(defun migrate-db (db-conn)
  "Migrates the database to the latest version"
  (let* ((provider (cl-migratum.provider.local-path:make-local-path-provider *migrations-path*))
         (driver (cl-migratum.driver.sql:make-sql-driver provider db-conn)))
    (cl-migratum:provider-init provider)
    (cl-migratum:driver-init driver)
    (cl-migratum:apply-pending driver)))

(defun db-execute (db-conn stmt &rest params)
  "Execute a given statement against the database"
  (log:debug "Executing `~a` (params ~a)" stmt params)
  (let ((stmt (cl-dbi:prepare db-conn stmt)))
    (cl-dbi:with-transaction db-conn
      (cl-dbi:fetch-all (cl-dbi:execute stmt params)))))

(defun table-info (db-conn table)
  "Get details about a given table"
  (let ((query (format nil "PRAGMA table_info(~a)" table)))
    (db-execute db-conn query)))

(defun table-columns (db-conn table)
  "Get the list of columns for a given table"
  (let ((info (table-info db-conn table)))
    (mapcar (lambda (item)
              (getf item :|name|))
            info)))

(defun persist-population-data (items db-conn)
  "Persists the given ITEMS into the database, which represent population stats per country"
  (log:debug "Persisting POPULATION data")
  (let* ((stmt (format nil "INSERT ~
                            INTO population (year, people, country_id) ~
                            VALUES (
                                $1, ~
                                $2, ~
                                (SELECT id FROM country WHERE numeric_code = $3) ~
                            ) ~
                            ON CONFLICT (year, country_id) DO UPDATE ~
                            SET ~
                                people = $2"))
         (prepared (cl-dbi:prepare db-conn stmt)))
    (cl-dbi:with-transaction db-conn
      (dolist (item items)
        (let ((year (getf item :year))
              (population (getf item :population))
              (country-name (getf item :country-name))
              (country-numeric-code (getf item :country-numeric-code)))
          (log:debug "Persisting population for country ~a (numeric code ~a) to ~a @ ~a"
                     country-name country-numeric-code population year)
          (cl-dbi:execute prepared (list year population country-numeric-code)))))))

(defun persist-countries-data (items db-conn)
  "Persists the given ITEMS into the database, which represent countries"
  (log:debug "Persisting COUNTRY data")
  (let* ((stmt (format nil "INSERT ~
                            INTO country (iso_code, name, slug) ~
                            VALUES ($1, $2, $3) ~
                            ON CONFLICT (iso_code) DO UPDATE ~
                            SET ~
                                name = $2, ~
                                slug = $3"))
        (prepared (cl-dbi:prepare db-conn stmt)))
    (cl-dbi:with-transaction db-conn
      (dolist (item items)
        (let ((iso-code (getf item :ISO2))
              (name (getf item :|Country|))
              (slug (getf item :|Slug|)))
          (log:debug "Persisting COUNTRY ~a (~a)" name iso-code)
          (cl-dbi:execute prepared (list iso-code name slug)))))))

(defun persist-time-series-data (items db-conn)
  "Persists the given ITEMS into the database, which represent time series data"
  (log:debug "Persisting TIME SERIES data")
  (let* ((stmt (format nil "INSERT ~
                            INTO time_series (country_id, confirmed, deaths, recovered, active, timestamp) ~
                            VALUES ( ~
                                (SELECT id FROM country WHERE name = $1), ~
                                $2, $3, $4, $5, $6 ~
                            ) ~
                            ON CONFLICT (country_id, timestamp) DO UPDATE ~
                            SET ~
                                confirmed = $2, ~
                                deaths = $3, ~
                                recovered = $4, ~
                                active = $5"))
         (prepared (cl-dbi:prepare db-conn stmt)))
    (cl-dbi:with-transaction db-conn
      (dolist (item items)
        (let ((country-name (getf item :|Country|))
              (confirmed (getf item :|Confirmed|))
              (deaths (getf item :|Deaths|))
              (recovered (getf item :|Recovered|))
              (active (getf item :|Active|))
              (timestamp (getf item :|Date|)))
          (log:debug "Persisting TIME SERIES for ~a @ ~a" country-name timestamp)
          (cl-dbi:execute prepared
                          (list country-name
                                confirmed
                                deaths
                                recovered
                                active
                                timestamp)))))))

(defun persist-continents-data (items db-conn)
  "Persists the given ITEMS into the database, which represent continents"
  (log:debug "Persisting CONTINENT data")
  (let* ((stmt (format nil "INSERT ~
                            INTO continent (iso_code, name) ~
                            VALUES ($1, $2) ~
                            ON CONFLICT (iso_code) DO UPDATE ~
                            SET ~
                                name = $2"))
         (prepared (cl-dbi:prepare db-conn stmt)))
    (cl-dbi:with-transaction db-conn
      (dolist (item items)
        (let ((iso-code (getf item :|Code|))
              (name (getf item :|Name|)))
          (log:debug "Persisting CONTINENT ~a (~a)" name iso-code)
          (cl-dbi:execute prepared (list iso-code name)))))))

(defun set-numeric-code-for-countries (items db-conn)
  "Sets the numeric code for each country using the ITEMS data"
  (log:debug "Setting numeric code for COUNTRY")
  (cl-dbi:with-transaction db-conn
    (dolist (item items)
      (let* ((country-name (getf item :|Country_Name|))
             (country-iso-code (getf item :|Two_Letter_Country_Code|))
             (country-numeric-code (getf item :|Country_Number|))
             (stmt (format nil "UPDATE ~
                                    country ~
                                SET
                                    numeric_code = $1 ~
                                WHERE ~
                                    iso_code = $2"))
             (prepared (cl-dbi:prepare db-conn stmt)))
        (log:debug "Setting numeric code for country ~a (~a) to ~a"
                   country-name country-iso-code country-numeric-code)
        (cl-dbi:execute prepared (list country-numeric-code country-iso-code))))))

(defun link-countries-with-continents (items db-conn)
  "Links countries with continents"
  (log:debug "Linking COUNTRY with CONTINENT")
  (cl-dbi:with-transaction db-conn
    (dolist (item items)
      (let* ((country-name (getf item :|Country_Name|))
             (country-iso-code (getf item :|Two_Letter_Country_Code|))
             (continent-code (getf item :|Continent_Code|))
             (continent-name (getf item :|Continent_Name|))
             (stmt (format nil "UPDATE
                                    country ~
                                SET ~
                                    continent_id = (SELECT id FROM continent WHERE iso_code = $1) ~
                                WHERE iso_code = $2"))
             (prepared (cl-dbi:prepare db-conn stmt)))
        (log:debug "Linking country ~a (~a) with continent ~a (~a)"
                   country-name country-iso-code continent-name continent-code)
        (cl-dbi:execute prepared (list continent-code country-iso-code))))))
