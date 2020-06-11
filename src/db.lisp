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
   :persist-countries-data
   :persist-summary-data))
(in-package :cl-covid19.db)

(defparameter *migrations-path*
  (let* ((system (asdf:find-system :cl-covid19))
         (system-path (slot-value system 'asdf/component:absolute-pathname)))
    (merge-pathnames (make-pathname :directory '(:relative "migrations"))
                     system-path))
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

(defun persist-countries-data (items db-conn)
  "Persists the given ITEMS representing countries with the database"
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

(defun persist-summary-data (items db-conn)
  "Persists the given ITEMS representing summary per country data"
  (log:debug "Persisting SUMMARY data")
  (let* ((stmt (format nil
                       "INSERT ~
                        INTO summary (country_id, total_recovered, new_recovered, total_deaths, new_deaths, total_confirmed, new_confirmed, timestamp) ~
                        VALUES ((SELECT id FROM country WHERE iso_code = $1), $2, $3, $4, $5, $6, $7, $8) ~
                        ON CONFLICT (country_id, timestamp) DO UPDATE ~
                        SET ~
                            total_recovered = $2, ~
                            new_recovered = $3, ~
                            total_deaths = $4, ~
                            new_deaths = $5, ~
                            total_confirmed = $6, ~
                            new_confirmed = $7"))
         (prepared (cl-dbi:prepare db-conn stmt)))
    (cl-dbi:with-transaction db-conn
      (dolist (item items)
        (let ((iso-code (getf item :|CountryCode|))
              (total-recovered (getf item :|TotalRecovered|))
              (new-recovered (getf item :|NewRecovered|))
              (total-deaths (getf item :|TotalDeaths|))
              (new-deaths (getf item :|NewDeaths|))
              (total-confirmed (getf item :|TotalConfirmed|))
              (new-confirmed (getf item :|NewConfirmed|))
              (timestamp (getf item :|Date|)))
          (log:debug "Persisting SUMMARY for ~a @ ~a" iso-code timestamp)
          (cl-dbi:execute prepared
                          (list iso-code
                                total-recovered
                                new-recovered
                                total-deaths
                                new-deaths
                                total-confirmed
                                new-confirmed
                                timestamp)))))))

