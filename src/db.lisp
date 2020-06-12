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
   :persist-time-series-data))
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

(defun persist-time-series-data (items db-conn)
  "Persists the given ITEMS representing time series data"
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
         (prepared (cl-dbi:prepare db-conn time-series-stmt)))
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
