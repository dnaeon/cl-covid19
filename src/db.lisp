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
   :migrate-db))
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
