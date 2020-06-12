(in-package :cl-user)
(defpackage :cl-covid19.core
  (:use :cl)
  (:nicknames :covid19.core)
  (:import-from :ascii-table)
  (:import-from :log4cl)
  (:import-from
   :cl-covid19.util
   :plist-keys
   :plist-values)
  (:import-from
   :cl-covid19.db
   :persist-countries-data
   :persist-time-series-data
   :db-execute)
  (:import-from
   :cl-covid19.api
   :get-countries-data
   :get-time-series-for-country)
  (:export
   :update-countries-data
   :update-time-series-data
   :update-all-data
   :display-table))
(in-package :cl-covid19.core)

(defun update-countries-data (api-client db-conn)
  "Updates the local database with the latest countries data from the remote API"
  (log:debug "Updating database with latest countries data from remote API")
  (persist-countries-data (get-countries-data api-client) db-conn)
  t)

(defun update-all-data (api-client db-conn)
  "Updates the local database with the data retrieved from the remote API"
  (log:debug "Updating database with latest data from remote API")
  (update-countries-data api-client db-conn)
  (update-time-series-data api-client db-conn)
  t)

(defun update-time-series-data (api-client db-conn)
  "Updates the local database with the latest time series data from the remote API"
  (log:debug "Updating database with latest time series data from remote API")
  (let ((items (db-execute db-conn "SELECT slug FROM country")))
    (dolist (item items)
      (let* ((country (getf item :|slug|))
             (time-series (get-time-series-for-country api-client country)))
        (persist-time-series-data time-series db-conn))))
  t)

(defun display-table (items)
  "Displays a table view of the items. Useful when used in combination with COVID19:DB-EXECUTE results"
  (let* ((columns (mapcar #'string
                          (plist-keys (first items))))
         (table (ascii-table:make-table columns)))
    (dolist (item items)
      (ascii-table:add-row table (plist-values item)))
    (ascii-table:display table)))
