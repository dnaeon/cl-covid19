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
   :persist-summary-data)
  (:import-from
   :cl-covid19.api
   :get-countries-data
   :get-summary-data)
  (:export
   :update-db-countries-data
   :update-db-summary-data
   :update-db-all-data
   :display-table))
(in-package :cl-covid19.core)

(defun update-db-countries-data (api-client db-conn)
  "Updates the local database with the latest countries data from the remote API"
  (log:debug "Updating database with latest countries data from remote API")
  (persist-countries-data (get-countries-data api-client) db-conn))

(defun update-db-summary-data (api-client db-conn)
  "Updates the local database with the latest summary data from the remote API"
  (log:debug "Updating database with latest summary data from remote API")
  (persist-summary-data (getf (get-summary-data api-client) :|Countries|) db-conn))

(defun update-db-all-data (api-client db-conn)
  "Updates the local data with the data retrieved from the remote API"
  (log:debug "Updating database with latest data from remote API")
  (update-db-countries-data api-client db-conn)
  (update-db-summary-data api-client db-conn))

(defun display-table (items)
  "Displays a table view of the items. Useful when used in combination with COVID19:DB-EXECUTE results"
  (let* ((columns (mapcar #'string
                          (plist-keys (first items))))
         (table (ascii-table:make-table columns)))
    (dolist (item items)
      (ascii-table:add-row table (plist-values item)))
    (ascii-table:display table)))
