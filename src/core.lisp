(in-package :cl-user)
(defpackage :cl-covid19.core
  (:use :cl)
  (:nicknames :covid19.core)
  (:import-from :ascii-table)
  (:import-from :log4cl)
  (:import-from :zip)
  (:import-from :flexi-streams)
  (:import-from :tmpdir)
  (:import-from :jonathan)
  (:import-from
   :cl-covid19.util
   :plist-keys
   :plist-values)
  (:import-from
   :cl-covid19.db
   :persist-countries-data
   :persist-summary-data
   :persist-time-series-data)
  (:import-from
   :cl-covid19.api
   :get-countries-data
   :get-summary-data
   :get-time-series-archive)
  (:export
   :update-countries-data
   :update-summary-data
   :update-time-series-data
   :update-all-data
   :display-table))
(in-package :cl-covid19.core)

(defun update-countries-data (api-client db-conn)
  "Updates the local database with the latest countries data from the remote API"
  (log:debug "Updating database with latest countries data from remote API")
  (persist-countries-data (get-countries-data api-client) db-conn)
  t)

(defun update-summary-data (api-client db-conn)
  "Updates the local database with the latest summary data from the remote API"
  (log:debug "Updating database with latest summary data from remote API")
  (persist-summary-data (getf (get-summary-data api-client) :|Countries|) db-conn)
  t)

(defun update-all-data (api-client db-conn)
  "Updates the local database with the data retrieved from the remote API"
  (log:debug "Updating database with latest data from remote API")
  (update-countries-data api-client db-conn)
  (update-summary-data api-client db-conn)
  (update-time-series-data api-client db-conn)
  t)

(defun update-time-series-data (api-client db-conn)
  "Updates the local database with the latest time series data from the remote API"
  (log:debug "Updating database with latest time series data from remote API")
  (let* ((tmpdir (tmpdir:mkdtemp))
         (destination (merge-pathnames (make-pathname :name "covid19" :type "zip") tmpdir)))
    (get-time-series-archive api-client destination)
    (log:debug "Time series data fetched successfully, processing items")
    (zip:with-zipfile (zip-archive destination)
      (let* ((zip-entry-name "all.json")
             (zip-entry (zip:get-zipfile-entry zip-entry-name zip-archive))
             (contents (flexi-streams:octets-to-string (zip:zipfile-entry-contents zip-entry)))
             (items (jonathan:parse contents)))
        (persist-time-series-data items db-conn)))
    (uiop:delete-directory-tree tmpdir :validate t)
    t))

(defun display-table (items)
  "Displays a table view of the items. Useful when used in combination with COVID19:DB-EXECUTE results"
  (let* ((columns (mapcar #'string
                          (plist-keys (first items))))
         (table (ascii-table:make-table columns)))
    (dolist (item items)
      (ascii-table:add-row table (plist-values item)))
    (ascii-table:display table)))
