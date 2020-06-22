(in-package :cl-user)
(defpackage :cl-covid19.core
  (:use :cl)
  (:nicknames :covid19.core)
  (:import-from :ascii-table)
  (:import-from :cl-csv)
  (:import-from :log4cl)
  (:import-from
   :cl-covid19.util
   :plist-keys
   :plist-values)
  (:import-from
   :cl-covid19.db
   :persist-countries-data
   :persist-time-series-data
   :db-execute
   :table-columns)
  (:import-from
   :cl-covid19.api
   :get-countries-data
   :get-time-series-for-country)
  (:import-from
   :cl-covid19.gnuplot-template
   :*gnuplot-time-series-with-filled-curves-template*
   :*gnuplot-histograms-per-country-template*
   :render-gnuplot-template)
  (:export
   :*default-result-limit*
   :update-countries-data
   :update-time-series-data
   :update-all-data
   :write-csv
   :display-table
   :fetch-country
   :fetch-countries
   :fetch-time-series
   :fetch-time-series-latest
   :fetch-time-series-for-country
   :fetch-time-series-global
   :fetch-top-countries-by
   :plot-data
   :plot-time-series-for-country
   :plot-time-series-global
   :plot-top-countries-by))
(in-package :cl-covid19.core)

(defparameter *default-result-limit*
  10
  "The default number of items to return when fetching data from the database")

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
    (when items
      (ascii-table:display table))))

(defun write-csv (items &key stream (include-headers t))
  "Writes the given items in CSV format"
  (let ((headers (plist-keys (first items)))
        (output (or stream (make-string-output-stream))))
    (when include-headers
      (cl-csv:write-csv-row headers :stream output))
    (dolist (item items)
      (cl-csv:write-csv-row (plist-values item) :stream output))
    (unless stream
      (get-output-stream-string output))))

(defun fetch-country (db-conn name)
  "Fetch a country by name from the database"
  (log:debug "Fetching country ~a from database" name)
  (let ((query (format nil "SELECT * FROM country ~
                            WHERE ~
                              LOWER(name) = LOWER($1) ~
                            OR ~
                              LOWER(iso_code) = LOWER($1) ~
                            OR ~
                              LOWER(slug) = LOWER($1)")))
    (db-execute db-conn query name)))

(defun fetch-countries (db-conn &key (limit *default-result-limit*) (offset 0))
  "Fetch countries from the database"
  (log:debug "Fetching countries from database")
  (db-execute db-conn "SELECT * FROM country LIMIT $1 OFFSET $2" limit offset))

(defun fetch-time-series (db-conn &key (limit *default-result-limit*) (offset 0))
  "Fetches time series data from the database"
  (log:debug "Fetching time series from database")
  (let ((query (format nil "SELECT * ~
                            FROM time_series_per_country ~
                            ORDER BY timestamp DESC ~
                            LIMIT $1 ~
                            OFFSET $2")))
  (db-execute db-conn query limit offset)))

(defun fetch-time-series-latest (db-conn &key (limit *default-result-limit*) (offset 0))
  "Fetch latest time series data from the database"
  (log:debug "Fetching latest time series from database")
  (let ((query (format nil "SELECT * ~
                            FROM time_series_per_country_latest ~
                            LIMIT $1 ~
                            OFFSET $2")))
    (db-execute db-conn query limit offset)))

(defun fetch-time-series-for-country (db-conn name &key (limit *default-result-limit*) (offset 0))
  "Fetch time series data for a given country from the database"
  (log:debug "Fetching time series for country ~a from database" name)
  (let ((query (format nil "SELECT * ~
                            FROM time_series_per_country ~
                            WHERE ~
                                LOWER(country_name) = LOWER($1) ~
                            OR ~
                                LOWER(country_iso_code) = LOWER($1) ~
                            OR ~
                                LOWER(country_slug) = LOWER($1) ~
                            ORDER BY timestamp DESC ~
                            LIMIT $2 ~
                            OFFSET $3")))
    (db-execute db-conn query name limit offset)))

(defun fetch-time-series-global (db-conn &key (limit *default-result-limit*) (offset 0))
  "Fetch global time series data from the database"
  (log:debug "Fetching global time series data from database")
  (let ((query (format nil "SELECT * ~
                            FROM time_series_global ~
                            ORDER BY timestamp DESC ~
                            LIMIT $1 ~
                            OFFSET $2")))
    (db-execute db-conn query limit offset)))

(defun fetch-top-countries-by (db-conn &key (column :confirmed) (limit *default-result-limit*) (offset 0))
  "Fetch top latest countries from the database, sorted by the given column"
  (log:debug "Fetching latest top countries by ~a column" column)
  (assert (member column
                  (table-columns db-conn "time_series_per_country_latest")
                  :test #'string-equal)
          (column))
  (let ((query (format nil "SELECT * ~
                            FROM time_series_per_country_latest ~
                            ORDER BY ~a DESC ~
                            LIMIT $1 ~
                            OFFSET $2" (string column))))
    (db-execute db-conn query limit offset)))

(defun plot-time-series-for-country (db-conn country &key (template *gnuplot-time-series-with-filled-curves-template*) (limit *default-result-limit*))
  "Plot time series data for a given country"
  (log:debug "Plotting time series data for country ~a" country)
  (plot-data (lambda ()
               (fetch-time-series-for-country db-conn country :limit limit))
             template
             :title country))

(defun plot-time-series-global (db-conn &key (template *gnuplot-time-series-with-filled-curves-template*) (limit *default-result-limit*))
  "Plot global time series"
  (log:debug "Plotting global time series")
  (plot-data (lambda ()
               (fetch-time-series-global db-conn :limit limit))
             template
             :title "Global"))

(defun plot-top-countries-by (db-conn &key (column :confirmed) (limit *default-result-limit*) (template *gnuplot-histograms-per-country-template*))
  "Plot time series per country sorted by given column"
  (log:debug "Plotting top countries by ~a" column)
  (plot-data (lambda ()
               (fetch-top-countries-by db-conn :column column :limit limit))
             template
             :title (format nil "Top countries by ~a" column)))

(defun plot-data (data-fun template &rest rest)
  "Plot the data returned by DATA-FUN using the given TEMPLATE"
  (tmpdir:with-tmpdir (tmpdir)
    (let* ((data-file (merge-pathnames (make-pathname :name "covid19" :type "dat") tmpdir))
           (plt-file (merge-pathnames (make-pathname :name "gnuplot" :type "plt") tmpdir))
           (data-points (funcall data-fun))
           (plt-script (apply #'render-gnuplot-template template :datafile data-file rest)))
      (unless data-points
        (error "No data points returned by function"))
      (with-open-file (out data-file :direction :output)
        (write-csv data-points :stream out :include-headers t))
      (with-open-file (out plt-file :direction :output)
        (write-string plt-script out))
      (uiop:run-program (list "gnuplot"
                              "-p"
                              (namestring plt-file)))
      nil)))
