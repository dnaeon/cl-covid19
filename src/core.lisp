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
(defpackage :cl-covid19.core
  (:use :cl)
  (:nicknames :covid19.core)
  (:import-from :ascii-table)
  (:import-from :log4cl)
  (:import-from :cl-csv)
  (:import-from :jonathan)
  (:import-from
   :cl-covid19.util
   :plist-keys
   :plist-values)
  (:import-from
   :cl-covid19.db
   :persist-continents-data
   :persist-countries-data
   :persist-time-series-data
   :persist-population-data
   :link-countries-with-continents
   :set-numeric-code-for-countries
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
   :*gnuplot-time-series-animation-template*
   :*gnuplot-time-series-with-lines-template*
   :render-gnuplot-template)
  (:export
   :*default-result-limit*
   :update-continents-data
   :update-countries-data
   :update-time-series-data
   :update-population-data
   :update-all-data
   :write-csv
   :display-table
   :fetch-continent
   :fetch-continents
   :fetch-country
   :fetch-countries
   :fetch-time-series
   :fetch-time-series-latest
   :fetch-time-series-for-country
   :fetch-time-series-global
   :fetch-time-series-for-continent
   :fetch-top-countries-by
   :plot-data
   :plot-time-series-for-country
   :plot-time-series-for-country-animation
   :plot-time-series-for-continent
   :plot-time-series-global
   :plot-time-series-global-animation
   :plot-top-countries-by))
(in-package :cl-covid19.core)

(defparameter *default-result-limit*
  10
  "The default number of items to return when fetching data from the database")

(defparameter *continents-data-path*
  (asdf:system-relative-pathname :cl-covid19 "misc/continent-codes.json")
  "Path to the file which contains the continents data")

(defparameter *countries-and-continents-data-path*
  (asdf:system-relative-pathname :cl-covid19 "misc/country-and-continent-codes-list.json")
  "Path to the file which contains the mapping of countries and continents")

(defparameter *population-data-path*
  (asdf:system-relative-pathname :cl-covid19 "misc/UN_Population_2019.csv")
  "Path to the file which contains population data")

(defun parse-json-data (path)
  "Parses the JSON document from the given PATH"
  (let ((data (alexandria:read-file-into-string path)))
    (jonathan:parse data)))

(defun parse-population-data (path)
  "Parses the CSV file from PATH, which contains world population data from the UN"
  (let* ((result nil)
         (data-as-str (alexandria:read-file-into-string path))
         (rows (cl-csv:read-csv data-as-str))
         (headers-row (first rows))

         ;; Throw away the `Country` and `Country Code` headers
         (headers (mapcar #'parse-integer (nthcdr 2 headers-row))))
    (dolist (row (rest rows)) ;; Skip header values
      (let* ((country-name (first row))
             (country-numeric-code (parse-integer (second row)))
             (data-cells (mapcar #'parse-integer (nthcdr 2 row)))
             (population (mapcar (lambda (year population)
                                   (list :year year
                                         :population (* population 1000)
                                         :country-name country-name
                                         :country-numeric-code country-numeric-code))
                                 headers data-cells)))
        (setf result (concatenate 'list result population))))
    result))

(defun update-population-data (db-conn)
  "Updates the local database with the latest population data"
  ;; Here we filter only the countries for which we know of.  The UN
  ;; population dataset contains more countries than what the COVID19
  ;; API exposes, so we set population only for the known countries.
  (log:debug "Updating database with latest population data")
  (let* ((known-countries (db-execute db-conn "SELECT numeric_code FROM country"))
         (known-country-ids (mapcar (lambda (item)
                                      (getf item :|numeric_code|))
                                    known-countries))
         (all-items (parse-population-data *population-data-path*))
         (filtered-items (remove-if-not (lambda (item)
                                          (member (getf item :country-numeric-code) known-country-ids))
                                        all-items)))
   (persist-population-data filtered-items db-conn)))

(defun update-continents-data (db-conn)
  "Updates the local database with the latest continents data"
  (log:debug "Updating database with latest continents data")
  (let ((items (parse-json-data *continents-data-path*)))
    (persist-continents-data items db-conn)))

(defun update-countries-data (api-client db-conn)
  "Updates the local database with the latest countries data from the remote API"
  (log:debug "Updating database with latest countries data from remote API")
  (persist-countries-data (get-countries-data api-client) db-conn)
  t)

(defun update-all-data (api-client db-conn)
  "Updates the local database with the data retrieved from the remote API"
  (log:debug "Updating database with latest data")
  (update-continents-data db-conn)
  (update-countries-data api-client db-conn)

  ;; The countries-and-continents data is used for filling in the
  ;; numeric code for each country and also for linking the
  ;; countries with their respective continent.
  ;; The COVID19 API doesn't provide numeric code, so we use the
  ;; continents mapping to fill in that gap.
  (let ((items (parse-json-data *countries-and-continents-data-path*)))
    (set-numeric-code-for-countries items db-conn)
    (link-countries-with-continents items db-conn))

  (update-population-data db-conn)
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

(defun fetch-continent (db-conn name)
  "Fetch a continent by name or code from the database"
  (log:debug "Fetching continent ~a from database" name)
  (let ((query (format nil "SELECT * FROM continent ~
                            WHERE ~
                              LOWER(name) = LOWER($1) ~
                            OR
                              LOWER(iso_code) = LOWER($1)")))
    (db-execute db-conn query name)))

(defun fetch-continents (db-conn &key (limit *default-result-limit*) (offset 0))
  "Fetch continents from the database"
  (log:debug "Fetching continents from the database")
  (db-execute db-conn "SELECT * FROM continent LIMIT $1 OFFSET $2" limit offset))

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

(defun fetch-time-series (db-conn &key (limit *default-result-limit*) (offset 0) (order :desc))
  "Fetches time series data from the database"
  (assert (member order (list :asc :desc) :test #'string-equal)
          (order))
  (log:debug "Fetching time series from database")
  (let ((query (format nil "SELECT * ~
                            FROM time_series_per_country ~
                            ORDER BY timestamp ~a ~
                            LIMIT $1 ~
                            OFFSET $2" order)))
  (db-execute db-conn query limit offset)))

(defun fetch-time-series-latest (db-conn &key (limit *default-result-limit*) (offset 0))
  "Fetch latest time series data from the database"
  (log:debug "Fetching latest time series from database")
  (let ((query (format nil "SELECT * ~
                            FROM time_series_per_country_latest ~
                            LIMIT $1 ~
                            OFFSET $2")))
    (db-execute db-conn query limit offset)))

(defun fetch-time-series-for-continent (db-conn name &key (limit *default-result-limit*) (offset 0) (order :desc))
  "Fetch time series data for a given continent from the database"
  (assert (member order (list :asc :desc) :test #'equal)
          (order))
  (log:debug "Fetching time series for continent ~a from database" name)
  (let ((query (format nil "SELECT * ~
                            FROM time_series_per_continent ~
                            WHERE ~
                                LOWER(continent_name) = LOWER($1) ~
                            OR ~
                                LOWER(continent_iso_code) = LOWER($1) ~
                            ORDER BY timestamp ~a ~
                            LIMIT $2 ~
                            OFFSET $3" order)))
    (db-execute db-conn query name limit offset)))

(defun fetch-time-series-for-country (db-conn name &key (limit *default-result-limit*) (offset 0) (order :desc))
  "Fetch time series data for a given country from the database"
  (assert (member order (list :asc :desc) :test #'equal)
          (order))
  (log:debug "Fetching time series for country ~a from database" name)
  (let ((query (format nil "SELECT * ~
                            FROM time_series_per_country ~
                            WHERE ~
                                LOWER(country_name) = LOWER($1) ~
                            OR ~
                                LOWER(country_iso_code) = LOWER($1) ~
                            OR ~
                                LOWER(country_slug) = LOWER($1) ~
                            ORDER BY timestamp ~a ~
                            LIMIT $2 ~
                            OFFSET $3" order)))
    (db-execute db-conn query name limit offset)))

(defun fetch-time-series-global (db-conn &key (limit *default-result-limit*) (offset 0) (order :desc))
  "Fetch global time series data from the database"
  (assert (member order (list :asc :desc) :test #'string-equal)
          (order))
  (log:debug "Fetching global time series data from database")
  (let ((query (format nil "SELECT * ~
                            FROM time_series_global ~
                            ORDER BY timestamp ~a ~
                            LIMIT $1 ~
                            OFFSET $2" order)))
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
                            OFFSET $2" column)))
    (db-execute db-conn query limit offset)))

(defun plot-time-series-for-continent (db-conn continent &key (template *gnuplot-time-series-with-lines-template*) (limit *default-result-limit*))
  "Plot time series data for a given continent"
  (log:debug "Plotting time series data for continent ~a" continent)
  (plot-data (lambda ()
               (fetch-time-series-for-continent db-conn continent :limit limit))
             template
             :title continent))

(defun plot-time-series-for-country (db-conn country &key (template *gnuplot-time-series-with-filled-curves-template*) (limit *default-result-limit*))
  "Plot time series data for a given country"
  (log:debug "Plotting time series data for country ~a" country)
  (plot-data (lambda ()
               (fetch-time-series-for-country db-conn country :limit limit))
             template
             :title country))

(defun plot-time-series-for-country-animation (db-conn country destination &key (limit *default-result-limit*) (delay 10) (height 1280) (width 720) (line-width 4))
  "Plot global time series for a given country as an animation"
  (log:debug "Plotting animation for country ~a" country)
  (plot-data (lambda ()
               (fetch-time-series-for-country db-conn country :limit limit :order :asc))
             *gnuplot-time-series-animation-template*
             :destination (namestring destination)
             :title country
             :delay delay
             :height height
             :width width
             :line-width line-width))

(defun plot-time-series-global (db-conn &key (template *gnuplot-time-series-with-filled-curves-template*) (limit *default-result-limit*))
  "Plot global time series"
  (log:debug "Plotting global time series")
  (plot-data (lambda ()
               (fetch-time-series-global db-conn :limit limit))
             template
             :title "Global"))

(defun plot-time-series-global-animation (db-conn destination &key (limit *default-result-limit*) (delay 10) (height 1280) (width 720) (line-width 4))
  "Plot global time series data as an animation"
  (log:debug "Plotting animation of global time series data")
  (plot-data (lambda ()
               (fetch-time-series-global db-conn :limit limit :order :asc))
             *gnuplot-time-series-animation-template*
             :destination (namestring destination)
             :title "Global"
             :delay delay
             :height height
             :width width
             :line-width line-width))

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
