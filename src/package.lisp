(in-package :cl-user)
(defpackage :cl-covid19
  (:use :cl)
  (:nicknames :covid19)
  (:import-from
   :cl-covid19.api
   :api-client
   :api-client-scheme
   :api-client-port
   :api-client-endpoint
   :make-api-client
   :make-api-uri
   :get-api-routes
   :display-api-routes
   :get-countries-data
   :get-summary-data
   :get-time-series-archive
   :get-time-series-for-country
   :api-version)
  (:import-from
   :cl-covid19.db
   :*migrations-path*
   :make-db-conn
   :disconnect-db-conn
   :migrate-db
   :db-execute
   :table-info
   :table-columns
   :persist-countries-data
   :persist-time-series-data)
  (:import-from
   :cl-covid19.util
   :plist-keys
   :plist-values)
  (:import-from
   :cl-covid19.gnuplot-template
   :*gnuplot-time-series-with-filled-curves-template*
   :*gnuplot-time-series-with-lines-template*
   :*gnuplot-histograms-per-country-template*
   :*gnuplot-time-series-with-filled-curves-new-cases-template*
   :*gnuplot-time-series-with-lines-new-cases-template*
   :*gnuplot-time-series-animation-template*
   :render-gnuplot-template)
  (:import-from
   :cl-covid19.core
   :*default-result-limit*
   :update-countries-data
   :update-time-series-data
   :update-all-data
   :display-table
   :write-csv
   :fetch-country
   :fetch-countries
   :fetch-time-series
   :fetch-time-series-latest
   :fetch-time-series-for-country
   :fetch-time-series-global
   :fetch-top-countries-by
   :plot-time-series-for-country
   :plot-time-series-global
   :plot-time-series-global-animation
   :plot-top-countries-by
   :plot-data)
  (:export
   ;; api
   :api-client
   :api-client-scheme
   :api-client-port
   :api-client-endpoint
   :make-api-client
   :make-api-uri
   :get-api-routes
   :display-api-routes
   :get-countries-data
   :get-summary-data
   :get-time-series-archive
   :get-time-series-for-country
   :api-version

   ;; db
   :*migrations-path*
   :make-db-conn
   :disconnect-db-conn
   :migrate-db
   :db-execute
   :table-info
   :table-columns
   :persist-countries-data
   :persist-time-series-data

   ;; util
   :plist-keys
   :plist-values

   ;; gnuplot-template
   :*gnuplot-time-series-with-filled-curves-template*
   :*gnuplot-time-series-with-lines-template*
   :*gnuplot-histograms-per-country-template*
   :*gnuplot-time-series-with-filled-curves-new-cases-template*
   :*gnuplot-time-series-with-lines-new-cases-template*
   :*gnuplot-time-series-animation-template*
   :render-gnuplot-template

   ;; core
   :*default-result-limit*
   :update-countries-data
   :update-time-series-data
   :update-all-data
   :display-table
   :write-csv
   :fetch-country
   :fetch-countries
   :fetch-time-series
   :fetch-time-series-latest
   :fetch-time-series-for-country
   :fetch-time-series-global
   :fetch-top-countries-by
   :plot-time-series-for-country
   :plot-time-series-global
   :plot-time-series-global-animation
   :plot-top-countries-by
   :plot-data))
(in-package :cl-covid19)
