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
   :persist-time-series-data
   :persist-continents-data
   :persist-population-data)
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
   :update-continents-data
   :update-countries-data
   :update-time-series-data
   :update-all-data
   :update-population-data
   :display-table
   :write-csv
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
   :plot-time-series-for-country
   :plot-time-series-for-country-animation
   :plot-time-series-for-continent
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
   :persist-continents-data
   :persist-population-data

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
   :update-continents-data
   :update-countries-data
   :update-time-series-data
   :update-population-data
   :update-all-data
   :display-table
   :write-csv
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
   :plot-time-series-for-country
   :plot-time-series-for-country-animation
   :plot-time-series-for-continent
   :plot-time-series-global
   :plot-time-series-global-animation
   :plot-top-countries-by
   :plot-data))
(in-package :cl-covid19)
