(in-package :cl-user)
(defpackage :cl-covid19.api
  (:nicknames :covid19.api)
  (:use :cl)
  (:import-from
   :alexandria
   :doplist)
  (:import-from :dexador)
  (:import-from :quri)
  (:import-from :jonathan)
  (:import-from :ascii-table)
  (:import-from :log4cl)
  (:export
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
   :api-version))
(in-package :cl-covid19.api)

(defparameter *endpoint*
  "api.covid19api.com"
  "The API from which to fetch data about COVID-19")

(defclass api-client ()
  ((scheme
    :initarg :scheme
    :initform "https"
    :accessor api-client-scheme
    :documentation "URI scheme to use")
   (port
    :initarg :port
    :initform 443
    :accessor api-client-port
    :documentation "Service port to connect to")
   (endpoint
    :initarg :endpoint
    :initform *endpoint*
    :accessor api-client-endpoint
    :documentation "API endpoint to interface with"))
  (:documentation "Client for interfacing with COVID-19 API"))

(defun make-api-client (&rest rest)
  "Creates a new API client"
  (apply #'make-instance 'api-client rest))

(defun make-api-uri (client &key path query)
  "Create new API URI for the given path and query params"
  (quri:make-uri :scheme (api-client-scheme client)
                 :host (api-client-endpoint client)
                 :port (api-client-port client)
                 :path path
                 :query query))

(defun get-api-routes (client &rest rest)
  "Fetch the available API routes"
  (log:debug "Fetching API routes")
  (let* ((uri (make-api-uri client))
         (resp (apply #'dexador:get uri rest)))
    (jonathan:parse resp)))

(defun display-api-routes (client &optional (out *standard-output*))
  "Display the API routes in a table"
  (let* ((routes (get-api-routes client))
         (version (api-version client))
         (table (ascii-table:make-table '("PATH" "NAME") :header (format nil "API Routes @ v~a" version))))
    (doplist (k v routes)
        (ascii-table:add-row table (list (getf v :|Path|)
                                         (getf v :|Name|))))
    (ascii-table:display table out)))

(defun get-countries-data (client &rest rest)
  "Retrieve the list of countries"
  (log:debug "Fetching countries from API")
  (let* ((uri (make-api-uri client :path "/countries"))
         (resp (apply #'dexador:get uri rest)))
    (jonathan:parse resp)))

(defun get-summary-data (client &rest rest)
  "Retrieve summary stats"
  (log:debug "Fetching summary data from API")
  (let* ((uri (make-api-uri client :path "/summary"))
         (resp (apply #'dexador:get uri rest)))
    (jonathan:parse resp)))

(defun get-time-series-archive (client destination &rest rest)
  "Download all time series data as a ZIP archive"
  (log:debug "Fetching time series data as a ZIP archive in ~a" destination)
  (let ((uri (make-api-uri client :path "/export")))
    (apply #'dex:fetch uri destination rest)))

(defun api-version (client &rest rest)
  "Retrieves the remote API version"
  (let* ((uri (make-api-uri client :path "/version"))
         (resp (apply #'dexador:get uri rest)))
    (jonathan:parse resp)))
