(in-package :cl-user)
(defpackage :cl-covid19.api
  (:nicknames :covid19.api)
  (:use :cl)
  (:import-from :dexador)
  (:import-from :quri)
  (:import-from :jonathan)
  (:import-from :ascii-table)
  (:export
   :api-client
   :api-client-scheme
   :api-client-port
   :api-client-endpoint
   :make-api-client
   :make-api-uri
   :get-api-routes
   :display-api-routes
   :get-api-countries
   :get-api-summary))
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
  (let* ((uri (make-api-uri client))
         (resp (apply #'dexador:get uri rest)))
    (jonathan:parse resp :as :hash-table)))

(defun display-api-routes (client &optional (out *standard-output*))
  "Display the API routes in a table"
  (let ((routes (get-api-routes client))
        (table (ascii-table:make-table '("PATH" "NAME") :header "API Routes")))
    (maphash (lambda (k v)
               (declare (ignore k))
               (ascii-table:add-row table (list (gethash "Path" v)
                                                (gethash "Name" v))))
             routes)
    (ascii-table:display table out)))

(defun get-api-countries (client &rest rest)
  "Retrieve the list of countries"
  (let* ((uri (make-api-uri client :path "/countries"))
         (resp (apply #'dexador:get uri rest)))
    (jonathan:parse resp)))

(defun get-api-summary (client &rest rest)
  "Get summary stats per country"
  (let* ((uri (make-api-uri client :path "/summary"))
         (resp (apply #'dexador:get uri rest)))
    (jonathan:parse resp)))
