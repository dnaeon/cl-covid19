(in-package :cl-user)
(defpackage :cl-covid19.core
  (:use :cl)
  (:nicknames :covid19.core)
  (:import-from :log4cl)
  (:import-from
   :cl-covid19.db
   :persist-countries-data
   :persist-summary-data)
  (:import-from
   :cl-covid19.api
   :get-countries-data
   :get-summary-data)
  (:export
   :update-db-data))
(in-package :cl-covid19.core)

(defun update-db-data (api-client db-conn)
  "Updates the local data with the data retrieved from the remote API"
  (log:debug "Updating database with latest data from remote API")
  (persist-countries-data (get-countries-data api-client) db-conn)
  (persist-summary-data (getf (get-summary-data api-client) :|Countries|) db-conn))
