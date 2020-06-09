(in-package :cl-user)
(defpackage :cl-covid19.util
  (:use :cl)
  (:nicknames :covid19.util)
  (:import-from
   :alexandria
   :doplist)
  (:export
   :plist-keys
   :plist-values))
(in-package :cl-covid19.util)

(defun plist-keys (plist)
  "Returns the keys of a property list"
  (let ((result nil))
    (doplist (k v plist)
        (push k result))
    (nreverse result)))

(defun plist-values (plist)
  "Returns the values of a property list"
  (let ((result nil))
    (doplist (k v plist)
        (push v result))
    (nreverse result)))
