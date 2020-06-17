(in-package :cl-user)
(defpackage :cl-covid19.gnuplot-template
  (:use :cl)
  (:nicknames :covid19.gnuplot-template)
  (:import-from :djula)
  (:export
   :*gnuplot-with-filled-curves-template*
   :*gnuplot-with-lines-template*
   :render-gnuplot-template))
(in-package :cl-covid19.gnuplot-template)

(djula:add-template-directory (asdf:system-relative-pathname :cl-covid19 "templates/"))

(defparameter *gnuplot-with-filled-curves-template*
  (djula:compile-template* "filled-curves.plt")
  "gnuplot(1) template for plotting time-series data with filled curves")

(defparameter *gnuplot-with-lines-template*
  (djula:compile-template* "lines.plt")
  "gnuplot(1) template for plotting time-series data with lines")

(defun render-gnuplot-template (template &rest rest)
  "Renders the given gnuplot(1) template"
  (apply #'djula:render-template* template nil rest))
