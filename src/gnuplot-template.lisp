(in-package :cl-user)
(defpackage :cl-covid19.gnuplot-template
  (:use :cl)
  (:nicknames :covid19.gnuplot-template)
  (:import-from :djula)
  (:export
   :*gnuplot-time-series-with-filled-curves-template*
   :*gnuplot-time-series-with-lines-template*
   :*gnuplot-histograms-per-country-template*
   :*gnuplot-time-series-with-filled-curves-new-cases-template*
   :*gnuplot-time-series-with-lines-new-cases-template*
   :render-gnuplot-template))
(in-package :cl-covid19.gnuplot-template)

(djula:add-template-directory (asdf:system-relative-pathname :cl-covid19 "templates/"))

(defparameter *gnuplot-time-series-with-filled-curves-template*
  (djula:compile-template* "time-series-with-filled-curves.plt")
  "gnuplot(1) template for plotting time-series data with filled curves")

(defparameter *gnuplot-time-series-with-lines-template*
  (djula:compile-template* "time-series-with-lines.plt")
  "gnuplot(1) template for plotting time-series data with lines")

(defparameter *gnuplot-histograms-per-country-template*
  (djula:compile-template* "histograms-per-country.plt")
  "gnuplot(1) template for plotting histograms per country")

(defparameter *gnuplot-time-series-with-filled-curves-new-cases-template*
  (djula:compile-template* "time-series-with-filled-curves-new-cases.plt")
  "gnuplot(1) template for plotting time-series data of new cases with filled curves")

(defparameter *gnuplot-time-series-with-lines-new-cases-template*
  (djula:compile-template* "time-series-with-lines-new-cases.plt")
  "gnuplot(1) template for plotting time-series data of new cases with lines")

(defun render-gnuplot-template (template &rest rest)
  "Renders the given gnuplot(1) template"
  (apply #'djula:render-template* template nil rest))
