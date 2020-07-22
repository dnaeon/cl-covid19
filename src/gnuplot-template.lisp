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
   :*gnuplot-time-series-animation-template*
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

(defparameter *gnuplot-time-series-animation-template*
  (djula:compile-template* "time-series-animation.plt")
  "gnuplot(1) template for plotting an animation of time-series data with lines")

(defun render-gnuplot-template (template &rest rest)
  "Renders the given gnuplot(1) template"
  (apply #'djula:render-template* template nil rest))
