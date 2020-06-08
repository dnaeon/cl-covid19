(defpackage :cl-covid19-system
  (:use :cl :asdf))
(in-package :cl-covid19-system)

(defsystem "cl-covid19"
  :name "cl-covid19"
  :description "Common Lisp library and utilities for inspecting COVID-19 data"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :homepage "https://github.com/dnaeon/cl-covid19"
  :bug-tracker "https://github.com/dnaeon/cl-covid19"
  :source-control "https://github.com/dnaeon/cl-covid19"
  :long-name "cl-covid19"
  :depends-on (:quri
               :dexador
               :jonathan
               :cl-ascii-table
               :cl-migratum
               :cl-migratum.provider.local-path
               :cl-migratum.driver.sql)
  :components ((:module "core"
                :pathname #P"src/"
                :components ((:file "api")
                             (:file "db")))
               (:module "client-package"
                :pathname #P"src/"
                :depends-on ("core")
                :components ((:file "package"))))
  :in-order-to ((test-op (test-op "cl-covid19.test"))))
