(defpackage :cl-covid19-system
  (:use :cl :asdf))
(in-package :cl-covid19-system)

(defsystem "cl-covid19"
  :name "cl-covid19"
  :description "Common Lisp library and utilities for inspecting COVID-19 data"
  :version "0.3.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :homepage "https://github.com/dnaeon/cl-covid19"
  :bug-tracker "https://github.com/dnaeon/cl-covid19"
  :source-control "https://github.com/dnaeon/cl-covid19"
  :long-name "cl-covid19"
  :depends-on (:alexandria
               :quri
               :dexador
               :jonathan
               :cl-csv
               :cl-ascii-table
               :cl-migratum
               :cl-migratum.provider.local-path
               :cl-migratum.driver.sql
               :tmpdir
               :djula)
  :components ((:module "migrations"
                :pathname #P"migrations/"
                :components ((:static-file "20200608135822-create_countries_table.down.sql")
                             (:static-file "20200608135822-create_countries_table.up.sql")
                             (:static-file "20200611230626-add_time_series_table.down.sql")
                             (:static-file "20200611230626-add_time_series_table.up.sql")
                             (:static-file "20200611234716-add_time_series_views.down.sql")
                             (:static-file "20200611234716-add_time_series_views.up.sql")
                             (:static-file "20201120141202-add_continent_table.down.sql")
                             (:static-file "20201120141202-add_continent_table.up.sql")
                             (:static-file "20201120154405-add_continent_to_country_table.down.sql")
                             (:static-file "20201120154405-add_continent_to_country_table.up.sql")
                             (:static-file "20201123170702-fix_time_series_per_continent_view.down.sql")
                             (:static-file "20201123170702-fix_time_series_per_continent_view.up.sql")
                             (:static-file "20201124133919-add_weekly_avg_time_series_per_country.down.sql")
                             (:static-file "20201124133919-add_weekly_avg_time_series_per_country.up.sql")
                             (:static-file "20201125182526-add_population_per_country_latest_view.down.sql")
                             (:static-file "20201125182526-add_population_per_country_latest_view.up.sql")
                             (:static-file "20201125183417-add_cases_per_country_per_million.down.sql")
                             (:static-file "20201125183417-add_cases_per_country_per_million.up.sql")
                             (:static-file "20201125185413-add_incidence_per_country_view.down.sql")
                             (:static-file "20201125185413-add_incidence_per_country_view.up.sql")))
               (:module "gnuplot-templates"
                :pathname #P"templates/"
                :components ((:static-file "histograms-per-country.plt")
                             (:static-file "time-series-with-filled-curves-new-cases.plt")
                             (:static-file "time-series-with-filled-curves.plt")
                             (:static-file "time-series-with-lines-new-cases.plt")
                             (:static-file "time-series-with-lines.plt")))
               (:module "sample-database"
                :pathname #P"databases/"
                :components ((:static-file "covid19.db")))
               (:module "misc"
                :pathname #P"misc/"
                :components ((:static-file "continent-codes.json")
                             (:static-file "country-and-continent-codes-list.json")
                             (:static-file "UN_Population_2019.csv")))
               (:module "core"
                :pathname #P"src/"
                :depends-on ("migrations" "gnuplot-templates" "misc" "sample-database")
                :serial t
                :components ((:file "util")
                             (:file "api")
                             (:file "db")
                             (:file "gnuplot-template")
                             (:file "core")))
               (:module "client-package"
                :pathname #P"src/"
                :depends-on ("core")
                :components ((:file "package")))))
