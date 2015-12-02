(defproject cspbox-trading "0.1.2"
  :description "Part of cspbox trading system for build standalone daemon"
  :url "https://github.com/mikewin/cspbox-trading"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                ; [clj-ta-lib "0.1.0"]
                 [com.taoensso/timbre "4.1.4"]
                 [com.taoensso/carmine "2.12.0"]
                 [clj-time "0.11.0"]
                 [clj-http "2.0.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [semantic-csv "0.1.0"]
                 [cspbox-runtime "0.1.1"]])
