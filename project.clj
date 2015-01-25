(defproject burnand "0.1.0-SNAPSHOT"
  :description "A tool for B&B bookkeeping"
  :url "http://reus.io"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/data.json "0.2.3"]
                 [compojure "1.1.5"]
                 [ring/ring-devel "1.1.8"]
                 [ring/ring-core "1.1.8"]
                 [ring/ring-json "0.3.1"]
                 [cheshire "5.1.1"]
                 [http-kit "2.1.12"]
                 [org.clojure/data.csv "0.1.2"]
                 [com.novemberain/monger "1.5.0"]
                 [clj-time "0.6.0"]
                 [enlive "1.1.4"]])
