(defproject core-async-tutorials "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-alpha2"]
                 [org.clojure/core.async "0.1.338.0-5c5012-alpha"]
                 [org.zeromq/jeromq "0.3.4"]
                 [cheshire "5.3.1"]
                 [http-kit "2.1.16"]
                 [org.clojure/data.xml "0.0.7"]
                 [medley "0.2.1"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [com.tbaldridge.hermod "0.1.3"]
                 [org.clojure/clojurescript "0.0-2322"]
                 [com.cemerick/piggieback "0.1.3"]
                 [org.clojure/test.check "0.5.9"]
                 [org.clojure/core.logic "0.8.8"]
                 [quil "2.2.2"]]
  :repositories [["sonatype" {:url "http://oss.sonatype.org/content/repositories/releases"}]]
  :plugins [[com.cemerick/austin "0.1.5"]]
  :profiles {:dev {:plugins [[com.cemerick/austin "0.1.5"]]}}
  )
