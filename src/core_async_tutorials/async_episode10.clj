(ns core-async-tutorials.episode10
  (:require [clojure.core.async :as async :refer [<!! >!! go <! >! alts! close! chan timeout thread]]
            [cheshire.core :refer [parse-string]]
            [clojure.core.async.impl.protocols :as impl]
            [org.httpkit.client :as http]
            [clojure.data.xml :as data.xml])
  (:import (org.zeromq ZContext ZMQ))
  (:import (java.util.zip Inflater)))



1. take at least on