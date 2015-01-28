(ns core-async-tutorials.episode21
  (:require [clojure.core.async :refer [go]])
  (:import [clojure.lang Var]))


(def ^:dynamic *foo* 42)

(alter-var-root #'*foo* + 1)



(defn my-func []
  (set! *foo* 44))

(binding [*foo* nil]
  (my-func))

((fn []
   (do (Var/pushThreadBindings {#'*foo* 43})
       (println *foo*)
       (Var/popThreadBindings))))


(let [f (fn [] *foo*)
      fut (binding [*foo* 43]
           (future (Thread/sleep 1000) (f)))]
  (println "Foo is now:"  *foo*)
  (println "Result: " @fut))


(stack {} {#'a 42} {#'a 42 #'*foo* 43})

