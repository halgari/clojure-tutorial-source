(ns test-check.episode2
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :refer [quick-check]]
            [clojure.zip :as z]))

(def commands-gen (gen/vector (gen/elements [:down :up])))

(def data-gen (gen/recursive-gen gen/vector (gen/choose 0 1000)))

(defn test-zipper [data commands]
  (let [result (reduce
                 (fn [z cmd]
                   (when z
                     (case cmd
                       :down (z/down z)
                       :up (z/up z))))
                 (z/vector-zip data)
                 commands)]
    (when result
      (z/node result))))

(defn test-update-in [data commands]
  (let [path (reduce
                 (fn [path cmd]
                   (when path
                     (case cmd
                       :down (if (get-in data path)
                               (conj path 0)
                               nil)
                       :up (if (empty? path)
                             nil
                             (pop path)))))
                 []
                 commands)]
    (when path
      (get-in data path))))


(-> (quick-check
      1000
      (prop/for-all [data data-gen
                     command commands-gen]
                    (= (test-zipper data command)
                       (test-update-in data command))))
    clojure.pprint/pprint)

(def failing [:down :down :down :down :up])
(test-zipper [] failing)
(test-update-in [] failing)

(get-in [:a] [])

(pop [])

