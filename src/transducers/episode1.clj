(ns transducers.episode
  (:import [java.lang StringBuilder]
           (java.io ByteArrayInputStream)))



(def data (vec (range 10)))

(defn -map [f coll]
  (reduce
    (fn [acc v]
      (conj acc (f v)))
    []
    coll))

(defn -filter [f coll]
  (reduce
    (fn [acc v]
      (if (f v)
        (conj acc v)
        acc))
    []
    coll))



(defn -mapping [f]
  (fn [xf]
    (fn
      ([] (xf))
      ([acc] (xf acc))
      ([acc v]
       (xf acc (f v))))))

(defn -filtering [f]
  (fn [xf]
    (fn
      ([] (xf))
      ([acc] (xf acc))
      ([acc v]
       (if (f v)
         (xf acc v)
         acc)))))

(def xform (comp
           (-mapping int)
           (-mapping inc)
           (-filtering odd?)
           (-mapping char)
           ))



(reduce (rfn +)
        0
        data)

(defn string-rf
  ([^StringBuilder acc ^Character ch]
   (.append acc ch)))

(defn string-rf
  ([] (StringBuilder.))
  ([^StringBuilder sb]
   (.toString sb))
  ([^StringBuilder sb ^Character ch]
   (.append sb ch)))

(defn vec-trans
  ([] (transient []))
  ([acc] (persistent! acc))
  ([acc val]
   (conj! acc val)))

(transduce xform vec-trans "Hello World")

(let [f (rfn string-rf)]
  (f (reduce f (f) "Hello World")))

(str (reduce (rfn string-rf)
             (StringBuilder.)
             "Hello World"))


(extend-protocol clojure.core.protocols/CollReduce
  java.io.InputStream
  (coll-reduce
    ([this f init]
    (let [is ^java.io.InputStream this]
      (loop [acc init]
        (if (reduced? acc)
          @acc
          (let [ch (.read is)]
            (if (= ch -1)
              acc
              (recur (f acc ch))))))))))

(reduce conj
        (java.io.ByteArrayInputStream.
          (.getBytes "Hello World")))

(transduce (comp (map char)
                 (map #(Character/toUpperCase %))
                 (partition-all 2)
                 (take 10))
           conj
           (java.io.ByteArrayInputStream.
             (.getBytes "Hello World")))








