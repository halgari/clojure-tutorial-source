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

(defn preserving-reduced
  [f1]
  #(let [ret (f1 %1 %2)]
    (if (reduced? ret)
      (reduced ret)
      ret)))

(def cat
  (fn [xf]
    (let [pr (preserving-reduced xf)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result coll]
          (reduce pr result coll))))))

(def print-stuff
  (map (fn [x]
         (print "-" x "-")
         x)))


(defn take [n]
  (fn [xf]
    (let [left (volatile! n)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result item]
          (if (> @left 0)
            (do (vswap! left dec)
                (xf result item))
            (reduced result)))))))

(type (sequence (map inc) [1 2 3 4]))

(def xform (comp
             (filter even?)
             (partition-all 2)
             cat
             (map str))

(def a (atom [])))

(def rf
  (fn [_ item]
    (swap! a conj item)))

(def f (xform rf))

(reset! a [])

(pr @a)

(f nil 3)

(let [val 4]
  (reset! a [])
  (f nil val)
  @a)

(chan 10 xform)




(require '[clojure.core.async :as async])

(let [c (async/chan 1 (map (fn [x]
                             (assert (odd? x))
                             x))
                    (fn [ex]
                      (println ex)
                      :error))]
  (async/>!! c 2)
  (async/<!! c))





