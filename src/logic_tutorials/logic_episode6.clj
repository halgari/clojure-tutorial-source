(ns logic-tutorials.episode
  (:refer-clojure :exclude [==])
  (:require [medley.core :refer [interleave-all]]))

(defn lvar
  ([] (lvar ""))
  ([nm] (gensym (str nm "_"))))

(defn lvar? [v]
  (symbol? v))

(defn walk [s u]
  (let [pr (get s u ::not-found)]
    (if-not (identical? pr ::not-found)
      (if (lvar? pr)
        (recur s pr)
        pr)
      u)))

(defprotocol ILCons
  (lfirst [this])
  (lnext [this]))

(defn lcons? [x]
  (satisfies? ILCons x))

(extend-type clojure.lang.ISeq
  ILCons
  (lfirst [this]
    (first this))
  (lnext [this]
    (next this)))

(defrecord LCons [h t]
  ILCons
  (lfirst [this]
    h)
  (lnext [this]
    t))

(defn lcons [h t]
  (->LCons h t))


(defn unify [s u v]
  (let [u (walk s u)
        v (walk s v)]
    (cond
      (and (lvar? u)
           (lvar? v)
           (= u v)) s
      (lvar? u) (assoc s u v)
      (lvar? v) (assoc s v u)

      (and (lcons? u) (lcons? v))
      (let [s (unify s (lfirst u) (lfirst v))]
        (and s (recur s (lnext u) (lnext v))))

      :else (and (= u v) s))))

(defn == [a b]
  (fn [s]
    (if-let [v (unify s a b)]
      [v]
      [])))

(defn -conj
  ([a] a)
  ([a b]
   (fn [s]
     (for [aret (a s)
           :when aret
           bret (b aret)
           :when bret]
       bret)))
  ([a b & more]
   (-conj a (apply -conj b more))))

(defn mapinter [f coll]
  (apply interleave-all (map f coll)))

(defn -disj [& goals]
  (fn [s]
    (mapinter (fn [goal]
                (goal s))
              goals)
    #_(mapcat (fn [goal]
              (goal s))
            goals)))

(apply concat (map f coll))


(defn conde [& goals]
  (apply -disj (map (partial apply -conj) goals)))

(defmacro fresh [lvars & goals]
  `(let [~@(vec (mapcat (fn [var]
                          `[~var (lvar ~(name var))])
                        lvars))]
     ~(if (> (count goals) 1)
        `(-conj ~@goals)
        (first goals))))

(defn reify-vars [s lvars]
  (map (fn [s']
         (map (fn [lvar]
                (walk s' lvar)) lvars)) s))

(defmacro run [lvars & goals]
  `(let [lvars# ~(vec (map (fn [var]
                             `(lvar ~(name var)))
                           lvars))
         ~lvars lvars#
         r# (-conj ~@goals)]
     (reify-vars (r# {}) lvars#)))


(defn conso [h t o]
  (== (lcons h t) o))

(defn firsto [h t]
  (fresh [rest]
         (conso h rest t)))

(defn resto [t col]
  (fresh [h]
         (conso h t col)))

(defmacro defer [goal]
  `(fn [s#]
     (~goal s#)))

(defn membero [v col]
  (conde
    [(firsto v col)]
    [(fresh [t]
            (resto t col)
            (defer (membero v t)))]))


(run [q]
     (conde
       [(conde
          [(== q 1)]
          [(== q 11)]
          [(== q 111)]
          [(== q 1111)]
          [(conde
             [(== q 4)]
             [(== q 44)])])]
       [(conde
          [(== q 2)]
          [(== q 22)]
          [(== q 222)])]
       [(== q 3)]))
