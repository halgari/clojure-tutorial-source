(ns logic-tutorials.episode
  (:refer-clojure :exclude [==])
  (:require [medley.core :refer [interleave-all]]
            [clojure.set :refer [intersection]]
            [clojure.math.combinatorics :refer [cartesian-product]]))

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


(defn unify [s u' v']
  (let [u (walk s u')
        v (walk s v')]
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



(defn conde [& goals]
  (apply -disj (map (partial apply -conj-set) goals)))

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
     (== q #{1 2 3})
     (== q #{2 3 4}))

(def data
  [[:fernand :kills :fernand]
   [:fernand :kills :ali]
   [:fernand :father :albert]
   [:mercedes :mother :albert]
   [:albert :duels :edmond]
   [:mercedes :husband :fernand]
   [:fernand :wife :mercedes]
   [:mercedes :cousin :fernand]
   [:fernand :cousin :mercedes]
   [:louis :father :edmond]
   [:edmond :saves :pierre]
   [:pierre :employer :edmond]
   [:pierre :father :maximillien]
   [:pierre :father :julie]])

(defn debug [x]
  (clojure.pprint/pprint x)
  x)


(defn relation [e k v]
  (fn [s]

    (let [e (walk s e)
          k (walk s k)
          v (walk s v)]
      (->> (for [x data]
             (unify s (list* x) (list e k v)))
           (remove false?)
           not-empty))))

(defn merge-unifiers [[acc other & others]]
  (if other
    (let [ks (intersection (set (keys acc))
                           (set (keys other)))]
      (if (empty? ks)
        (recur (cons (merge acc other) others))
        (if (= (select-keys acc ks)
               (select-keys other ks))
          (recur (cons (merge acc other) others))
          nil)))
    acc))

(defn -conj-set [& goals]
  (fn [s]
    (let [unifiers (pmap #(% s) goals)]
      (->> (pmap
             merge-unifiers
             (apply cartesian-product unifiers))
           (remove nil?)
           (doall)))))

(dotimes [x 100]
  (time
   (doall (run [a b c]
               (-conj-set
                 (relation a :father b)
                 (relation c :cousin a)
                 (relation c :mother b))))))

(run [a b c]
     (-conj-set
       (relation a :father b)
       (relation c :cousin a)
       (relation c :mother b)))

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
