(ns mu-kanren-transducers
  (:refer-clojure :exclude [==]))

(defn lvar
  ([] (lvar ""))
  ([nm] (gensym (str nm "_"))))

(defn lvar? [v]
  (symbol? v))

(defn walk [s u]
  (if-let [pr (get s u)]
    (if (lvar? pr)
      (recur s pr)
      pr)
    u))

(defn unify [s u v]
  (let [u (walk s u)
        v (walk s v)]
    (cond
      (and (lvar? u)
           (lvar? v)
           (= u v)) s
      (lvar? u) (assoc s u v)
      (lvar? v) (assoc s v u)
      :else (and (= u v) s))))

(defn == [a b]
  (keep (fn [s]
          (if-let [v (unify s a b)]
            v
            nil))))

(def -conj comp)

(defn foreach-xform [& xforms]
  (fn [xf]
    (let [xform-term (fn
                       ([result] result)
                       ([result value] (xf result value)))
          xforms (mapv #(% xform-term) xforms)]
      (fn
        ([] (xf))
        ([result] (let [result (reduce
                                 (fn [acc f]
                                   (f acc))
                                 result
                                 xforms)]
                    (xf result)))
        ([result val]
         (reduce (fn [acc f]
                   (f acc val))
                 result
                 xforms))))))

(defn -disj [& goals]
  (apply foreach-xform goals))

(defmacro fresh [lvars & body]
  `(let [~@(mapcat (fn [x]
                     `[~x (lvar ~(name x))])
                   lvars)]
     (-conj ~@body)))

(defn reify-lvars [lvars]
  (map
    (fn [s]
      (mapv #(walk s %) lvars))))


(defmacro run [lvars & body]
  `(let [~@(mapcat (fn [x]
                     `[~x (lvar ~(name x))])
                   lvars)
         v# [~@lvars]]
     (comp (-conj ~@body) (reify-lvars v#) )))

(defn conde [& goals]
  (apply -disj (map (partial apply -conj) goals)))


(defn foo [a b]
  (conde
    [(== a 1)]
    [(== b 1)]))

(def empty-env [{}])

(def program (run [a b]
                   (conde
                     [(== a 1)]
                     [(== a 42)])
                   (conde
                     [(== b 2)]
                     [(== b 1)])))

(second (sequence program empty-env))

(iteration program empty-env)