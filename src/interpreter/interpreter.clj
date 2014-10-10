(ns interpreter.interpreter
  (:refer-clojure :except [eval]))




(def special-forms '#{quote})

(defn process-special-form
  [[sym & args]]
  (condp = sym
    'quote (first args)))

(defn eval [form]
  (cond
    (integer? form) form
    (symbol? form) (resolve form)
    (seq? form) (if (contains? special-forms (first form))
                  (process-special-form form)
                  (let [f (eval (first form))
                        args (map eval (next form))]
                    (apply f args)))
    (keyword? form) form
    (vector? form) (mapv eval form)

    :else (assert false (str "can't eval " (type form)))))