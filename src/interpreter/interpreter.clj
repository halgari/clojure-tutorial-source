(ns interpreter.interpreter
  (:refer-clojure :except [eval]))




(def special-forms '#{quote fn})

'(fn [x y] body)

'{x 1 y 2}

(declare -eval)

(defn create-function [locals body]
  (assert (= (count body) 2))
  (let [[arg-names fn-body] body]
    (fn [& args]
      (let [new-locals (merge locals
                              (zipmap arg-names args))]
        (-eval new-locals fn-body)))))

(defn process-special-form
  [locals [sym & args]]
  (condp = sym
    'quote (first args)
    'fn (create-function locals args)
    ))

(defn -eval [locals form]
  (cond
    (integer? form) form
    (symbol? form) (let [v (get locals form ::fail)]
                     (if (identical? v ::fail)
                       (resolve form)
                       v))
    (seq? form) (if (contains? special-forms (first form))
                  (process-special-form locals form)
                  (let [f (-eval locals (first form))
                        args (map (partial -eval locals)
                                  (next form))]
                    (apply f args)))
    (keyword? form) form
    (vector? form) (mapv (partial -eval locals) form)

    :else (assert false (str "can't eval " (type form)))))