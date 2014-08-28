(ns function-of-the-day.fotd-conj)


(merge {:a 1 :b 2} {:c 3 :d 4 :e 5})

(def data {:a {:b {:c 3
                   :d []
                   :e {1 2 3 4}}}})

(defn add-to-data [node val]
  (update-in data [:a :b node] conj val))


(add-to-data [:a :b :c] [:foo :bar])