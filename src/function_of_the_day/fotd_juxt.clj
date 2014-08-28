(ns function-of-the-day.fotd-juxt)



(def j (juxt inc dec))

(def data
  [{:name :bill :age 42 :state :WI}
   {:name :jim :age 40 :state :NY}
   {:name :jane :age 22 :state :NC}
   {:name :sally :age 55 :state :NC}])

(map (juxt :name :state :age) data)


(sort-by (juxt :state :age) data)

(sort-by #(vector (:state %) (:age %)) data)