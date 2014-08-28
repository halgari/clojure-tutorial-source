(ns function-of-the-day.fotd-keep)


(remove nil? (map f seq))

(def data (range 10))

(keep odd? data)

(def data {:a "a"
           :b "b"
           :c "c"})

(remove nil? (map data [:a :c :f :g :b]))

(keep data [:a :c :f :g :b])