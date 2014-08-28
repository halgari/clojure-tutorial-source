(ns function-of-the-day.fotd-memfn)



(macroexpand '(memfn getBytes ^long x ^int y z))

(set! *warn-on-reflection* true)

(def get-bytes (memfn ^String getBytes))

(map get-bytes  ["foo" "bar" "baz"])
