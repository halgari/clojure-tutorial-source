(ns core-async-tutorials.scratch)


(defn eve-market-relay []
  (let [out (chan 1024)]
    (let [context (ZMQ/context 1)
          subscriber (.socket context ZMQ/SUB)]
      (.connect subscriber "tcp://relay-us-central-1.eve-emdr.com:8050")
      (.subscribe subscriber (byte-array 0))
      (thread
        (try
          (loop []
            (let [data (.recv subscriber)]
              #_(println "received data")
              (when (>!! out data)
                (recur))))
          (catch Throwable ex
            (println "Error " (pr-str ex))))
        (close! out)
        (println "Shutting down ZMQ")
        (.term context))
      out)))

(defn inflater [data]
  (let [inflater (Inflater.)
        decompressed (byte-array (* (alength data) 16))
        _ (.setInput inflater data)
        decompressed-size (.inflate inflater decompressed)
        output (byte-array decompressed-size)]
    (System/arraycopy decompressed 0 output 0 decompressed-size)
    (String. output "UTF-8")))

(time
  (let [c (eve-market-relay)]
    (println (:rows (first (:rowsets (parse-string (inflater (<!! c)) true)))))
    (close! c)))

(def c (eve-market-relay))

(let [pipe (pipeline< [2 inflater
                       2 #(parse-string % true)
                       1 (comp :rows first :rowsets)]
                      c)]
  (println (<!! (async/into [] (async/take 4 pipe))))
  (println (<!! pipe))
  (close! c))

(close! c)
