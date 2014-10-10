(ns core-async-tutorials.episode15
  (:require [clojure.core.async
             :as async
             :refer [go <! >! <!! >!! chan dropping-buffer
                     put!]]
             ))


;; Actor can:
;; 1) send a finite number of msgs to other actors
;; 2) create a finite number of actors
;; 3) become another actor
;; I) recv a msg


(defn ! [msgbox v]
  (put! msgbox v))

(defmacro actor [f]
  `(let [msgbox# (chan (dropping-buffer 32))
         ~'self msgbox#]
     (go (loop [f# ~f]
           (let [v# (<! msgbox#)]
             (recur (f# v#)))))
     msgbox#))

(let [a (actor (fn b [m]
                 (println m)
                 b))
      c (actor (fn c [m]
                 (let [d (actor (fn e [m]
                                  (! a (inc m))
                                  e))]
                   (! d m))
                 (! a m)
                 (! a m)
                 c))]
  (>!! c 42)
  (>!! c 43))

(let [a (actor (fn inc-actor
                 ([m] (inc-actor 0 m))
                 ([i m]
                  (println i m)
                  (partial inc-actor (inc i)))))]
  (! a 1)
  (! a 2)
  (! a 1)
  (! a 1))

(defn tagging-actor [tag to]
  (actor (fn tag-a
           [msg]
           (! to {:from tag :msg msg})
           tag-a)))

(defn multi-input-actor []
  (let [master (actor (fn master
                        [{:keys [from msg]}]
                        (println "Got " msg " from " from)
                        master))
        input-a (tagging-actor :input-a master)
        input-b (tagging-actor :input-b master)]
    [input-a input-b]))

(let [[input-a input-b] (multi-input-actor)]
  (! input-a 42)
  (! input-b 44))


(def uni (actor (fn uni [msg]
                  (cond
                    (= (:op msg) :init) (:fn msg)
                    :else uni))))

(def counter (actor (fn counter
                      ([msg]
                       (counter 0 msg))
                      ([cnt msg]
                       (case (:op msg)
                         :reset (partial counter 0)
                         :inc (partial counter (inc cnt))
                         :print (do (println "The Count:" cnt)
                                    (partial counter cnt))
                         :get (do (! (:to msg) cnt)
                                  (partial counter cnt)))))))

(! counter {:op :print})
(! counter {:op :inc})
(! counter {:op :reset})

(let [a (actor (fn [_]
                 (! counter {:op :reset})
                 (! counter {:op :inc})
                 (! counter {:op :inc})
                 (! counter {:op :get
                             :to self})
                 (fn print [msg]
                   (println "Got back Actor:" msg)
                   print)))]
  (! a :something))



(! uni {:op :init
        :fn (fn print-msg [msg]
              (println msg)
              print-msg)})

(! uni 42)









