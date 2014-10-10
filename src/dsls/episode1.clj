(ns dsls.episode1
  (:require [quil.core :as q]))

(defn setup []
  (q/smooth)                          ;; Turn on anti-aliasing
  (q/frame-rate 1)                    ;; Set framerate to 1 FPS
  (q/background 200))                 ;; Set the background colour to
;; a nice shade of grey.

(defn clear []
  (q/stroke 0)
  (q/fill 128)
  (q/stroke-weight 0)
  (q/rect 0 0 (q/width) (q/height)))

(defmacro create-program [& args]
  (let [commands (->> (partition 2 args)
                      (mapv (fn [[cmd amount]]
                             {:op (keyword (.toLowerCase (name cmd)))
                              :amount amount})))]
    commands))

(create-program RIGHT 100 DOWN 100 LEFT 100 UP 100)

(defn square [size]
  {:op       :do
   :commands [{:op     :right
               :amount size}
              {:op     :down
               :amount size}
              {:op     :left
               :amount size}
              {:op     :up
               :amount size}]})


(def program
  [{:op       :do
    :commands (concat (create-program RIGHT 100 DOWN 100 LEFT 100 UP 100)
                       [{:op :right
                         :amount 42}]
                       (create-program RIGHT 10 DOWN 10 LEFT 10 UP 10)
                       )}])

(def add-distance nil)
(defmulti add-distance (fn [acc command]
                         (:op command)))

(defmethod add-distance :do
           [acc {:keys [commands]}]
  (reduce add-distance acc commands))

(defmethod add-distance :default
           [acc {:keys [amount]}]
  (+ acc amount))

(reduce add-distance 0 program)

(def run-command nil)
(defmulti run-command (fn [acc command]
                        (:op command)))

(defmethod run-command :right
           [{[x y :as pos] :pos :as state}
            {:keys [amount]}]
  (let [new-pos [(+ x amount) y]]
    (q/line pos new-pos)
    (assoc state :pos new-pos)))

(defmethod run-command :left
           [{[x y :as pos] :pos :as state}
            {:keys [amount]}]
  (let [new-pos [(- x amount) y]]
    (q/line pos new-pos)
    (assoc state :pos new-pos)))


(defmethod run-command :down
           [{[x y :as pos] :pos :as state}
            {:keys [amount]}]
  (let [new-pos [x (+ y amount)]]
    (q/line pos new-pos)
    (assoc state :pos new-pos)))


(defmethod run-command :up
           [{[x y :as pos] :pos :as state}
            {:keys [amount]}]
  (let [new-pos [x (- y amount)]]
    (q/line pos new-pos)
    (assoc state :pos new-pos)))

(defmethod run-command :do
           [acc {:keys [commands]}]
  (reduce run-command acc commands))


(defn run-program [program]
  (reduce run-command
    {:pos [(/ (q/width) 2)
           (/ (q/height) 2)]}
    program))

(defn draw []
  (clear)
  (q/stroke-weight 4)
  (run-program program)
  )         ;; Draw a circle at x y with the correct diameter

(q/defsketch example                ;; Define a new sketch named example
             :title "Oh so many grey circles"    ;; Set the title of the sketch
             :setup #'setup                        ;; Specify the setup fn
             :draw #'draw                          ;; Specify the draw fn
             :features [:keep-on-top]
             :size [422 400])                    ;; You struggle to beat the golden ratio