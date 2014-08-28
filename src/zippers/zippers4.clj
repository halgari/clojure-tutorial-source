(ns zippers-rewrite
  (:refer-clojure :exclude [replace next]))

(set! *warn-on-reflection* true)

(defrecord Zipper [branch?-fn children-fn make-node-fn node
                   pnodes l r ppath path changed? at-end?])


(defn zipper [branch? children make-node root]
  (->Zipper branch? children make-node root nil nil nil nil nil nil nil))

(defn seq-zip
  [root]
  (zipper seq?
          identity
          (fn [node children] (with-meta children (meta node)))
          root))

(defn node [^Zipper z]
  (.node z))

(defn branch? [^Zipper z]
  ((.branch?-fn z) (node z)))

(defn children [^Zipper z]
  (if (branch? z)
    ((.children-fn z) (node z))))

(defn make-node [^Zipper z node children]
  ((.make-node-fn z) node children))

(defn path [^Zipper z]
  (.pnodes z))

(defn lefts [^Zipper z]
  (.l z))

(defn rights [^Zipper z]
  (.r z))


(defn down [^Zipper z]
  (when (branch? z)
    (let [[c & cnext :as cs] (children z)]
      (when cs
        (assoc z :node c
                 :l []
                 :pnodes (if (.path z)
                           (conj (.pnodes z) (.node z))
                           [(.node z)])
                 :ppath z
                 :r cnext)))))

(defn up [^Zipper z]
  (when (.pnodes z)
    (let [pnode (peek (.pnodes z))
          ppath (.ppath z)]
      (if (.changed? z)
        (and ppath (assoc ppath
                     :changed? true
                     :node (make-node
                             z
                             (.node z)
                             (concat (.l z)
                                     (cons (.node z)
                                           (.r z))))))
        ppath))))

(defn right
  [^Zipper z]
  (let [[r & rnext :as rs] (.r z)]
    (when rs
      (assoc z :l (conj (.l z) (.node z))
               :r rnext
               :node r))))

(defn left
  [^Zipper z]
  (let [l (.l z)]
    (when (seq l)
      (assoc z :l (pop l)
               :r (cons (.node z) (.r z))
               :node (peek l)))))


(defn root
  [z]
  (let [p (up z)]
    (if p
      (recur p)
      (node z))))


(defn next
  [^Zipper z]
  (if (.at-end? z)
    z
    (or (and (branch? z) (down z))
        (right z)
        (loop [p z]
          (if (up p)
            (or (right (up p))
                (recur (up p)))
            (assoc p :at-end? true))))))


(defn end? [^Zipper z]
  (.at-end? z))

(defn replace [^Zipper z node]
  (assoc z :node node :changed? true))


(def data '((1 (2 2.5) 3)
            (4 5 6)))

(defn zipper-seq [z]
  (when-not (end? z)
    (cons z (lazy-seq (zipper-seq (next z))))))

(->> (seq-zip data) zipper-seq (map node))

