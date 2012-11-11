(ns coding.utils
  (:use [coding.multisets :only (multiset?)]))

(defn random-elt [coll]
  ;; not very efficient...
  (cond (empty? coll) nil
        (map? coll) (random-elt (into [] coll))
        (multiset? coll) (coding.multisets/random-elt coll)
        (set? coll) (random-elt (into [] coll))
        :else (nth coll (rand-int (count coll)))))

(defn subst [key-vals, in]
  (if (coll? in)
    (if (empty? in)
      in
      (conj (subst key-vals (pop in))
            (subst key-vals (peek in))))
    (or (key-vals in) in)))
