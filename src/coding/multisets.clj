(ns coding.multisets
  (:use [clojure.string :only (join split)]))

(deftype MultiSet [content cnt]
  clojure.lang.IPersistentSet
  (seq [this] (if (= 0 cnt)
                nil
                (apply concat (map (fn [[elt cnt]] (repeat cnt elt))
                                   content))))
  (cons [this elt] (if-let [count (content elt)]
                     (MultiSet. (assoc content elt (+ count 1)) (+ cnt 1))
                     (MultiSet. (assoc content elt 1) (+ cnt 1))))
  (count [this] cnt)
  (empty [this] (MultiSet. {} 0))
  (disjoin [this elt] (if-let [count (content elt)]
                        (if (= 1 count)
                          (MultiSet. (dissoc content elt) (- cnt 1))
                          (MultiSet. (assoc content elt (- count 1)) (- cnt 1)))
                        this))
  (contains [this elt] (contains? content elt))
  (equiv [this other] (= (.content this) (.content other)))
  clojure.lang.Associative
  (containsKey  [this elt] (contains? content elt))
  (valAt [this elt] (or (get content elt) 0))
  (entryAt [this elt] (get content elt))
  (assoc [this elt n]
    (when (< n 0)
      (throw (Exception. "cannot have negative number of elements in a multiset")))
    (if-let [count (content elt)]
      (if (> n 0)
        (MultiSet. (assoc content elt n) (+ (- cnt count) n))
        (MultiSet. (dissoc content elt) (- cnt count)))
      (if (> n 0)
        (MultiSet. (assoc content elt n) (+ cnt n))
        this))))

;; (prefer-method print-method clojure.lang.IPersistentSet clojure.lang.IPersistentMap)

(defn multiset
  ([] (MultiSet. {} 0))
  ([coll]
     (cond (map? coll)
           (MultiSet. coll
                      (reduce (fn [result [elt cnt]] (+ result cnt))
                              0
                              coll))
           :else (reduce conj (MultiSet. {} 0) coll)))
  ([elt & elts] (multiset (conj elts elt))))

(defn union
  ([ms1 ms2]
     (multiset
      (reduce (fn [result [elt cnt]]
                (assoc result elt (+ cnt (get ms1 elt))))
              (.content ms1) (.content ms2))))
  ([ms1 ms2 & msets]
     (reduce union ms1 (conj msets ms2))))

(defn intersection
  ([ms1 ms2]
     (reduce (fn [result [elt cnt]]
               (assoc result elt (min cnt (get ms2 elt))))
             ms1 (.content ms1)))
  ([ms1 ms2 & msets]
     (reduce intersection ms1 (conj msets ms2))))

(defn difference
  ([ms1 ms2]
     (reduce (fn [result [elt cnt]]
               (assoc result elt (max 0 (- cnt (get ms2 elt)))))
             ms1 (.content ms1)))
  ([ms1 ms2 & msets]
     (reduce intersection ms1 (conj msets ms2))))

(defn multiset? [x]
  (= (type x) MultiSet))


