(ns coding.multisets)

(deftype MultiSet [content cnt]
  Object
  (toString [this] (str content))
  (hashCode [this] (hash content))
  clojure.lang.IPersistentSet
  (seq [this] (if (= 0 cnt)
                nil
                (apply concat (map (fn [[elt cnt]] (repeat cnt elt))
                                   content))))
  (cons [this elt] (if-let [count (content elt)]
                     (MultiSet. (assoc content elt (+ count 1)) (+ cnt 1))
                     (MultiSet. (assoc content elt 1) (+ cnt 1))))
  (count [this] cnt)
  (empty [this] (MultiSet. (array-map) 0))
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

(defn multiset
  ([] (MultiSet. (array-map) 0))
  ([elt] (conj (multiset) elt))
  ([elt & elts] (into (multiset) (conj elts elt))))

(defn union
  ([ms1 ms2]
     (MultiSet. (reduce (fn [result [elt cnt]]
                          (assoc result elt (+ cnt (get ms1 elt))))
                        (.content ms1) (.content ms2))
                (+ (.cnt ms1) (.cnt ms2))))
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

(defn random-elt
  ([mset] (random-elt (.content mset) (rand-int (.cnt mset))))
  ([content index]
     (if (> (val (first content)) index)
       (key (first content))
       (recur (rest content) (- index (val (first content)))))))

(defn filter-ms [pred ms]
  (let [filtered (into {} (filter (fn [[elt cnt]]
                                    (pred elt))
                                  (.content ms)))]
    (MultiSet. filtered
               (reduce (fn [total [elt cnt]] (+ total cnt)) 0 filtered))))
