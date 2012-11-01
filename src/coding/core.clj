(ns coding.core
  (:use [clojure.string :only (join split)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               I. Multisets                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype MultiSet [content cnt]
  clojure.lang.IPersistentSet
  (seq [this] (seq content))
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
     (reduce conj (MultiSet. {} 0) coll)))

(defn union
  ([ms1 ms2]
     (reduce (fn [result [elt cnt]]
               (assoc result elt (+ cnt (get ms1 elt))))
             ms1 ms2))
  ([ms1 ms2 & msets]
     (reduce union ms1 (conj msets ms2))))

(defn intersection
  ([ms1 ms2]
     (reduce (fn [result [elt cnt]]
               (assoc result elt (min cnt (get ms2 elt))))
             ms1 ms1))
  ([ms1 ms2 & msets]
     (reduce intersection ms1 (conj msets ms2))))

(defn difference
  ([ms1 ms2]
     (reduce (fn [result [elt cnt]]
               (assoc result elt (max 0 (- cnt (get ms2 elt)))))
             ms1 ms1))
  ([ms1 ms2 & msets]
     (reduce intersection ms1 (conj msets ms2))))

(defn multiset? [x]
  (= (type x) MultiSet))

(defn as-sequence [multiset]
  (apply concat (map (fn [[elt cnt]] (repeat cnt elt))
                     (.content multiset))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            II. Utility functions             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn random-elt [coll]
  ;; not very efficient...
  (cond (empty? coll)
        nil
        (map? coll)
        (random-elt (into [] coll))
        (multiset? coll)
        (random-elt (apply concat (map (fn [[elt cnt]] (repeat cnt elt)) coll)))
        (set? coll)
        (random-elt (into [] coll))
        :else (nth coll (rand-int (count coll)))))


(defn subst [key-vals, in]
  (if (coll? in)
    (if (empty? in)
      in
      (conj (subst key-vals (pop in))
            (subst key-vals (peek in))))
    (or (key-vals in) in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            III. Simulation                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 1) Datastructure for keeping the state of the simulation

(defrecord SimulationState
    [amino-acids ;; "words". Also contains nutrients.
     polypeptides ;; strings with spaces ("sentences")
     enzymes ;; functional units consuming nutrients and amino-acids and producing more complex (longer) amino-acids
     genes ;; sequences of "codons" (numbers)
     adapters ;; associations between "codons" and "amino-acids"
     code ;; code according to which adapters are made from amino-acids (codons are assumed to be available abundantly)
     last-changed])

(defn simulation-state [amino-acids polypeptides enzymes genes adapters code last-changed]
  (SimulationState. amino-acids polypeptides enzymes genes adapters code last-changed))

;;; The following initial state can be used as the basis of
;;; simulations. There are four nutrients, "j", "o", "i" and
;;; "n". There are three genes which, according to the code given,
;;; translate into the polypeptides "join j o", "join jo i" and "join
;;; joi n". When folded (see later), these are turned into enzymes
;;; that actually produce the amino acids "jo", "joi" and "join" from
;;; nutrients and smaller amino-acids.

(def initial-state
  (simulation-state (multiset ["j" "o" "i" "n"]), ;; nutrients and amino-acids
                    (multiset),                   ;; polypeptides
                    (multiset),                   ;; enzymes
                    #{[0 1 2], [0 3 4], [0 5 6]}, ;; genes
                    (multiset),                   ;; adapters
                    {0 "join", 1 "j", 2 "o", 3 "jo", 4 "i", 5 "joi", 6 "n"} ;; genetic code
                    -1))                ;; last-changed

;;; 1) Nutrients are added randomly unless the pool of nutrients and
;;; amino-acids is full (threshold set at 100):

(defn add-nutrients [state]
  (if (< (count (:amino-acids state)) 100)
    (into state {:amino-acids (conj (:amino-acids state) (random-elt ["j" "o" "i" "n"]))})
    state))

;;; 2) Enzymes are functional entities that consume nutrients and
;;; amino-acids ("letters" and "words") and produce a new amino-acid
;;; (a "word")

(defrecord Enzyme [consumes produces])

(defn enzyme [consumes produces]
  (Enzyme. (multiset consumes) produces))

(defn run-enzyme [enzyme state time]
  "Applies enzyme if possible and returns an updated state accordingly."
  ;; first check if all required nutrients are available
  (let [available (intersection (:amino-acids state) (:consumes enzyme))]
    (if (= (count available) (count (:consumes enzyme)))
      ;; all nutrients and and amino-acids consumed are available, so
      ;; apply the enzyme and return a new state accordingly
      (into state
            {:amino-acids (conj (difference (:amino-acids state) (:consumes enzyme))
                                (:produces enzyme))
             :last-changed time})
      ;; not all nutrients available, just return the same state
      state)))

;; example: (run-enzyme (enzyme ["j" "o"] "jo") initial-state 0)

(defn run-random-enzyme
  "Selects a random enzyme and applies it if possible. Returns an
updated state accordingly."
  ([state time]
     (if-let [enzyme (random-elt (:enzymes state))]
       (run-enzyme enzyme state time)
       state)))

;; example: (run-random-enzyme (into initial-state {:enzymes (multiset [(enzyme ["j" "o"] "jo")])}) 0)

;;; 3) Polypeptides are "sentences" that when folded become enzymes

(defn fold-polypeptide [polypeptide]
  "Makes an enzyme from a polypeptide."
  (let [amino-acids (split polypeptide #" ")]
    (enzyme (rest amino-acids)
            ((eval (read-string (first amino-acids))) (rest amino-acids)))))

(defn fold-polypeptides [state time]
  "Folds all polypeptides and returns a new state accordingly."
  (if (empty? (:polypeptides state))
    state
    (let [enzymes (reduce (fn [result [polypeptide cnt]]
                            (assoc result (fold-polypeptide polypeptide) cnt))
                          (multiset)
                          (:polypeptides state))]
      (into state
            {:polypeptides (multiset)
             :enzymes (union (:enzymes state) enzymes),
             :last-changed time}))))

;; example: (fold-polypeptides (into initial-state {:polypeptides (multiset ["join j o" "join j o" "join jo i"])}) 0)

;;; 4) Adapters are associations between a codon and an amino
;;; acid. They are made by coupling an amino acid or nutrient to a
;;; codon according to the rules in the genetic code.  It is assumed
;;; that the codons are available.

(defrecord Adapter [codon amino-acid])
(defn adapter [codon amino-acid] (Adapter. codon amino-acid))

(defn make-adapter [rule state time]
  "Makes an adapter as specified by a genetic coding rule if possible
and returns an updated state accordingly."
  (if (contains? (:amino-acids state) (val rule))
    (into state
          {:adapters (conj (:adapters state) (adapter (key rule) (val rule))),
           :amino-acids (disj (:amino-acids state) (val rule)),
           :last-changed time})
    state))

;; Adapters are made only when there is room in the pool of adapters
;; (threshold set at 100):

(defn make-random-adapter [state time]
  "Selects a random genetic coding rule and makes an adapter if
possible. Returns an updated state accordingly."
  (if (< (count (:adapters state)) 100)
    (if-let [rule (random-elt (:code state))]
      (make-adapter rule state time)
      state)
    state))

;; example: (make-random-adapter initial-state 0)

;;; 5) Translation of genes into polypeptides occurs if adapters are
;;; available.

(defn get-adapter [codon state]
  ;; note: expands the adapter multiset in state into a sequence which
  ;; is not efficient
  (when-let [adapters (filter (fn [adapter] (= (:codon adapter) codon))
                              (as-sequence (:adapters state)))]
    (random-elt adapters)))

(defn translate-gene-helper
  ([gene state] (translate-gene-helper gene state '()))
  ([gene state adapters]
     (if (empty? gene)
       (into state
             {:polypeptides (conj (:polypeptides state)
                                  (join (butlast (interleave (map :amino-acid adapters)
                                                             (repeat " ")))))})
       (when-let [adapter (get-adapter (peek gene) state)]
         (recur (pop gene)
                (assoc state :adapters (disj (:adapters state) adapter))
                (conj adapters adapter))))))

(defn translate-gene [gene state time]
  (if-let [new-state (translate-gene-helper gene state)]
    (assoc new-state :last-changed time)
    state))

;; examples:
;; (def s (make-random-adapter initial-state 0))
;; (def s2 (make-random-adapter s 0))
;; (translate-gene [1] s2 1)

(defn translate-random-gene [state time]
  (translate-gene (random-elt (:genes state)) state time))

;;;  5) In a simulation, all possible actions are simply tried one
;;;  after the other:

(defn simulate-step [state time]
  (translate-random-gene
   (run-random-enzyme
    (fold-polypeptides
     (make-random-adapter (add-nutrients state)
                          time)
     time)
    time)
   time))

(defn simulate-steps [state steps time]
  (if (= 0 steps)
    state
    (simulate-steps (simulate-step state time) (- steps 1) (+ time 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            IV. Examples                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 1) When starting from the normal initial state adapters are made
;;; for the codons 1,2,4 and 6 corresponding to the nutrients "j",
;;; "o", "i" and "n". But since no adapter is ever made for the "join"
;;; amino acid, and since every gene relies on it, nothing else
;;; happens. Eventually the amino-acid and adapter pools become filled
;;; and no further changes occur.

(simulate-steps initial-state 1000 0)

;;; 2) If we add a single adapter for the "join", then the first gene
;;; gets translated into an enzyme which in turn leads to the
;;; production of the amino-acid "jo". Eventually the simulation stops
;;; again, because no other enzyme can be made and the amino-acid and
;;; adapter pools become filled.

(def s2 (assoc initial-state :adapters (multiset [(adapter 0 "join")])))

(simulate-steps s2 1000 0)

;;; 3) If the amount of initial "join" adapters is increased (at least
;;; 3, but due to randomization on average rather something like 5),
;;; then suddenly the system takes off. This happens as soon as all
;;; three enzymes are produced. At this point, more and more "join"
;;; amino-acids are produced. Since these are however not comsumed
;;; themselves, eventually the amino-acid and adapter pools are
;;; filled, and the simulation stops again.


(def s3 (assoc initial-state :adapters (multiset (repeat 10 (adapter 0 "join")))))

(simulate-steps s3 1000 0)

