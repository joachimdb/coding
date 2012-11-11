(ns coding.example
  (:use [clojure.string :only (join)]
        [coding.utils :only (random-elt)]
        [coding.simulation-engine]
        [coding.entities]))


;;; EXAMPLE SIMULATION

;;; Each time step, a feed-amnt number of nutrients is added to the
;;; state (as amino acids). To keep the state balanced, the least
;;; abundant nutrients are considered first. When the total number of
;;; amino acids reaches pool-size, a decay-amnt number of random amino
;;; acids are deleted.

;;; An initial state can be made in three ways:
;;;
;;; * with (generate-state-1 ["mk-enzyme"]), resulting is a state containing:
;;;
;;;   - 50 "mk-enzyme" amino acids,
;;;   - one uncharged ribosome,
;;;   - the genome (((6 4 2) (6 14 3) (6 7 1) (6 9 5) (6 11 13) (6 0 12) (6 10 4) (6 8 1)))
;;;   - 15 uncharged adapters:
;;;
;;;            {{:codon 10, :word "mk-enzy"},
;;;             {:codon 8, :word "mk-enzym"},
;;;             {:codon 9, :word "mk-e"},
;;;             {:codon 3, :word "-"},
;;;             {:codon 6, :word "mk-enzyme"},
;;;             {:codon 0, :word "mk-enz"},
;;;             {:codon 7, :word "mk-"},
;;;             {:codon 5, :word "n"},
;;;             {:codon 4, :word "m"},
;;;             {:codon 1, :word "e"},
;;;             {:codon 11, :word "mk-en"},
;;;             {:codon 13, :word "z"},
;;;             {:codon 14, :word "mk"},
;;;             {:codon 2, :word "k"},
;;;             {:codon 12, :word "y", :charge nil} 1}
;;;
;;;   This results in a simulation where more and more enzymes are
;;;   created.
;;;
;;; * with (generate-state-2 ["mk-enzyme" "mk-adapter" "1"])
;;;
;;;   This results in a state containing:
;;;
;;;   - 50 "mk-enzyme" amino acids,
;;;   - one uncharged ribosome,
;;;   - the genome ((10 8 6) (10 27 7) (10 11 5) (10 22 9) (10 24 26) (10 3 25) (10 23 8) (10 21 5) (10 8 6) (10 27 7)
;;;                 (10 11 1) (10 18 4) (10 17 1) (10 12 13) (10 2 20) (10 19 5) (10 16 15) (0 6 14 14 14 14 14 14)
;;;                 (0 15 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14) (0 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
;;;                 (0 26 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
;;;                 (0 18 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14) (0 13 14 14 14 14 14 14 14 14 14 14 14 14 14)
;;;                 (0 3 14 14 14) (0 16 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
;;;                 (0 19 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
;;;                 (0 17 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14) (0 2 14 14) (0 7 14 14 14 14 14 14 14) (0 0)
;;;                 (0 24 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
;;;                 (0 10 14 14 14 14 14 14 14 14 14 14)
;;;                 (0 27 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
;;;                 (0 1 14) (0 25 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
;;;                 (0 11 14 14 14 14 14 14 14 14 14 14 14) (0 9 14 14 14 14 14 14 14 14 14)
;;;                 (0 22 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14) (0 8 14 14 14 14 14 14 14 14)
;;;                 (0 23 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
;;;                 (0 21 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
;;;                 (0 12 14 14 14 14 14 14 14 14 14 14 14 14) (0 4 14 14 14 14)
;;;                 (0 20 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14) (0 5 14 14 14 14 14))
;;;   - 28 uncharged adapters:
;;;
;;;                 {{:codon 6, :word "k"},
;;;                  {:codon 15, :word "r"},
;;;                  {:codon 14, :word "1"},
;;;                  {:codon 26, :word "z"},
;;;                  {:codon 18, :word "mk-a"},
;;;                  {:codon 13, :word "p"},
;;;                  {:codon 3, :word "mk-enz"},
;;;                  {:codon 16, :word "mk-adapte"},
;;;                  {:codon 19, :word "mk-adapt"},
;;;                  {:codon 17, :word "mk-ad"},
;;;                  {:codon 2, :word "mk-adap"},
;;;                  {:codon 7, :word "-"},
;;;                  {:codon 0, :word "mk-adapter"},
;;;                  {:codon 24, :word "mk-en"},
;;;                  {:codon 10, :word "mk-enzyme"},
;;;                  {:codon 27, :word "mk"},
;;;                  {:codon 1, :word "a"},
;;;                  {:codon 25, :word "y"},
;;;                  {:codon 11, :word "mk-"},
;;;                  {:codon 9, :word "n"},
;;;                  {:codon 22, :word "mk-e"},
;;;                  {:codon 8, :word "m"},
;;;                  {:codon 23, :word "mk-enzy"},
;;;                  {:codon 21, :word "mk-enzym"},
;;;                  {:codon 12, :word "mk-ada"},
;;;                  {:codon 4, :word "d"},
;;;                  {:codon 20, :word "t"},
;;;                  {:codon 5, :word "e"}}
;;;
;;;   This results in a simulation where more and more enzymes and
;;;   adapters are created.
;;;
;;;   This results in a simulation where 
;;;                 
;;; * with (generate-state-3 ["mk-enzyme" "mk-adapter" "1" "mk-ribosome"])
;;;
;;;   This results in a state containing:
;;;
;;;   - 50 "mk-enzyme" amino acids,
;;;   - one uncharged ribosome,
;;;   - the genome
;;;
;;;     ((16 14 12) (16 39 13) (16 19 8) (16 32 15) (16 34 36) (16 4 35) (16 33 14) (16 31 8) (16 14 12) (16 39 13)
;;;      (16 19 2) (16 26 7) (16 25 2) (16 20 21) (16 3 30) (16 28 8) (16 24 23) (16 14 12) (16 39 13) (16 19 23)
;;;      (16 6 11) (16 9 5) (16 37 17) (16 29 27) (16 1 17) (16 18 14) (16 38 8) (0 2 22 22)
;;;      (0 11 22 22 22 22 22 22 22 22 22 22 22) (0 0) (0 3 22 22 22) (0 6 22 22 22 22 22 22)
;;;      (0 23 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22) (0 1 22)
;;;      (0 4 22 22 22 22)
;;;      (0 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 5 22 22 22 22 22)
;;;      (0 12 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 26 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 15 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 21 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 24 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 27 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 34 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 7 22 22 22 22 22 22 22)
;;;      (0 14 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 25 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 35 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 18 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 10 22 22 22 22 22 22 22 22 22 22)
;;;      (0 13 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 36 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 19 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 9 22 22 22 22 22 22 22 22 22)
;;;      (0 16 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 28 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 17 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 38 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 29 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 8 22 22 22 22 22 22 22 22)
;;;      (0 39 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 31 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 30 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 20 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 37 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 32 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (0 33 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
;;;      (10))
;;;
;;;   - 40 uncharged adapters:
;;;
;;;     {{:codon 2, :word "a"},
;;;      { 11, :word "i"},
;;;      { 0, :word "mk-adapter"},
;;;      { 3, :word "mk-adap"},
;;;      { 6, :word "mk-r"},
;;;      { 23, :word "r"},
;;;      { 1, :word "mk-ribos"},
;;;      { 4, :word "mk-enz"},
;;;      { 22, :word "1"},
;;;      { 5, :word "b"},
;;;      { 12, :word "k"},
;;;      { 26, :word "mk-a"},
;;;      { 15, :word "n"},
;;;      { 21, :word "p"},
;;;      { 24, :word "mk-adapte"},
;;;      { 27, :word "s"},
;;;      { 34, :word "mk-en"},
;;;      { 7, :word "d"},
;;;      { 14, :word "m"},
;;;      { 25, :word "mk-ad"},
;;;      { 35, :word "y"},
;;;      { 18, :word "mk-riboso"},
;;;      { 10, :word "mk-ribosome"},
;;;      { 13, :word "-"},
;;;      { 36, :word "z"},
;;;      { 19, :word "mk-"},
;;;      { 9, :word "mk-ri"},
;;;      { 16, :word "mk-enzyme"},
;;;      { 28, :word "mk-adapt"},
;;;      { 17, :word "o"},
;;;      { 38, :word "mk-ribosom"},
;;;      { 29, :word "mk-ribo"},
;;;      { 8, :word "e"},
;;;      { 39, :word "mk"},
;;;      { 31, :word "mk-enzym"},
;;;      { 30, :word "t"},
;;;      { 20, :word "mk-ada"},
;;;      { 37, :word "mk-rib"},
;;;      { 32, :word "mk-e"},
;;;      { 33, :word "mk-enzy"}}
;;;
;;;   This results in a simulation where more and more enzymes and
;;;   adapters are created and eventually a new ribosome.

(def pool-size 100)
(def feed-amnt 1)
(def decay-amnt 10)

(defn feed
  ([state]
     (let [counts (map (fn [nutrient]
                         (count (filter (fn [aa] (= (:word aa) nutrient)) (get-entities state coding.entities.AminoAcid))))
                       (:nutrients state))
           remainig (reduce (fn [[result min] [new cnt]]
                              (cond (< cnt min) [[new] cnt]
                                    (= cnt min) [(conj result new) min]
                                    :else [result min]))
                            [[] 9999]
                            (zipmap (:nutrients state) counts))]
       (add-entity state (mk-amino-acid (random-elt (first remainig))))))
  ([state n]
     (if (= 0 n)
       state
       (recur (feed state) (dec n)))))

(defn decay
  ([state] (del-entity state (random-elt (get-entities state coding.entities.AminoAcid))))
  ([state n]
     (if (= 0 n)
       state
       (recur (decay state) (dec n)))))


(defn run-simulation [state steps]
  (if (= 0 steps)
    state
    (if (< (count (get-entities state coding.entities.AminoAcid)) pool-size)
      (recur (feed (simulation-step state) feed-amnt)
             (dec steps))
      (recur (decay (simulation-step state) decay-amnt)
             (dec steps)))))

(defn generate-nutrients
  ([word] (generate-nutrients word #{}))
  ([word result]
     (if (empty? word)
       result
       (recur (.substring word 1)
              (conj result (.substring word 0 1))))))

(defn generate-amino-acids
  ([word] (generate-amino-acids word #{}))
  ([word result]
     (cond (< (count word) 3) result
           :else (generate-amino-acids (.substring word 0 2) (.substring word 2) result)))
  ([prev rem result]
     (if (empty? rem)
       (conj result prev)
       (recur (join [prev (.substring rem 0 1)])
              (.substring rem 1)
              (conj result prev)))))

(defn generate-adapter
  ([word] (generate-adapter word #{}))
  ([word adapters]
     (if (some (fn [adapter] (= (:word adapter) word)) adapters)
       adapters
       (conj adapters (mk-adapter (conj (repeat (count adapters) "1") word))))))

(defn generate-adapters
  ([words] (generate-adapters words #{}))
  ([words adapters]
     (reduce (fn [result word]
               (generate-adapter word result))
             adapters
             words)))

(defn generate-gene
  ([words adapters]
     (generate-gene words adapters ()))
  ([words adapters result]
     (if (empty? words)
       result
       (when-let [adapter (random-elt (filter (fn [adapter]
                                                (= (:word adapter) (peek words)))
                                              adapters))]
         (recur (pop words) adapters (conj result (:codon adapter)))))))

(defn generate-state-1 [operators]
  (let [nutrients (into #{} (mapcat generate-nutrients operators))
        amino-acids (map generate-amino-acids operators)
        adapters (generate-adapters (into #{} (apply concat nutrients amino-acids)))
        genes (filter (fn [gene] (not (empty? gene)))
                      (map (fn [pathway]
                             (map (fn [amino-acid]
                                    (generate-gene ["mk-enzyme"
                                                    (.substring amino-acid 0 (dec (count amino-acid)))
                                                    (.substring amino-acid (dec (count amino-acid)))]
                                                   adapters))
                                  (sort (fn [str1 str2] (< (count str1) (count str2)))
                                        pathway)))
                           amino-acids))]
    (add-entities
     (apply (partial mk-state nutrients) (conj adapters
                                               (mk-ribosome)
                                               (mk-genome genes)))
     (repeat 50 (mk-amino-acid "mk-enzyme")))))

(defn generate-state-2 [constructors]
  (let [s (generate-state-1 constructors)
        genome (first (get-entities s coding.entities.Genome))]
    (add-entity (del-entity s genome)
                (assoc genome :genes
                       (list (apply concat
                                    (concat (:genes genome)
                                            (map (fn [adapter]
                                                   (list (generate-gene (into [] (cons "mk-adapter"
                                                                                       (conj (repeat (:codon adapter) "1") (:word adapter))))
                                                                        (get (:entities s) coding.entities.Adapter))))
                                                 (get (:entities s) coding.entities.Adapter)))))))))


(defn generate-state-3 [constructors]
  (let [s (generate-state-2 constructors)
        genome (first (get-entities s coding.entities.Genome))]
    (add-entity (del-entity s genome)
                (assoc genome :genes
                       (list (apply concat
                                    (concat (:genes genome)
                                            (list (list (generate-gene ["mk-ribosome"]
                                                                       (get (:entities s) coding.entities.Adapter)))))))))))

