(ns coding.entities
  (:use [clojure.string :only (join)]
        [coding.multisets :only (multiset)]
        [coding.utils :only (random-elt)]
        [coding.simulation-engine]))

;;;
;;; ENTITIES
;;;
;;; Implements the following entity creator functions (see simulation
;;; engine):
;;;
;;; - (mk-amino-acid word)
;;;   Amino acids are not accepting
;;;   
;;;
;;; - (mk-adapter words)
;;;
;;;   The first in words specifies the amino acid, the number of
;;;   remaining words the codon. For instance, (mk-adapter
;;;   ["mk-adapter "1" "1" "1" "1"]) creates an adapter between the
;;;   codon 4 and the amino acid "mk-adapter"
;;;   
;;;   Adapters can be charged or uncharged. Uncharged adapters accept
;;;   a specific amino acid.
;;;
;;; - (mk-enzyme words)
;;;
;;;   Creates an enzyme that accepts amino acids (words) and produces
;;;   an amino acid for their concatenation
;;;
;;; - (mk-genome genes)
;;;
;;;   Genes is a list of lists of number sequences. Genome's are not
;;;   accepting.
;;;
;;; - (mk-ribosome)
;;;
;;;   Creates a ribosome that accepts a genome. When given a genome,
;;;   it charges itself with one of the genes in it (one of the lists
;;;   of number sequences). Then it works through these sequences,
;;;   each time accepting an adapter matching the current number in
;;;   the current sequence, progressively building a sentence. When
;;;   the end of the current sequence is reached, the resulting
;;;   sentence is evaluated, and the ribosome moves to the next
;;;   sequence. When all sequences in the gene are processed, it
;;;   accepts a genome again etc.
;;;

(defrecord AminoAcid [word]
  InteractionProtocol
  (priority [this] +not-accepting+)
  (requires [this] nil)
  (accept [this entity] (list this entity)))
;; (defmethod clojure.core/print-method AminoAcid [aa writer]
;;   (.write writer (.word aa)))
(defn mk-amino-acid [word]
  (AminoAcid. word))

(defn get-amino-acids [state]
  (when-let [amino-acids (get (:entities state) AminoAcid)]
    (.content amino-acids)))


(defrecord Adapter [codon word charge]
  InteractionProtocol
  (priority [this] 1)
  (requires [this] (when-not charge
                     {:type AminoAcid :test (fn [amino-acid] (= (:word amino-acid) word))}))
  (accept [this amino-acid] (list (Adapter. codon word amino-acid))))
;; (defmethod clojure.core/print-method Adapter [a writer]
;;   (.write writer (str "[" (:codon a) " " (if (:charge a) "" "?") (:word a) "]")))

(defn mk-adapter [words]
  ;; Expects at least two words. The first word identifies the amino
  ;; acid. The number of remaining words identifies the codon number.
  (Adapter. (dec (count words)) (first words) nil))

(defn get-adapters [state]
  (when-let [adapters (get (:entities state) Adapter)]
    (.content adapters)))

(defrecord Enzyme [words remaining]
  InteractionProtocol
  (priority [this] 1)
  (requires [this] {:type AminoAcid :test (fn [amino-acid] (= (:word amino-acid) (first remaining)))})
  (accept [this amino-acid]
    (if (= 1 (count remaining))
      ;; final reactant
      (list (AminoAcid. (join words))
            (assoc this :remaining (into (multiset) words)))
      (list (assoc this :remaining (disj remaining (:word amino-acid)))))))
;; (defmethod clojure.core/print-method Enzyme [e writer]
;;   (.write writer (str "[" (:words e) "->" (join (:words e)) "]")))
(defn mk-enzyme [words]
  (Enzyme. words (into (multiset) words)))

(defn get-enzymes [state]
  (when-let [enzymes (get (:entities state) Enzyme)]
    (.content enzymes)))

(defrecord Genome [genes]
  InteractionProtocol
  (priority [this] +not-accepting+)  
  (requires [this] nil)
  (accept [this entity] (list this entity)))
(defn mk-genome [genes]
  (Genome. genes))

(defn get-genomes [state]
  (when-let [genomes (get (:entities state) Genome)]
    (.content genomes)))

(defn fold [words]
  (if (= 1 (count words))
    ((eval (read-string (first words))))
    ((eval (read-string (first words))) (rest words))))

(defrecord Ribosome [remaining words]
  InteractionProtocol
  (priority [this] 0)  
  (requires [this] (if (empty? remaining)
                     {:type Genome :test (fn [genome] (constantly true))}
                     {:type Adapter
                      :test (fn [adapter] (and (= (:codon adapter) (nth (first remaining) (count words)))
                                               (:charge adapter)))}))
  (accept [this entity]
    (if (= (type entity) Genome)
      ;; return both genome and charged ribosome
      (list entity (into this {:remaining (random-elt (:genes entity))}))
      (conj (if (= (inc (count words)) (count (first remaining)))
              ;; final word of current sentence, return result and move to next sentence
              (list (into this {:remaining (rest remaining), :words []})
                    (fold (conj words (:word entity))))
              (list (assoc this :words (conj words (:word entity)))))
            (assoc entity :charge nil)))))

(defn mk-ribosome []
  (Ribosome. nil []))

(defn get-ribosomes [state]
  (when-let [ribosomes (get (:entities state) Ribosome)]
    (.content ribosomes)))
