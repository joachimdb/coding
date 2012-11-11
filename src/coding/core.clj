(ns coding.core
  (:use [coding.entities]
        [coding.example]))

;;; Example simulation where more and more enzymes are produced:

(def s (generate-state-1 ["mk-enzyme"]))
(def s (run-simulation s 1000))

(get-amino-acids s)
(get-enzymes s)

;;; Example simulation where enzymes and adapters are produced:

(def s (generate-state-2 ["mk-enzyme" "mk-adapter" "1"]))
(def s (run-simulation s 5000))

(get-amino-acids s)
(get-enzymes s)
(get-adapters s)

;;; Example simulation where eventually enzymes, adapters and
;;; eventually a new ribosome is produced:

(def s (generate-state-3 ["mk-enzyme" "mk-adapter" "1" "mk-ribosome"]))
(def s (run-simulation s 12000)) ;;; should be enough to create an additional ribosome

(get-amino-acids s)
(get-enzymes s)
(get-adapters s)
(count (get-ribosomes s))
