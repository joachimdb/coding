(ns coding.simulation-engine
  (:use [coding.utils :only (random-elt)]
        [coding.multisets :only (multiset union filter-ms)]))

;;;
;;; SIMULATION ENGINE
;;;
;;; The record State holds the state of the simulation, which
;;; basically is a set of entities. Each entity must implement the
;;; InteractionProtocol, meaning the following functions:
;;;
;;; - (requires this) : should return a {:type entity-type :test fn}
;;;    map.
;;;
;;;   The simulation engine will look for entities that require
;;;   another entity of the specified type and that satisfies the
;;;   specified test.
;;;
;;; - (accept this entity) : should return a collection of entities
;;;    representing the result of the interaction between the
;;;    accepting entity and the provided entity
;;;
;;; - (priority this) : should return a number. Entities whith lower
;;;    priority are considered first by the engine
;;;
;;;   If an entity is not accepting itself, one can give it the
;;;   priority +not-accepting+ (mainly for speedup).
;;;
;;; A state can be created with (mk-state nutrients & entities), where
;;; nutrients is a collection specifying what strings are considered
;;; to be nutrients.
;;;
;;; Entities can be added and deleted from a state with the functions
;;; (add-entity state entity), (add-entities state entities),
;;; (del-entity state entity), (del-entities state entities)
;;;
;;; To retrieve an entity (get-entities state entity-type) and
;;; (get-entities state entity-type test-fn) can be used
;;;

(defrecord State [nutrients entities last-changed priorities])

(defprotocol InteractionProtocol
  (priority [this])
  (requires [this])
  (accept [this thing]))

(defn add-entity
  ([state entity]
     (let [entity-type (type entity)]
       (if-let [entry (get (:entities state) entity-type)]
         (assoc-in state [:entities entity-type]
                   (conj entry entity))
         (assoc-in (assoc-in state [:entities entity-type]
                             (multiset entity))
                   [:priorities (priority entity)]
                   (conj (get (:priorities state) (priority entity))
                         entity-type))))))

(defn del-entity
  ([state entity]
     (let [entity-type (type entity)]
       (if-let [entry (get (:entities state) entity-type)]
         (assoc-in state [:entities entity-type]
                   (disj entry entity))
         state))))

(defn add-entities [state entities]
  (reduce (fn [result entity]
            (add-entity result entity))
          state
          entities))

(defn del-entities [state entities]
  (reduce (fn [result entity]
            (del-entity result entity))
          state
          entities))

(defn get-entities  
  ([state entity-type]
     (get (:entities state) entity-type))
  ([state entity-type test-fn]
     (filter-ms test-fn (get (:entities state) entity-type))))

(defn mk-state
  ([nutrients] (State. nutrients {} -1 (sorted-map)))
  ([nutrients entity] (add-entity (mk-state nutrients) entity))
  ([nutrients entity & entities] (add-entities (mk-state nutrients) (conj entities entity))))

(def +not-accepting+ 999)

(defn find-accepting 
  "Returns a multiset of entities that are accepting as a multiset"
  ([state type]
     (filter-ms requires (get (:entities state) type)))
  ([state type & types]
     (reduce (fn [result t]
               (union result (find-accepting state t)))
             (find-accepting state type)
             types)))

(defn find-required
  "Returns a multiset of entities fulfilling the requirements of entity"
  [state entity]  
  (let [requirement (requires entity)]
    (get-entities state (:type requirement) (:test requirement))))

(def interaction-accepting first)
(def interaction-required second)

(defn find-interaction
  ([state] (find-interaction state (map val (dissoc (:priorities state) +not-accepting+))))
  ([state type-sets]
     (when-not (empty? type-sets)
       (loop [accepting (apply find-accepting state (first type-sets))]
         (if (empty? accepting)
           (find-interaction state (rest type-sets))
           (let [entity (random-elt accepting)
                 required (random-elt (find-required state entity))]
             (if required
               [entity required]
               (recur (disj accepting entity)))))))))

(defn apply-interaction [state interaction]
  (add-entities (del-entities state interaction)
                (accept (interaction-accepting interaction)
                        (interaction-required interaction))))

(defn simulation-step
  ([state]
     (simulation-step state (inc (:last-changed state))))
  ([state time]
     (if-let [interaction (find-interaction state)]
       (assoc (apply-interaction state interaction)
         :last-changed time)
       state)))

(defn simulation-steps
  ([state nsteps]
     (simulation-steps state nsteps (inc (:last-changed state))))
  ([state nsteps time]
     (if (<= nsteps 0)
       state
       (recur (simulation-step state time) (dec nsteps) (inc time)))))
