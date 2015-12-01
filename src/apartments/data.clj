(ns apartments.data
  (:require [io.rkn.conformity :as conformity]
            [datomic.api :as d])
  (:use clojure.test))


(defn attribute
  ([ident value-type]
   (attribute ident value-type :one))

  ([ident value-type cardinality & {:keys [identity fulltext] :or {identity false fulltext false}}]
   (-> {:db/id (d/tempid :db.part/db)
        :db/ident ident
        :db/valueType (keyword "db.type" (name value-type))
        :db/cardinality (keyword "db.cardinality" (name cardinality))
        :db.install/_attribute :db.part/db}
       (cond-> identity
         (assoc :db/unique :db.unique/identity))
       (cond-> fulltext
         (assoc :db/fulltext true)))))

(def schema [(attribute :apartments/id :string :one :identity true)
             (attribute :apartments/last-seen :instant)
             (attribute :apartments/address :string)])

(def migrations {:add-comment
                 {:txes [[(attribute :apartments/comment :string)]]}})


(def db-uri #_"datomic:mem://apartments" "datomic:free://localhost:4334/apartments")

(defn migrate [db-uri migrations]
  (let [conn (d/connect db-uri)]
    (conformity/ensure-conforms conn
                                migrations)))


(defn create-apartments-database [uri]
  (d/create-database db-uri)
  (let [conn (d/connect db-uri)]
    (d/transact conn schema)))

;; queries


(defn entity-by-value [db attribute value]
  (d/q '[:find ?entity .
         :in $ ?attribute ?value
         :where
         [?entity ?attribute ?value]]
       db
       attribute
       value))

(defn entities-by-value [db attribute value]
  (d/q '[:find [?entity ...]
         :in $ ?attribute ?value
         :where
         [?entity ?attribute ?value]]
       db
       attribute
       value))

(defn value [db entity attribute]
  (d/q '[:find ?value .
         :in $ ?entity ?attribute
         :where
         [?entity ?attribute ?value]]
       db
       entity
       attribute))

(defn values [db entity attribute]
  (d/q '[:find [?value ...]
         :in $ ?entity ?attribute
         :where
         [?entity ?attribute ?value]]
       db
       entity
       attribute))

(defn attributes [db entity]
  (d/q '[:find [?attribute ...]
         :in $ ?entity
         :where
         [?entity ?attribute]]
       db
       entity))


;; apartment queries

(defn apartment-by-id [db id]
  (entity-by-value db
                   :apartments/id
                   id))

(defn apartment-entities [db]
  (d/q '[:find [?entity ...]
         :in $ 
         :where
         [?entity :apartments/id ?_]]
       db))

;; adding

(defn new-id []
  (d/tempid :db.part/user))

(defn set-for-apartment [id attribute value]
  [{:db/id (new-id)
    :apartments/id id
    attribute value}])

(defn seen [id]
  (set-for-apartment id :apartments/last-seen (java.util.Date.)))



(defn set-attribute-value [transaction entity-id attribute value]
  (let [updated-transaction (reduce (fn [updated-transaction statement-map]
                                      (if (= entity-id (:db/id statement-map))
                                        (conj updated-transaction
                                              (assoc statement-map
                                                     attribute value))
                                        (conj updated-transaction
                                              statement-map)))
                                    []
                                    transaction)]
    (if (= transaction updated-transaction)
      (conj transaction
            {:db/id entity-id
             attribute value})
      updated-transaction)))

(defn tempids [transaction]
  (->> transaction
       (map :db/id)
       (filter #(instance? datomic.db.DbId %))))

(defn ids-to-tempids [db transaction-tempids tempids]
  (reduce (fn [result tempid]
            (assoc result
                   (d/resolve-tempid db
                                     transaction-tempids
                                     tempid)
                   tempid))
          {}
          tempids))


(defn tempid-to-id [state tempid]
  (-> (filter (fn [[key value]]
                (= value tempid))
              (:ids-to-tempids-map state))
      (first)
      (first)))

(defn reset-apartments-database []
  (d/delete-database db-uri) 
  (create-apartments-database db-uri))

#_(create-apartments-database db-uri)

#_(let [conn (d/connect db-uri)]
    (d/transact conn (concat (seen "123")
                             (set-for-apartment "123" :apartments/address "Address 1"))))

#_(let [conn (d/connect db-uri)]
    (let [db (d/db conn)
          entity (d/entity db
                           (apartment-by-id db "123"))]
      (select-keys entity [:apartments/last-seen
                           :apartments/address])))

#_(migrate db-uri migrations)
