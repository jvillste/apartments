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
             (attribute :apartments/first-seen :instant)
             (attribute :apartments/not-seen :instant)
             (attribute :apartments/address :string :one :fulltext true)
             (attribute :apartments/area :long)
             (attribute :apartments/price :long)
             (attribute :apartments/comment :string :one :fulltext true)])

(def migrations {} #_{:add-comment
                      {:txes [[]]}})

(def memory-db-uri "datomic:mem://apartments")

(def db-uri "datomic:free://localhost:4334/apartments2")

(defn migrate [db-uri migrations]
  (let [conn (d/connect db-uri)]
    (conformity/ensure-conforms conn
                                migrations)))

(defn create-apartments-database [uri]
  (d/create-database db-uri)
  (let [conn (d/connect db-uri)]
    (d/transact conn schema)))


(defn create-new-in-memory-apartments-database []
  (let [db-uri (str "datomic:mem://" (int (rand 100000)))]
    (create-apartments-database db-uri)
    db-uri))

;; queries

(defn db-attributes [db]
  (d/q '[:find ?ident ?valueType
         :in $
         :where
         [?entity :db/ident ?ident]
         [?entity :db/valueType ?valueTypeId]
         [?valueTypeId :db/ident ?valueType]]
       db))

(defn apartments-by-address [db search]
  (d/q '[:find [?apartment ...]
         :in $ ?search
         :where
         [(fulltext $ :apartments/address ?search) [[?apartment ?address]]]]
       db
       search))

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

(defn apartment-entity-ids [db]
  (d/q '[:find [?entity ...]
         :in $ 
         :where
         [?entity :apartments/id ?_]]
       db))

(defn apartment-ids [db]
  (d/q '[:find [?id ...]
         :in $ 
         :where
         [?_ :apartments/id ?id]]
       db))

;; adding

(defn new-id []
  (d/tempid :db.part/user))

(defn set-for-apartment [id attribute value]
  [{:db/id (new-id)
    :apartments/id id
    attribute value}])

(defn first-seen-now [id]
  (set-for-apartment id :apartments/first-seen (java.util.Date.)))

(defn not-seen-now [id]
  (set-for-apartment id :apartments/not-seen (java.util.Date.)))




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

(defn get-apartment-entities [db]
  (map #(d/entity db %)
       (apartment-entity-ids db)))


(defn raw-data-to-apartments-data [raw-data & keys]
  (reduce (fn [data [key raw-key]]
            (assoc data key (get raw-data raw-key)))
          {}
          (partition 2 keys)))

#_(create-apartments-database db-uri)

#_(d/delete-database db-uri)

#_(reset-apartments-database)

#_(let [conn (d/connect db-uri)]
    (d/transact conn (concat (seen "123")
                             (set-for-apartment "123" :apartments/address "Address 1"))))

#_(let [conn (d/connect db-uri)]
    (let [db (d/db conn)
          entity (d/entity db
                           (apartment-by-id db "123"))]
      (select-keys entity [:apartments/last-seen
                           :apartments/address])))


#_(let [conn (d/connect db-uri)]
    (apartments-by-address (d/db conn) "Vaaksiaisentie"))

#_(let [conn (d/connect db-uri)]
    (d/transact conn schema))

#_(let [conn (d/connect db-uri)]
    (d/transact conn [{:db/id :apartments/address
                       :db/fulltext true
                       :db.alter/_attribute :db.part/db}])
    #_(d/q '[:find ?ident ?valueType
             :in $
             :where
             [?entity :db/ident ?ident]
             [?entity :db/ident :apartments/address]
             [?entity :db/valueType ?valueTypeId]
             [?valueTypeId :db/ident ?valueType]]
           (d/db conn)))

#_(-> db-uri
      d/connect
      d/db
      apartment-ids)


#_(migrate db-uri migrations)

