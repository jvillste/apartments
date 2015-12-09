(ns apartments.lots
  (:require (apartments [core :as core]
                        [data :as data]
                        [etuovi :as etuovi])
            [clojure.string :as string]
            [datomic.api :as d]
            [clojure.set :as set]
            [flow-gl.tools.trace :as trace])
  (:use [clojure.test]))

(def test-query "http://www.etuovi.com/myytavat-tontit/tulokset?haku=M100905128&page=9")

(def query-url-base "http://www.etuovi.com/myytavat-tontit/")
(def query-url "tulokset?haku=M100905128")

(def lot-url-base "http://www.etuovi.com/kohde/")

(defonce hickup (core/get-hickup "http://www.etuovi.com/kohde/1165909"))

(defn get-lot-hickup [id]
  (core/get-hickup (str lot-url-base id)))


(defn get-etuovi-address [hickup]
  (let [path (core/find-term [:dt {} "Sijainti:"] hickup)]
    (get-in hickup
            (-> path
                (core/navigate 1 [9 3 5 2 2])))))




(defn clean-number [value]
  (->> (take-while #(re-find #"[0-9 ]" (str %)) value)
       (filter #(not= \  %))
       (apply str)
       (read-string)
       (int)))

(defn etuovi-raw-data-to-apartments-data [raw-data]
  (-> (data/raw-data-to-apartments-data raw-data
                                        :apartments/id "Kohdenumero:"
                                        :apartments/address "Sijainti:"
                                        :apartments/area "Tontin pinta-ala:"
                                        :apartments/price "Myyntihinta:")
      (update-in [:apartments/area] clean-number)
      (update-in [:apartments/price] clean-number)))

(defn get-lot-data [lot-id]
  (etuovi-raw-data-to-apartments-data (etuovi/get-lot-data (get-lot-hickup lot-id))))

(defn set-new-apartments-data [apartment-data-map]
  [(assoc apartment-data-map
          :db/id (data/new-id)
          :apartments/first-seen (java.util.Date.))])

(defn new-ids [old-ids current-ids]
  (set/difference (apply hash-set current-ids)
                  (apply hash-set old-ids)))

(defn removed-ids [old-ids current-ids]
  (set/difference (apply hash-set old-ids)
                  (apply hash-set current-ids)))


(defn load-data [ids]
  (->> ids
       (map get-lot-data)
       (map set-new-apartments-data)))

(defn refresh-data [conn current-ids]
  (let [db (d/db conn)
        old-ids (data/apartment-ids db)]
    (d/transact conn
                (flatten
                 (load-data (new-ids old-ids
                                     current-ids))))))

(defn mark-not-seen [ids]
  (map data/not-seen-now ids))

;; sample data

(defn int-rand [from to step]
  (* step (int (/ (+ from
                     (int (rand (- to from))))
                  step))))

(defn sample-apartment [id]
  {:apartments/id id
   :apartments/address (str "address " id)
   :apartments/comment (str "comment " id)
   :apartments/price (int-rand 80000 150000 1000)
   :apartments/area (int-rand 500 2500 10)})

;; test

(def db-uri "datomic:free://localhost:4334/apartments")

#_(def raw-data (etuovi/get-lot-data (get-lot-hickup "1165909")))

#_(data/create-apartments-database db-uri)

#_(let [conn (d/connect db-uri)]
  (refresh-data conn (get-all-etuovi-lot-ids "http://www.etuovi.com/myytavat-tontit/tulokset?haku=M100905128&page=9")))

#_(let [conn (d/connect db-uri)]
    (refresh-data conn #{"7663526" "7663600"}))

#_(flatten (load-data ["7663526"]))

#_(let [;conn (d/connect (data/create-new-in-memory-apartments-database))
        data (map sample-apartment (range 1 3))
        new-data (map sample-apartment (range 2 4))]
    (clojure.pprint/pprint (map set-new-apartments-data data)))

#_(def ids (get-all-etuovi-lot-ids "http://www.etuovi.com/myytavat-tontit/tulokset?haku=M100905128&page=9"))

#_(removed-ids ids
               (-> db-uri
                   d/connect
                   d/db
                   data/apartment-ids))

#_(etuovi-raw-data-to-apartments-data raw-data)

#_(let [conn (d/connect db-uri)]
    (data/apartment-ids (d/db conn)))
