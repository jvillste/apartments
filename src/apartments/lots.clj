(ns apartments.lots
  (:require (apartments [core :as core]
                        [data :as data])
            [clojure.string :as string]
            [datomic.api :as d]
            [flow-gl.tools.trace :as trace])
  (:use [clojure.test]))

(def test-query "http://www.etuovi.com/myytavat-tontit/tulokset?haku=M100905128&page=9")

(def query-url-base "http://www.etuovi.com/myytavat-tontit/")
(def query-url "tulokset?haku=M100905128")

(def lot-url-base "http://www.etuovi.com/kohde/")

(defonce hickup (core/get-hickup "http://www.etuovi.com/kohde/1165909"))

(defn get-lot-hickup [id]
  (core/get-hickup (str lot-url-base id)))

(defn get-etuovi-lot-ids [hickup]
  (let [result-list-path (core/find-term {:class "results list"} hickup)
        result-list (get-in hickup
                            (-> result-list-path
                                (core/navigate 1 [21])))]
    (->> (map #(when (vector? %)
                 (-> % second :id))
              result-list)
         (filter identity))))

(defn next-url [hickup]
  (let [span-path (core/find-term "Seuraava" hickup)]
    (get-in hickup
            (-> span-path
                (core/navigate 2 [1 :href])))))

(defn get-all-etuovi-lot-ids [query-url]
  (loop [url query-url
         ids []]
    (Thread/sleep 1000)
    (let [hickup (core/get-hickup (str query-url-base url))
          next-url (next-url hickup)
          ids (concat ids (get-etuovi-lot-ids hickup))]
      (if next-url
        (recur next-url
               ids)
        ids))))

(defn get-etuovi-address [hickup]
  (let [path (core/find-term [:dt {} "Sijainti:"] hickup)]
    (get-in hickup
            (-> path
                (core/navigate 1 [9 3 5 2 2])))))

(defn see [conn id]
  (d/transact conn (data/seen id)))

(defn add-missing-data [conn id]
  (let [db (d/db conn)]
    (if (not (data/value db
                         (data/apartment-by-id db id)
                         :apartments/address))
      (d/transact conn (data/set-for-apartment id
                                               :apartments/address
                                               (get-etuovi-address (get-lot-hickup id)))))))

(defn update-data []
  (let [ids (get-all-etuovi-lot-ids test-query)
        conn (d/connect data/db-uri)]
    (doseq [id ids]
      (println id)
      (see conn id)
      (add-missing-data conn id))))

(defn get-apartment-entities [conn]
  (let [db (d/db conn)]
    (map #(d/entity db %)
         (data/apartment-entities db))))






