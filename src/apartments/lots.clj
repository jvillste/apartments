(ns apartments.lots
  (:require [apartments.core :as core]
            [clojure.string :as string]
            [flow-gl.tools.trace :as trace])
  (:use [clojure.test]))

(def query-url-base "http://www.etuovi.com/myytavat-tontit/")
(def query-url "tulokset?haku=M100905128")

(def lot-url-base "http://www.etuovi.com/kohde/")

(defonce hickup (core/get-hickup query-url))

#_(deftest get-test
    (is (= ""
           (get-etuovi-address hickup))))

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
    (println "getting " url)
    (Thread/sleep 1000)
    (let [hickup (core/get-hickup (str query-url-base url))
          next-url (next-url hickup)
          ids (concat ids (get-etuovi-lot-ids hickup))]
      (if next-url
        (recur next-url
               ids)
        ids))))

(defn get-etuovi-data [url]
  (let [hickup (core/get-hickup url)]
    {:url url
     :address (core/get-etuovi-address hickup)}))

(defonce data (atom {}))

#_(defn load-data []
    (reset! data (reduce (fn [data url]
                           (if (not (contains? data url))
                             (assoc data url (get-data url))
                             data))
                         @data
                         urls)))

#_(defn print-schedule []
    (doseq [url urls]
      (let [url-data (get @data url)]
        (println (get-time (:show-time url-data)))
        (println url)
        (println (get-address (:address url-data)))
        (println "\n"))))

#_(load-data)
#_(print-schedule)

#_(run-tests)


