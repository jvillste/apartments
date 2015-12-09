(ns apartments.etuovi
  (:require [apartments.core :as core])
  (:use [clojure.test]))


(defn clean-value [value]
  (->> (flatten value)
       (filter string?)
       (map #(.trim %))
       (filter #(not= "" %))
       (interpose " ")
       (apply str)))

(defn get-section [page-hickup section-name]
  (if-let [path (core/find-term section-name page-hickup)]
    (let [section-hickup (as-> path x
                           (core/navigate x 3 [])
                           (get-in page-hickup x))]
      (as-> section-hickup x
        (core/find-term :dl x)
        (core/navigate x 1 [])
        (get-in section-hickup x)
        (->> x
             (filter #(and (vector? %)
                           (#{:dd :dt} (first %))))
             (map clean-value)
             (apply hash-map))))
    {}))


(defn get-show-time [hickup]
  (let [section-path (core/find-term  "Esittely" hickup)]
    (get-in hickup
            (-> section-path
                (core/navigate 3 [5 3 2])))))


(defn get-address-from-show-time [show-time]
  (->> (clojure.string/split show-time #", ")
       (drop 2)
       (interpose ", ")
       (apply str)))

(defn get-address-from-hickup [hickup]
  (let [section-path (core/find-term {:id "reference_number"} hickup)]
    (get-in hickup
            (-> section-path
                (core/navigate 3 [5 3 2])))))


(defn get-lot-data [hickup]
  (core/get-sections hickup
                get-section
                "Kohteen perustiedot"
                "Hinta ja kustannukset"
                "Kohteen lisätiedot"
                "Tontti ja kaavoitus"))

(defn get-query-results [hickup]
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

(defn get-all-query-results [query-url-base query-url]
  (loop [url query-url
         ids []]
    (Thread/sleep 1000)
    (let [hickup (core/get-hickup (str query-url-base url))
          next-url (next-url hickup)
          ids (concat ids (get-query-results hickup))]
      (if next-url
        (recur next-url
               ids)
        ids))))


;; test


#_(def hickup (core/get-hickup "http://www.etuovi.com/kohde/9511178"))

#_(get-section hickup "Tontti ja kaavoitus")

#_(get-lot-data hickup)

#_(get-section hickup "Kohteen perustiedot")

