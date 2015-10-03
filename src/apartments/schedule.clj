(ns apartments.schedule
  (:require [apartments.core :as core]
            [clojure.string :as string])
  (:use [clojure.test]))



(def urls ["http://www.etuovi.com/kohde/9466019"
           "http://www.etuovi.com/kohde/492541"
           "http://www.etuovi.com/kohde/9999914"
           "http://www.etuovi.com/kohde/533694"])


(defn get-time [show-time]
  (-> show-time
      (clojure.string/split #" ")
      (nth 2)))

(defn get-etuovi-show-time [hickup]
  (let [section-path (core/find-term  "Esittely" hickup)]
    (get-in hickup
            (-> section-path
                (core/navigate 3 [5 3 2])))))


(defn get-address [show-time]
  (->> (clojure.string/split show-time #", ")
       (drop 2)
       (interpose ", ")
       (apply str)))

(defn get-etuovi-address [hickup]
  (let [section-path (core/find-term {:id "reference_number"} hickup)]
    (get-in hickup
            (-> section-path
                #_(core/navigate 1 [3 5 5 3 2])
                (core/navigate 3 [5 3 2])))))

(defonce hickup (core/get-hickup "http://www.etuovi.com/kohde/9999914"))

(deftest get-test
  (is (= ""
         (get-etuovi-address hickup))))

(defn get-data [url]
  (let [hickup (core/get-hickup url)]
    {:url url
     :show-time (get-etuovi-show-time hickup)
     :address (get-etuovi-address hickup)}))

(defonce data (atom {}))

(defn load-data []
  (reset! data (reduce (fn [data url]
                         (if (not (contains? data url))
                           (assoc data url (get-data url))
                           data))
                       @data
                       urls)))



(defn print-schedule []
  (doseq [url urls]
    (let [url-data (get @data url)]
      (println (get-time (:show-time url-data)))
      (println url)
      (println (get-address (:address url-data)))
      (println))))

(load-data)
(print-schedule)

#_(run-tests)


