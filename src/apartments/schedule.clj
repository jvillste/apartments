(ns apartments.schedule
  (:require [apartments.core :as core]
            [clojure.string :as string])
  (:use [clojure.test]))



#_(def urls ["http://www.etuovi.com/kohde/9466019"
           "http://www.etuovi.com/kohde/492541"
           "http://www.etuovi.com/kohde/9999914"
           "http://www.etuovi.com/kohde/533694"
           "http://www.etuovi.com/kohde/9596073"
           "http://www.etuovi.com/kohde/493617"
           "http://www.etuovi.com/kohde/493548"
           "http://www.etuovi.com/kohde/9694964"])

#_(def urls ["http://www.etuovi.com/kohde/c23273"
           "http://www.etuovi.com/kohde/9636648"
           "http://www.etuovi.com/kohde/9744457"
           "http://www.etuovi.com/kohde/7653981"
           "http://www.etuovi.com/kohde/9864164"
           "http://www.etuovi.com/kohde/7199361"])

(def urls ["http://www.etuovi.com/kohde/9517941"])


#_(defonce hickup (core/get-hickup "http://www.etuovi.com/kohde/9999914"))

#_(deftest get-test
  (is (= ""
         (get-etuovi-address hickup))))

(defn get-data [url]
  (let [hickup (core/get-hickup url)]
    {:url url
     :show-time (core/get-etuovi-show-time hickup)
     :address (core/get-etuovi-address hickup)}))

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
      (println "\n"))))

(load-data)
(print-schedule)

#_(run-tests)


