(ns apartments.core
  (:require [clj-http.client :as http]
            [hickory.core :as hickory]
            [flow-gl.tools.trace :as trace]))



#_(trace/inspect-value hickup)


(defn find-term
  ([term hickup]
     (find-term term hickup []))

  ([term hickup path]
     (loop [hickup hickup
            index 0]
       (when-let [element (first hickup)]
         (let [new-path (conj path index)]
           (cond (and (string? element)
                      (.contains element term)
                      path)
                 new-path

                 (vector? element)
                 (if-let [result (find-term term element new-path)]
                   result
                   (recur (rest hickup)
                          (inc index)))

                 :default
                 (recur (rest hickup)
                        (inc index))))))))

(defn get-value [hickup term]
  (let [path (find-term term hickup)]
    (get-in hickup (concat (take (- (count path) 3)
                                 path)
                           [5 2]))))

(defn navigate [path back-steps forward-steps]
  (concat (take (- (count path) back-steps)
                path)
          forward-steps))

(defn get-section [hickup section-name]
  (let [section-path (find-term section-name hickup)]
    (loop [values {}
           term-index 0]
      (if-let [term (get-in hickup
                            (-> section-path
                                (navigate 2 [5 (+ 3 (* 2 term-index)) 3 3 2])))]
        (recur (assoc values term (get-value hickup term))
               (inc term-index))
        values))))

(defn get-data [url]
  (let [hickup (into [] (hickory/as-hiccup (hickory/parse (:body (http/get url)))))]
    
    (conj (get-section hickup "Perustiedot")
          (get-section hickup "Hintatiedot ja muut kustannukset")
          (get-section hickup "Talon ja tontin tiedot"))))



(comment

  (println (get (get-data "http://asunnot.oikotie.fi/card/9252072")
                "Sijainti"))
  
  (def path (find-term "Perustiedot" hickup))

  (trace/inspect-value (get-in hickup (take (- (count path) 2)
                                            path)))
  (def data [{} [[:bar "Bar"] "foo"]])

  (find-term "fo" data)

  (get-in data [1 1]))



