(ns apartments.core
  
  (:require [clojure.core.async :as async]
            [flow-gl.csp :as csp]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layout-dsl :as l]
                         [controls :as controls]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [transformer :as transformer])
            (flow-gl.graphics [font :as font])
            [clj-http.client :as http]
            [hickory.core :as hickory]
            [flow-gl.tools.trace :as trace])
  (:import [java.awt Desktop]
           [java.net URI])
  (:use [clojure.test]))

(defn find-term
  ([term hickup]
   (find-term term hickup []))

  ([term hickup path]
   (loop [hickup hickup
          index 0]
     (when-let [element (first hickup)]
       (let [new-path (conj path index)]
         (cond (= element term)
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

(defn get-time [show-time]
  (-> show-time
      (clojure.string/split #" ")
      (nth 2)))

(defn get-etuovi-show-time [hickup]
  (let [section-path (find-term  "Esittely" hickup)]
    (get-in hickup
            (-> section-path
                (navigate 3 [5 3 2])))))


(defn get-address [show-time]
  (->> (clojure.string/split show-time #", ")
       (drop 2)
       (interpose ", ")
       (apply str)))

(defn get-etuovi-address [hickup]
  (let [section-path (find-term {:id "reference_number"} hickup)]
    (get-in hickup
            (-> section-path
                (navigate 3 [5 3 2])))))


(defn get-hickup [url]
  (into [] (hickory/as-hiccup (hickory/parse (:body (http/get url))))) )

(defn get-data [url]
  (let [hickup (get-hickup url)]
    
    (conj (get-section hickup "Perustiedot")
          (get-section hickup "Hintatiedot ja muut kustannukset")
          (get-section hickup "Talon ja tontin tiedot"))))





(defonce data (atom {}))



(defn load-data [urls]
  (reset! data (reduce (fn [data url]
                         (if (not (contains? data url))
                           (assoc data url (get-data url))
                           data))
                       @data
                       urls)))



(comment

  
  (println (get (get-data "http://asunnot.oikotie.fi/myytavat-asunnot/9559356")
                "Sijainti"))


  (http/get "http://asunnot.oikotie.fi/myytavat-asunnot/9559356")
  
  (def path (find-term "Perustiedot" hickup))

  (trace/inspect-value (get-in hickup (take (- (count path) 2)
                                            path)))
  (def data [{} [[:bar "Bar"] "foo"]])

  (find-term "fo" data)

  (get-in data [1 1]))

;;; GUI


(defn text
  ([value]
   (text value [255 255 255 255]))

  ([value color]
   (drawable/->Text (str value)
                    (font/create "LiberationSans-Regular.ttf" 15)
                    color)))

(defn do-async [view-context task finish]
  (let [data-channel (async/thread (task))]
    (async/go (let [data (async/<! data-channel)]
                (gui/apply-to-state view-context (fn [state]
                                                   (finish state data)))))))


(defn cells [apartment & keys]
  (for [key keys]
    (l/margin 3 3 3 3 (text (get apartment key)))))

(defn create-text-editor-keyboard-event-handler [view-context]
  (fn [state event]
    (cond
      (events/key-pressed? event :back-space)
      (gui/update-binding state
                          view-context
                          (fn [text] (apply str (drop-last text)))
                          :text)

      (and (:character event)
           (= (:type event)
              :key-pressed))
      (gui/update-binding state
                          view-context
                          #(str % (:character event))
                          :text)

      :default
      state)))

(defn loca-x-to-rating [local-x width margin shift]
  (let [rating (int (min 100
                         (max 0
                              (* 100
                                 (/ (- local-x margin)
                                    width)))))
        rating (if shift
                 (* (int (/ rating 10)) 10)
                 rating)]
    rating))

(defn rating-view [view-context state]
  (let [margin 5
        width 200
        height 10
        total-width (+ width (* 2 margin))
        total-height (+ height (* 2 margin))]
    (-> (l/superimpose (drawable/->Rectangle total-width
                                             total-height
                                             (if (:rating state)
                                               [50 50 50 255]
                                               [100 100 100 255]))
                       (when (:rating state)
                         (l/margin margin 0 0 margin
                                   (drawable/->Rectangle (* (/ (:rating state) 100)
                                                            width)
                                                         height
                                                         [200 200 0 255])))
                       
                       (l/center total-width total-height
                                 (drawable/->Text (str (or (:rating state) "-"))
                                                  (font/create "LiberationSans-Regular.ttf" 12)
                                                  [0 0 0 255])))
        (gui/on-mouse-clicked-with-view-context view-context
                                                (fn [state event]
                                                  (assoc state
                                                         :rating
                                                         (loca-x-to-rating (:local-x event)
                                                                           width
                                                                           margin
                                                                           (:shift event)))))
        (gui/on-mouse-event-with-view-context :mouse-dragged view-context 
                                              (fn [state event]
                                                (assoc state
                                                       :rating
                                                       (loca-x-to-rating (:local-x event)
                                                                         width
                                                                         margin
                                                                         (:shift event))))))))

(defn rating [view-context]
  {:local-state {}
   :view #'rating-view})


(defn view [view-context state]
  (l/margin 100 100 100 100
            (l/preferred (gui/call-view rating :rating)))
  #_(l/vertically (l/horizontally (controls/button view-context
                                                   "Load"
                                                   (fn [state]
                                                     (let [data-channel (async/thread (load-data))]
                                                       (async/go (let [data (async/<! data-channel)]
                                                                   (gui/apply-to-state view-context (fn [state]
                                                                                                      (assoc state
                                                                                                             :loading false
                                                                                                             :data data)))))
                                                       (assoc state :loading true))))
                                  (controls/button view-context
                                                   "Save"
                                                   (fn [state]
                                                     (do-async view-context
                                                               (fn []
                                                                 (Thread/sleep 1000)
                                                                 (spit "data.clj" @data)
                                                                 :result)
                                                               (fn [state result]
                                                                 (assoc state :saving false)))
                                                     
                                                     (assoc state :saving true)))
                                  
                                  (controls/button view-context
                                                   "Load from disk"
                                                   (fn [state]
                                                     (reset! data (slurp  "data.clj"))
                                                     (assoc state :data @data)))
                                  
                                  (when (:loading state) (text "Loading"))
                                  (when (:saving state) (text "Saving")))
                  
                  (layouts/grid (for [[url apartment] (:data state)]
                                  (concat [(-> (text (str (get apartment "Sijainti")))
                                               (gui/on-mouse-clicked (fn [state event]
                                                                       (.browse (Desktop/getDesktop)
                                                                                (URI. url))
                                                                       state)))
                                           (text (str (get apartment "Kattomateriaali")))
                                           (text (str (get apartment "Velaton hinta")))
                                           (text (str (get apartment "Tontin pinta-ala")))]
                                          [(text "haa")]
                                          (cells "Lämmitys"))))))




(defn apartments [view-context]
  {:local-state {:data @data}
   :view #'view})

(defn start []
  (gui/start-control apartments))



#_(gui/redraw-last-started-view)

(run-tests)
