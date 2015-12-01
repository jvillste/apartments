(ns apartments.editor
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layout-dsl :as l]
                         [controls :as controls]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [transformer :as transformer])
            (apartments [data :as data]
                        [lots :as lots])
            [datomic.api :as d]
            (flow-gl.opengl.jogl [quad :as quad]
                                 [render-target :as render-target]
                                 [opengl :as opengl])
            (flow-gl.tools [profiler :as profiler]
                           [trace :as trace])
            (flow-gl.graphics [font :as font]))
  (:import [javax.media.opengl GL2]
           [java.awt Desktop]
           [java.net URI])
  (:use flow-gl.utils
        clojure.test))


(defn text
  ([value]
   (text value [255 255 255 255]))

  ([value color]
   (text value color 15))
  
  ([value color size]
   (drawable/->Text (str value)
                    (font/create "LiberationSans-Regular.ttf" size)
                    color)))

(defn button [text-value]
  (layouts/->Box 10 [(drawable/->Rectangle 0
                                           0
                                           [0 200 200 1])
                     (text text-value)]))

(defn set-changes [state changes]
  (let [result (d/with (:db state)
                       changes)]
    (assoc state
           :changes changes
           :db-with-changes (:db-after result)
           :ids-to-tempids-map (ids-to-tempids (:db-after result)
                                               (:tempids result)
                                               (tempids changes)))))

(defn attribute-editor [view-context state entity-id attribute]
  (let [old-value (value (:db state)
                         entity-id
                         attribute)
        new-value (value (:db-with-changes state)
                         entity-id
                         attribute)
        changed-value-key [entity-id attribute]]
    (l/horizontally (-> (gui/call-view controls/text-editor [:editor changed-value-key])
                        (update-in [:state-overrides]
                                   assoc
                                   :text new-value)
                        (update-in [:constructor-overrides]
                                   assoc [:on-change :text]
                                   (fn [global-state new-value]
                                     (gui/apply-to-local-state global-state
                                                               view-context
                                                               (fn [state]
                                                                 (set-changes state
                                                                              (set-attribute-value (:changes state)
                                                                                                   (or (get (:ids-to-tempids-map state)
                                                                                                            entity-id)
                                                                                                       entity-id)
                                                                                                   attribute
                                                                                                   new-value)))))))
                    (when (not= old-value new-value)
                      (text "*" [255 0 0 255] 30)))))


(defn connect [state]
  (assoc state :conn (d/connect (:db-uri state))))

(defn refresh [state]
  (-> (assoc state :db (d/db (:conn state)))
      (set-changes (:changes state))))

(defn cell [content]
  (let [margin 5 ]
    (l/minimum-size 0 23 (l/margin margin margin margin margin content))))

(defn root-view [view-context state]
  
  (l/vertically

   (let [entities (lots/get-apartment-entities (:conn state))]
     (l/horizontally (l/vertically (for [entity entities]
                                     (-> (cell (text (:apartments/address entity)))
                                         (gui/on-mouse-clicked (fn [state event]
                                                                 (.browse (Desktop/getDesktop)
                                                                          (URI. (str lots/lot-url-base (:apartments/id entity))))
                                                                 state)))))
                     (l/vertically (for [entity entities]
                                     (cell (attribute-editor view-context
                                                             state
                                                             (:db/id entity)
                                                             :apartments/comment))))))
   
   

   (-> (button "Save")
       (gui/on-mouse-clicked-with-view-context view-context
                                               (fn [state event]
                                                 (d/transact (:conn state)
                                                             (:changes state))
                                                 (-> state
                                                     (assoc :db (d/db (:conn state)))
                                                     (set-changes [])))))
   (-> (button "Refresh")
       (gui/on-mouse-clicked-with-view-context view-context
                                               (fn [state event]
                                                 (refresh state))))
   (-> (button "Cancel")
       (gui/on-mouse-clicked-with-view-context view-context
                                               (fn [state event]
                                                 (set-changes state []))))
   (text (vec (:changes state)))))

(defn root [db-uri]
  (fn [view-context]
    {:local-state (-> {:db-uri db-uri
                       :changes []}
                      (connect)
                      (refresh))
     
     :view #'root-view}))

(defonce event-channel (atom nil))

(defn start []
  (reset! event-channel (gui/start-control (root data/db-uri)))

  #_(trace/with-trace
      (trace/log "start")
      (gui/start-control (root data/db-uri))))


(when @event-channel
  (gui/redraw-app @event-channel))