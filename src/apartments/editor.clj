(ns apartments.editor
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layout-dsl :as l]
                         [controls :as controls]
                         [gui :as gui]
                         [transformers :as transformers]
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
           :ids-to-tempids-map (data/ids-to-tempids (:db-after result)
                                                    (:tempids result)
                                                    (data/tempids changes)))))

(defn attribute-editor [view-context state entity-id attribute]
  (let [old-value (data/value (:db state)
                              entity-id
                              attribute)
        new-value (data/value (:db-with-changes state)
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
                                                                              (data/set-attribute-value (:changes state)
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
  (let [margin 5]
    (l/margin margin margin margin margin content)))

(defn table-view [view-context state]
  (-> (let [entities (data/get-apartment-entities (:db state))]
        (l/table 1
                 (for [entity (take 40 entities)]
                   [(-> (text (:apartments/address entity))
                        (gui/on-mouse-clicked (fn [state event]
                                                (.browse (Desktop/getDesktop)
                                                         (URI. (str lots/lot-url-base (:apartments/id entity))))
                                                state)))
                    (attribute-editor (:root-view-context state)
                                      state
                                      (:db/id entity)
                                      :apartments/comment)])))
      (assoc :transformer (assoc transformers/cache
                                 :id :cache))))

(defn table [view-context]
  {:local-state {}   
   :view #'table-view})

(defn root-view [view-context state]
  (l/float-top (l/horizontally (-> (button "Save")
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
                                                                             (set-changes state [])))))
               
               (l/vertically

                (gui/call-view controls/scroll-panel
                               :apartments-scroll-panel
                               {:content (gui/call-view table
                                                        :table (assoc (select-keys state [:db :db-with-changes])
                                                                      :root-view-context view-context))})
                
                (text (vec (:changes state))))))

(defn root [db-uri]
  (fn [view-context]
    {:local-state (-> {:db-uri db-uri
                       :changes []}
                      (connect)
                      (refresh))
     
     :view #'root-view}))


(trace/trace-ns 'flow-gl.gui.gui)

(defn start []
  #_(gui/start-redrawable-control (root data/db-uri))

  (trace/with-trace
    (gui/start-control (root data/db-uri))))

(gui/redraw-last-started-redrawable-control)

(comment
  (defn foo []
    (foo))

  (defn stack-frame-to-map [stack-frame]
    {:method-name (.getMethodName stack-frame)
     :class-name (.getClassName stack-frame)
     :file-name (.getFileName stack-frame)
     :line-number (.getLineNumber stack-frame)})

  (defn bar []
    (throw (Exception. "FAIL!"))  )

  #_(stack-frame-to-map stack-frame)

  (try
    (foo)
    #_(throw (Exception. "FAIL!"))
    (catch Throwable e
      (def stack-trace (.getStackTrace e))
      #_(def stack-frame (first  (.getStackTrace e)))
      #_(println (stack-frame-to-map (last (.getStackTrace e)))))))




