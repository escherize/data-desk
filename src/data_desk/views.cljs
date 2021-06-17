(ns data-desk.views
  (:require
   [reagent.core :as r]
   [re-frame.core :as re-frame]
   [re-com.core :as re-com :refer [at]]
   [data-desk.subs :as subs]
   [cljs.reader :as reader]
   [malli.core :as m]
   [clojure.string :as str]))

(defn title []
  (let [name (re-frame/subscribe [::subs/name])]
    [re-com/title
     :src   (at)
     :label "data-desk"
     :level :level2]))

(def default-schema '
  [:map [:my-int int?]
      [:stringies [:vector string?]]
      [:another-thing [:map
                       [:a-bool-value boolean?]
                       [:an-int-value int?]]]
   [:vec-o-maps [:vector [:map
                          [:a-bool-value boolean?]
                          [:an-int-value int?]]]]])

  (defonce local-state (r/atom {:schema-text
                                (str/join "\n"
                                          ["[:map [:my-int int?]"
                                           "      [:stringies [:vector string?]]"
                                           "      [:vec-o-maps [:vector [:map"
                                           "                            [:a-bool-value boolean?]"
                                           "                            [:an-int-value int?]]]])"])
                              :schema-value default-schema}))

(defmulti editor (fn [schema edit-path] [(pr-str (m/type schema))]))

(defmethod editor ["boolean?"]
  [_schema edit-path]
  (swap! local-state assoc-in edit-path true)
  (fn [schema edit-path]
    [:div
     ;;[:pre (pr-str edit-path)]
     [re-com/checkbox
      :model (get-in @local-state edit-path)
      :on-change #(swap! local-state assoc-in edit-path %)]]))

(defmethod editor ["string?"]
  [_schema edit-path]
  (swap! local-state assoc-in edit-path (str "string " (rand-nth (map str "ABCDEFGHIJK"))))
  (fn [schema edit-path]
    [:div
     ;;[:pre (pr-str edit-path)]
     [re-com/input-text
      :model (get-in @local-state edit-path)
      :on-change #(swap! local-state assoc-in edit-path %)]]))

(defmethod editor ["int?"]
  [_schema edit-path]
  (swap! local-state assoc-in edit-path (rand-nth (range -1000 1000)))
  (fn [schema edit-path]
    [:div
     ;;[:pre (pr-str edit-path)]
     [re-com/input-text
      :model (str (get-in @local-state edit-path))
      :validation-regex #"^\s*-?[0-9]{1,10}\s*$"
      :on-change #(swap! local-state assoc-in edit-path (reader/read-string %))]]))

(defn vec-drop-idx [idx coll]
  ;; (vec-drop-idx [:0 :1 :2 :3 :4] 2) => [:0 :1 :3 :4]
  (vec (concat
        (take idx coll)
        (drop (inc idx) coll))))

(defmethod editor [":vector"]
  [schema edit-path]
  (swap! local-state assoc-in edit-path [])
  (fn [schema edit-path]
    (let [len (count (get-in @local-state edit-path))]
      (js/console.log "LEN" len)
      [:div {:style {:padding 10
                     :margin 10
                     :border "1px rgba(20,200,40,0.8) solid"
                     :background-color "rgba(20,230,40,0.05)"}}
       (doall
        (for [idx (range len)]
          [:div {:key idx}
           idx ":"
           [re-com/button
             :label "x"
             :on-click #(swap! local-state update-in
                               edit-path
                               (partial vec-drop-idx idx))]
           [:div
            [editor
             (m/schema (first (m/children schema)))
             (conj (vec edit-path) idx)]]]))
       ;;"+ button"
       [re-com/button
        :label "+"
        :on-click #(swap! local-state assoc-in
                          (conj (vec edit-path) len)
                          "?")]])))

(defmethod editor [":map"]
  [schema edit-path]
  (swap! local-state assoc-in edit-path {})
  (fn [schema edit-path]
    [:div {:style {:padding 15
                   :margin 15
                   :border "1px rgba(20,40,200,0.8) solid"
                   :background-color "rgba(20,40,230,0.05)"}}
     (doall
      (for [[k _ child-schema] (m/children schema)]
        [:div {:key k}
         k ":"
         [:div
          [editor
           child-schema
           (conj (vec edit-path) k)]]]))]))

(defmethod editor :default [schema edit-path]
  (js/console.log (pr-str ["schema was:" (keyword (m/type schema))]))
  [:pre "Broken editor"])

(defn test-panel []
  [re-com/v-box
   :src (at)
   :style {:margin "20px"
           :padding "20px"
           :border "2px solid red"}
   :children [[:div
               [re-com/title
                :label "boolean?"
                :level :level3]
               (editor boolean?)]
              [:div
               [re-com/title
                :label "string?"
                :level :level3]
               (editor string?)]
              [:div
               [re-com/title
                :label "int?"
                :level :level3 ]
               (editor int?)]
              [:div
               [re-com/title
                :label "[:vector boolean?]"
                :level :level3]
               (editor [:vector boolean?])]

              [:div
               [re-com/title
                :label "[:map [:x string?]]"
                :level :level3 ]
               (editor [:map [:x string?]])]

              [:div
               [re-com/title
                :label "[:map [:foo [:map [:bar boolean?] [:baz string?] [:qux [:vector [:map [:bar boolean?] [:baz string?] [:qux [:vector string?]]]]]]]]"
                :level :level3 ]
               (editor [:map [:foo [:map [:bar boolean?] [:baz string?] [:qux [:vector [:map [:bar boolean?] [:baz string?] [:qux [:vector string?]]]]]]]])]

              [:div
               [re-com/title
                :label "[:vector [:map [:x string?]]]"
                :level :level3 ]
               (editor [:vector [:map [:x string?]]])]

              [:div
               [re-com/title
                :label "[:map [:s string?] [:i int?] [:b boolean?]]"
                :level :level3]
               (editor [:map [:s string?] [:i int?] [:b boolean?]])]

              [:div
               [re-com/title
                :label "[:vector [:map [:strang string?] [:eient int?] [:bvool boolean?] [:VEKTER [:vector [:map [:s string?] [:i int?] [:b boolean?]]]]]]"
                :level :level3]
               (editor [:vector [:map [:strang string?] [:eient int?] [:bvool boolean?] [:VEKTER [:vector [:map [:s string?] [:i int?] [:b boolean?]]]]]])]]])

(defn safe-read [maybe-code-string]
  (try (reader/read-string maybe-code-string)
       (catch :default _ nil)))

(js/console.log
 (pr-str
  (safe-read "[:vector [:map [:my-int int?]]]  ")))

(defn main-panel []
  (fn []
    [re-com/v-box
     :src      (at)
     :style {:margin "20px"}
     :height   "100%"
     :children [[title]

                [re-com/input-textarea
                 :src (at)
                 :model (str (get-in @local-state [:schema-text]))
                 :rows 5
                 :style {:width "500px"}
                 :on-change (fn [v]
                              (swap! local-state assoc :schema-text v)
                              (when-let [code (safe-read v)]
                                (when
                                    (not= code (:schema-value @local-state))
                                  (swap! local-state assoc :schema-value int?)
                                  (swap! local-state
                                         #(-> %
                                              (assoc :root nil)
                                              (assoc :schema-value code))))))]

                (js/console.log (pr-str (:schema-value @local-state)))
                (js/console.log (m/type (:schema-value @local-state)))

                [re-com/line]

                [editor (:schema-value @local-state) [:root]]

                [re-com/line]

                [:div
                 [re-com/title :label "as json"]
                 [:pre (js/JSON.stringify
                        (clj->js (:root @local-state))
                        nil
                        2)]]

                #_[test-panel]]]))
