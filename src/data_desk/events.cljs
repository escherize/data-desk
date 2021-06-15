(ns data-desk.events
  (:require
   [re-frame.core :as re-frame]
   [data-desk.db :as db]
   ))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))
