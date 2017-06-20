(ns lead-control.core
  (:require [aleph.tcp :as tcp]
            [byte-streams]
            [clojure.spec :as s]
            [gloss.core :as gloss :refer [defcodec enum header]]
            [gloss.io :as io]
            [manifold.deferred :as d]
            [manifold.stream :as stream]))

(defcodec mode
  (enum :uint16 {:on-off 0x0212 :brightness 0x0833 :color-temp 0x0836}))

(defcodec on-off
  {:mode :on-off :val (enum :uint16 {:on 0xabc3 :off 0xa9c1})})

(defcodec brightness
  {:mode :brightness :val :uint16})

(defcodec color-temp
  {:mode :color-temp :val :uint16})

(defcodec protocol
  (header mode {:on-off on-off :brightness brightness :color-temp color-temp} :mode))

(defn frame [x]
  (vec
    (concat
      [(byte-streams/to-byte-buffer (byte-array [0x55 0x99 0x2e 0xa7 0x02 0x02]))]
      (io/encode protocol x)
      [(byte-streams/to-byte-buffer (byte-array [0xaa 0xaa]))])))

(s/def ::brightness
  #(s/int-in-range? 0 65 %))

(s/fdef brightness
  :args (s/cat :brightness ::brightness))

(defn brightness [x]
  (+ (bit-shift-left x 8) x 0x3f))

(s/def ::color-temp
  #(s/int-in-range? 0 33 %))

(s/fdef color-temp
  :args (s/cat :color-temp ::color-temp))

(defn color-temp [x]
  (+ (bit-shift-left x 8) x 0x42))

(defonce state
  (atom
    {:state :off
     :brightness 64
     :color-temp 32}))

(defn post! [host port command]
  (with-open [client @(tcp/client {:host host :port port})]
    @(stream/put! client (frame command))))

(defn sync-fn [host port]
  (fn [_ _ old-state new-state]
    (when (not= (:state old-state) (:state new-state))
      (post! host port {:mode :on-off :val (:state new-state)}))
    (when (not= (:brightness old-state) (:brightness new-state))
      (post! host port {:mode :brightness :val (brightness (:brightness new-state))}))
    (when (not= (:color-temp old-state) (:color-temp new-state))
      (post! host port {:mode :color-temp :val (color-temp (:color-temp new-state))}))))

(comment
  (require '[clojure.spec.test :as st])
  (st/instrument)

  (add-watch state :send (sync-fn "192.168.178.26" 8899))

  (swap! state assoc :state :on)
  (swap! state assoc :state :off)
  (swap! state assoc :brightness 32)
  (swap! state assoc :brightness 64)
  (swap! state assoc :color-temp 0)
  (swap! state assoc :color-temp 8)
  (swap! state assoc :color-temp 16)
  (swap! state assoc :color-temp 32)

  )
