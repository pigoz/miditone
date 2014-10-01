(ns
  ^{:doc "playback a midi file parsed with parser", :author "Stefano Pigozzi"}
  miditone.player
  (:use [overtone.live] [overtone.inst.piano])
  (:require [miditone.reader :as midi-reader]))

;; stolen shit is stolen
(definst bell [frequency 440 duration 10
               h0 1 h1 0.6 h2 0.4 h3 0.25 h4 0.2 h5 0.15]
  (let [harmonic-series [ 1  2  3  4  5  6]
                        ;[ 1  2  3  4.2 5.4 6.8]
        proportions     [h0 h1 h2 h3 h4 h5]
        component
         (fn [harmonic proportion]
           (* 1/2
              proportion
              (env-gen (perc 0.01 (* proportion duration)))
              (sin-osc (* harmonic frequency))))
        whole
          (mix (map component harmonic-series proportions))]
    (detect-silence whole :action FREE)
    whole))

(defn ding [midi dur] (bell (midi->hz midi) dur))

(comment
  (def f "/Users/pigoz/Downloads/The_Legend_of_Zelda_-_Ocarina_of_Time_-_Hyrule_Castle_Courtyard_by_BlueSCD.mid")
  (def f "/Users/pigoz/Downloads/smb_over.mid")
  (def r (midi-reader/parse-file f 1))
  (map #(at (+ (now) (:tick %)) (ding (:key %) (/ (:duration %) 1000))) r)
)
