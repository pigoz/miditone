(ns
  ^{:doc "parse a midi file and returns notes", :author "Stefano Pigozzi"}
  miditone.reader
  (:import (java.io File FileNotFoundException)
           (javax.sound.midi MidiSystem Sequence MetaMessage ShortMessage)))

;; convert java object responding to .size and .get to a seq
(defn- jseq [jo]
  (map #(.get jo %) (range (.size jo))))

(defn- jmap [fn jo]
  (map fn (jseq jo)))

(defn- fmap [fn m]
  (zipmap (keys m) (map fn (vals m))))

;; Instead of dealing with MIDI's timing quirks, use getMicrosecondLength and
;; getTickLength
;; (def mspm 60000000.0) ;; MicroSeconds Per Minute
;; (defn- mpqn2bpm [tempo] ;; Micorseconds Per QuarterNote => Beats Per Minute
;;   (cond
;;     (<= tempo 0) 0.1
;;     :else (/ 60000000.0 tempo)))
;; 
;; (defn- midi-bpm [d]
;;   (let [fst (bit-shift-left (bit-and (nth d 0) 0xff) 16)
;;         snd (bit-shift-left (bit-and (nth d 1) 0xff) 8)
;;         trd (bit-and (nth d 2) 0xff)
;;         tempo (bit-or fst snd trd)]
;;     (/ mspm tempo)))

(defmulti #^{:private true} parse-message class)
;; (defmethod parse-message MetaMessage [m] 
;;   (let [t (.getType m) data (.getData m)]
;;     (cond
;;       ;; Set Tempo Meta Message
;;       (= t 0x51) {:meta :bpm  :value (midi-bpm data)}
;;       (= t 0x58) {:meta :mcmt :value (bit-and (nth data 2) 0xff)}
;;       :else nil)))

(defmethod parse-message ShortMessage [m] {
  :key      (.getData1 m)
  :velocity (.getData2 m)
  :command  ({0x90 :on 0x80 :off} (.getCommand m))})
(defmethod parse-message :default [message] nil)

(defn- parse-event [event]
  (let [message (parse-message (.getMessage event))]
    (cond
      (nil? message) nil
      :else          (assoc message :tick (.getTick event)))))

(defn- events [track]
  (remove nil? (jmap parse-event track)))

(defn- tracks [seq]
  (.getTracks seq))

(defn- midi-sequence [filename]
  (try
    (MidiSystem/getSequence (File. filename))
    (catch FileNotFoundException e '())
  ))

(defn- events-pairs [events]
  (fmap (partial partition 2) (group-by :key events)))

(defn- notes [events]
  (let [key-map  (events-pairs events)
        merge-fn (fn [on off] (assoc on :duration (- (off :tick) (on :tick))))]
    (flatten (vals (fmap (partial map #(apply merge-fn %)) key-map)))))

(defn- file-visitor [filename ntrack visitor filterfn]
  (let [midiseq (midi-sequence filename)
        track   (nth (tracks midiseq) ntrack)
        notes   (notes (events track))]
    (filter filterfn (sort-by :tick (map visitor notes)))))

(defn- parse-notes [filename ntrack]
  (file-visitor
    filename
    ntrack
    (fn [note] (select-keys note [:tick :duration :key]))
    (fn [item] true)))

;; (defn- parse-meta [filename ntrack]
;;   (file-visitor
;;     filename
;;     ntrack
;;     (fn [note] (select-keys note [:tick :meta :value]))
;;     (fn [item] (:meta item))))
;; 
;; (defn- lookup-meta [m coll]
;;   (:value (first (filter #(= (:meta %) m) coll))))

(defn- tick-to-us [filename tick]
  (let [s  (midi-sequence filename)
        us (float (.getMicrosecondLength s))
        tk (float (.getTickLength s))]
    (* tick (/ us tk))))

(defn- tick-to-ms [filename tick]
  (/ (tick-to-us filename tick) 1000))

(defn parse-file [filename track]
  (let [events (parse-notes filename track)
        tconv  (partial tick-to-ms filename)]
    (map #(assoc % :tick (tconv (:tick %)) :duration (tconv (:duration %))) events)))

(comment
  (def f "/Users/pigoz/Downloads/The_Legend_of_Zelda_-_Ocarina_of_Time_-_Hyrule_Castle_Courtyard_by_BlueSCD.mid")
  (def f "/Users/pigoz/Downloads/smb_over.mid")
  (parse-file f 1)
)
