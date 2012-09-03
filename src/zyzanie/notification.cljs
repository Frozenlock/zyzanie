(ns zyzanie.notification
  (:require [zyzanie.core :as z]
            [siren.core :as s]))


;; ================= Utils =======================
(defn- reverse-map [m]
  (into {} (map (fn [[a b]] [b a]) m)))

(defn- char-from-code [keycode]
  (.fromCharCode js/String keycode))

(defn- add-div [string]
  (clojure.string/join (concat ["<div>"] string ["</div>"])))
;; ===============================================

(defn- name-from-chord
  "Given a single keychord, return the key names"
  [keychord]
  (let [key-name-map (reverse-map
                      (merge z/special-ks z/modifiers z/mouse-buttons))
        chord-list (reverse (for [key keychord]
                              (or (get key-name-map key)
                                  (.toLowerCase (char-from-code key)))))]
    (clojure.string/join "-" chord-list)))

(defn- names-from-keyseq
  "Return a human-readable string of a key sequence"
  [keyseq]
  (let [name-list (for [chord keyseq]
                    (name-from-chord chord))]
    (add-div (clojure.string/join " " name-list))))


(defn- translate-all-keyseq [keyseqs]
  (clojure.string/join (map #(add-div (names-from-keyseq %)) keyseqs)))

(def- keyseq-siren (s/continuous-siren-factory))

(def- completed-keyseq-siren (s/continuous-siren-factory))

(defn- show-current-keyseq []
  (let [keyseqs (vals @z/!keyseq)]
    (when (seq keyseqs)
      (keyseq-siren {:html-content (translate-all-keyseq keyseqs) :delay 700}))))

(defn- show-last-valid-keyseq []
  (completed-keyseq-siren
   {:html-content
    (str "Function called!" (names-from-keyseq @z/!last-valid-keyseq))
    :style :light :delay 1500}))


;; ================ API ================

(defn add-notifications
  "Add notifications for every key sequence and function called." []
  (add-watch z/!last-valid-keyseq :notif-function-call #(show-last-valid-keyseq))
  (add-watch z/!keyseq :notif-key #(show-current-keyseq)))

(defn remove-notifications
  "Remove notification" []
  (remove-watch z/!keyseq :notif-key)
  (remove-watch z/!last-valid-keyseq :notif-function-call))