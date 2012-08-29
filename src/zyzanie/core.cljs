(ns zyzanie.core
  (:require [clojure.browser.dom :as dom]
            [domina :as domina]
            [domina.events :as events]
            [clojure.string :as string]))


;;; Highly inspired by https://github.com/AndreasKostler/dyscord

;;; We use the bubble phases of event propegation (from the event node
;;; upward) to go throught all possible elements. This simulate the
;;; 'local key' by testing the inner nodes first.

;;; We must however be very careful: this requires to have multiple
;;; event listeners, which will call the same handler multiple times
;;; for a single key.

;;; The 'solution' is to accumulate every keystroke history in a map
;;; divided by dom element. The first element to have a matching
;;; key-sequence history trigger its associated command and reset the
;;; entire history for every element.


(def !key-maps
  "A place where every key-binding is stored"
  (atom {}))

(def modifiers
  (reduce (fn [m [k v]]
            (assoc m k v))
          { "S" 16
            "shift" 16
            "M" 18
            "alt" 18
            "option" 18
            "C" 17
            "ctrl" 17
            "control" 17
            "cmd" 91
            "command" 91}
          (for [k (range 1 20)] [(str "f" k) (+ 111 k)])))

(def special-ks
  { "backspace" 8
    "tab" 9
    "clear" 12
    "enter" 13
    "return" 13
    "esc" 27
    "escape" 27
    "space" 32
    "left" 37
    "up" 38
    "right" 39
    "down" 40
    "del" 46
    "delete" 46
    "home" 36
    "end" 35
    "pageup" 33
    "pagedown" 34
    "," 188
    "." 190
    "/" 191
    "`" 192
    "-" 189
    "=" 187,
    ";" 186
    "'" 222
    "[" 219
    "]" 221
    "\\" 220})

(defn- get-keycode
  "Return the keycode (number) of the key given as a string."
  [key]
  (or (get special-ks key)
      (get modifiers key)
      (.charCodeAt (.toUpperCase key) 0)))


(defn- canonicalize-keyseq
  "Separate key chords and key sequence. Return a vector of sets."
  [kseq]
  (vec
   (for [chord (remove string/blank? (string/split kseq #"[\t ]"))]
     (let [keys (string/split chord #"-")
           keycodes (map get-keycode keys)]
       (when (every? identity keycodes)
         (set keycodes))))))


(defn- canonicalize-command-key
  "If it's the command-key, return its keycode. Otherwise, simply
  return key."
  [key]
  (if (or (== key 93)
          (== key 224))
    91
    key))


(def modifier {16 false 18 false 17 false 91 false})

(def mods (atom {}))

(defn- reset-mods! []
  (reset! mods {}))

(defn- set-modifier!
  "Set the modifier history by dividing it by element"
  [key element boolean]
  (swap! mods #(merge % {element
                         (assoc (or (get % element)
                                    modifier) key boolean)})))

(defn- modifier?
  "Return true if key is a modifier."
  [key]
  (find modifier key))


(defn- get-chord
  "If a modifier has been pressed recently, add it to the set."
  [key element]
  (set (conj (map first (filter (fn [[_ v]] (true? v)) (get @mods element))) key)))



(def keyseq (atom {}))

(defn- set-keyseq!
  "Set the key sequence history by dividing it by element"
  [chord element]
  (swap! keyseq #(merge % {element (conj (or (get % element) []) chord)})))


                           
(defn- reset-keyseq! []
  (reset! keyseq {}))

(defn- modifier-pressed? [element]
  (some (fn [[_ v]] (true? v)) (get @mods element)))


(defn- prevent-default-if-needed
  "Check if the current modifier and key chord are required for a
  local key-binding. If it is, prevent the default browser behavior."
  [key-bindings chord event]
  (when (and (> (count chord) 1);at least two keys
             (some #(some (fn [a] (= chord a)) %) (keys key-bindings)))
    (events/prevent-default event)))

  
(defn- key-down! [event]
  "At each keydown, check if we match a key sequence. If yes, reset
the current key sequence and call the associated command."
  (let [raw-event (events/raw-event event)
        element (events/current-target event)
        key (canonicalize-command-key (.-keyCode raw-event))]
    ;(dom/log (str "You pressed: " key)) ;debug only
    (if (modifier? key)
      (set-modifier! key element true)
      (let [chord (get-chord key element)
            total-keyseq (conj (get @keyseq element) chord)
            key-bindings (get @!key-maps element)
            handler (or (get key-bindings [chord]);check if current chord match
                        (get key-bindings total-keyseq))];check with previous chords
        (prevent-default-if-needed key-bindings chord event)
        (if-not (nil? handler) ;; see if key-seq is complete
          (do
            (events/stop-propagation event)
            (reset-keyseq!)
            (handler))
          (when (modifier-pressed? element)
            (set-keyseq! chord element)))))))



(defn- clear-modifier! [event]
  (let [key (canonicalize-command-key
             (.-keyCode (events/raw-event event)))
        element (events/current-target event)]
    (when (modifier? key)
      (set-modifier! key element false))))

(defn reset-all! [event]
  (reset-keyseq!)
  (reset-mods!))


(defn- add-local-listeners! [element]
  (into []
        (concat 
        (events/listen! element :keydown key-down!)
        (events/listen! element :keyup clear-modifier!)
        (events/listen! element :focus reset-all!))))

(defn- remove-listeners! [listener-keys]
  (doseq [l-key listener-keys]
    (events/unlisten-by-key! l-key)))


(defn- add-key-binding!
  "Add a map of the KEY, COMMAND and ELEMENT to `!key-maps'.
  Associate event listeners for every new element."
  [kseq command element]
  (let [kseq (string/split kseq #",")]
    (doseq [k kseq]
      (when-let [k (canonicalize-keyseq k)]
        (swap! !key-maps #(let [local-binding (get % element)
                                    listeners (or (:listeners local-binding)
                                                  (add-local-listeners! element))
                                    local-with-listn (merge local-binding {:listeners listeners})]
                                (merge % {element (assoc local-with-listn k command)})))))))


(defn- remove-key-binding!
  "Remove KEY and its COMMAND from `!key-maps'. When ELEMENT
  doesn't have any associated key-bindings, remove the listeners"
  [kseq element]
  (let [kseq (string/split kseq #",")]
    (doseq [k kseq]
      (when-let [k (canonicalize-keyseq k)]
        (swap! !key-maps #(let [local-binding (get % element)
                                    new-binding (dissoc local-binding k)
                                    empty-? (empty? (dissoc new-binding :listeners))]
                                (when empty-? (remove-listeners! (:listeners local-binding)))
                                (merge % {element
                                          (if empty-?
                                            (dissoc new-binding :listeners)
                                            new-binding)})))))))




(defn local-set-key [key command element]
  "Give KEY a local binding as COMMAND. Local is defined by the given
dom ELEMENT."
  (add-key-binding! key command element))

(defn local-unset-key [key element]
  "Remove local binding of KEY."
  (remove-key-binding! key element))


;; Yeah, global is basically the same thing as local, we just hardcode
;; the root element in the function.
(defn global-set-key [key command]
  "Give KEY a global binding as COMMAND."
  (add-key-binding! key command events/root-element))

(defn global-unset-key [key]
  "Remove global binding of KEY."
  (remove-key-binding! key events/root-element))

;; Really necessary, otherwise you NEED to complete a valid
;; keysequence in order to clear the keysequence history.
(global-set-key "C-g" (fn [] ((reset-all! _)
                              (domina/log "C-g: Cancel!"))))