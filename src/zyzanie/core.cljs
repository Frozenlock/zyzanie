(ns zyzanie.core
  (:require [clojure.browser.dom :as dom]
            [domina :as domina]
            [domina.events :as events]
            [enfocus.core :as ef]
            [clojure.string :as string])
  (:require-macros [enfocus.macros :as em]))


;;; Inspired by https://github.com/AndreasKostler/dyscord

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


;; ================ Hook functions ================

(def !completed-keysequence-hooks
  "Any function stored here will be called after every completed key
sequence"
  (atom {}))

(defn add-local-hook! [hook-fn element]
  (let [current-local-hook (get @!completed-keysequence-hooks element)]
    (swap! !completed-keysequence-hooks
           #(merge % {element (conj current-local-hook hook-fn)}))))

(defn add-global-hook! [hook-fn]
  (add-local-hook! hook-fn events/root-element))


(defn- call-local-hooks [event element]
  (doseq [func (get @!completed-keysequence-hooks element)]
    (func event)))

(defn call-hooks [event element]
  (call-local-hooks event element) ;local hooks
  (when-not (= element events/root-element) ;global hooks (don't call again if we were already global)
    (call-local-hooks event events/root-element)))

;; There is no way to compare if two functions are equal. Thus, we
;; can't remove a precise function once it's in the hooks list.
;; Perhaps we could return the vector position?

(defn remove-all-hooks! []
  (reset! !completed-keysequence-hooks {}))

(defn remove-local-hooks! [element]
  (swap! !completed-keysequence-hooks #(dissoc % element)))

(defn remove-global-hooks! []
  (remove-local-hooks! events/root-element))


;; ================ Main ================

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


;; Can't use the button numbers directly; they could clash with the
;; keys numbers.
(def mouse-buttons
  {"mouse0" :m0
   "mouseleft" :m0
   "mouse2" :m2
   "mouseright" :m2
   "mouse1" :m1
   "mousemiddle" :m1})

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
      (get mouse-buttons key)
      (.charCodeAt (.toUpperCase key) 0)))
;; keyCode will give us the code the for the uppercase letter


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



(def !keyseq (atom {}))

(defn- set-keyseq!
  "Set the key sequence history by dividing it by element"
  [chord element]
  (swap! !keyseq #(merge % {element (conj (or (get % element) []) chord)})))


                           
(defn- reset-keyseq! []
  (reset! !keyseq {}))

(defn- modifier-pressed? [element]
  (some (fn [[_ v]] (true? v)) (get @mods element)))


(defn- prevent-default-if-needed
  "Check if the current modifier and key chord are required for a
  local key-binding. If it is, prevent the default browser behavior."
  [key-bindings chord event]
  (when (and (> (count chord) 1);at least two keys
             (some #(some (fn [a] (= chord a)) %) (keys key-bindings)))
    (events/prevent-default event)))

(def !last-valid-keyseq (atom []))

(defn- validate-keysequence
  "Check if we match a key sequence. If yes, reset the current key
  sequence add call the associated command."[key event]
  (let [raw-event (events/raw-event event)
        element (events/current-target event)]
    (if (modifier? key)
      (set-modifier! key element true)
      (let [chord (get-chord key element)
            total-keyseq (conj (get @!keyseq element) chord)
            key-bindings (get @!key-maps element)
            found-key-handler (or (find key-bindings [chord]);check if current chord match
                                  (find key-bindings total-keyseq))];check with previous chords
        (prevent-default-if-needed key-bindings chord event)
        (when-not (nil? found-key-handler);; see if key-seq is complete
          (let [handler (val found-key-handler)]
            (events/stop-propagation event)
            (reset! !last-valid-keyseq (first found-key-handler))
            (call-hooks event element)
            (reset-keyseq!)
            (handler event)))
        (when (modifier-pressed? element)
          (set-keyseq! chord element))))))

(defn- key-down! [event]
  "At each keydown, check if we match a key sequence. If yes, reset
the current key sequence and call the associated command."
  (let [raw-event (events/raw-event event)
        key (canonicalize-command-key (.-keyCode raw-event))]
    (validate-keysequence key event)))



(defn- mouse-down! [event]
  "Similar to key-down!, but this time we get which mouse button was
pressed."
  (let [raw-event (events/raw-event event)
        key (keyword (str "m" (.-button raw-event)))]
    (validate-keysequence key event)))

(defn- remove-context-menu-if-needed
  "If the mouse right button is needed for a local keybinding, remove
  the context menu" [event]
  (let [element (events/current-target event)
        local-keymap (get @!key-maps element)
        chord (get-chord :m2 element)]
    (when (some #(some (fn [a] (= chord a)) %) (keys local-keymap))
      (events/prevent-default event))))


(defn- clear-modifier! [event]
  (let [key (canonicalize-command-key
             (.-keyCode (events/raw-event event)))
        element (events/current-target event)]
    (when (modifier? key)
      (set-modifier! key element false))))

(defn reset-all! [event]
  (reset-keyseq!)
  (reset-mods!))


(defn auto-focus-hover
  "Make the element focusable and auto focus when mouse over. This
  enable the keyboard to fire local events." [element]
  (let [raw-el (first (domina/nodes element))]
    (when-not (domina/attr raw-el :zyzanie)
      (when-not (domina/attr raw-el :tabindex)
        (domina/set-attr! element :tabindex "-1"));focusable using JS
      (ef/at element (em/listen :mouseenter #(.focus raw-el)))
      (ef/at element (em/listen :mouseleave #(.blur raw-el)))
      (domina/set-attr! element :zyzanie "true"))))

(defn remove-auto-focus-hover
  "Remove the :mouseenter and :mouseleave event. Unfortunately Enfocus
  doesn't provide event keys like Domina, so we can't be precise."[element]
  (let [raw-element (first (domina/nodes element))]
    (when (domina/attr raw-element :zyzanie)
      (ef/at element (em/remove-listener :mouseenter))
      (when (= (domina/attr raw-element :tabindex) "0")
        (domina/remove-attr! raw-element :tabindex))
      (domina/remove-attr! raw-element :zyzanie))))


(defn- add-local-listeners! [element]
  (into []
        (concat 
        (events/listen! element :keydown key-down!)
        (events/listen! element :keyup clear-modifier!)
        (events/listen! element :mousedown mouse-down!)
        (events/listen! element :contextmenu remove-context-menu-if-needed)
        (events/listen! element :focus reset-all!))))
;; We can't use the :click event because some browsers only use it for
;; the left mouse button.

(defn- remove-listeners! [listener-keys element]
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
                                (when empty-? (remove-listeners! (:listeners local-binding) element))
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
(global-set-key "C-g" (fn [] ((reset-all! nil)
                              (domina/log "C-g: Cancel!"))))