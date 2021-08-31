; GUI demos, so it is more clear how to get the gui changed.

(ns test.demogui
  (:require [globals]
    [app.rtext :as rtext]
    [clojure.set :as set]))

(defn add-box [s new-box] "Use ^:global in the repl."
  (assoc-in s [:components (gensym)] new-box))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simple demos ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-simple-rtext [box txt]
  "Rtext has multible :pieces for the purposes of selective display of data.
   We only use one piece."
  (assoc box :pieces [{:text txt}]))

(defn light-dispatch-reporter []
  "This box reports and prints dispatches it gets, these are light dispatches that dont make as much use of the API."
  (let [box0 (set-simple-rtext (assoc rtext/empty-text :position [50 20] :size [800 600] :z 10)
               "No event happened to this box yet.")

        report1 (fn [evt box]
                  (let [evt-str (apply str (interpose "\n" (mapv #(str %1 " " %2) (keys evt) (vals evt))))]
                    (set-simple-rtext box (str "Type of event " (:type evt) "\nEvent: \n" evt-str))))
        box1 (assoc box0 :dispatch report1 :render rtext/render)]
    box1))

(defn hot-dispatch-reporter []
  "Mouse moves and animations need special keys in the dispatch. This saves CPU when they are not in use."
  (let [box0 (light-dispatch-reporter)
        report1 (fn [ty evt box]
                  (let [evt-str (apply str (interpose "\n" (mapv #(str %1 " " %2) (keys evt) (vals evt))))]
                    (set-simple-rtext box (str "Type of event " ty "\nEvent: \n" evt-str))))
        dispatch {:mouseMoved (fn [evt box] (report1 :mouseMoved evt box))
                  :everyFrame (fn [evt box] (report1 :everyFrame evt box))}]
    (assoc box0 :dispatch dispatch :render rtext/render)))

(defn heavy-dispatch-reporter []
  "Heavy dispatches allow the component to modify the app state when it recieves events.
   Args: evt state0 state name-of-us.
     State0 is before running light dispatches, state is after.
   Output: The modified state."
  (let [box0 (set-simple-rtext (assoc rtext/empty-text :position [50 20] :size [800 600] :z 10)
               "Click here to move all other boxes to the right.")
        move-fn (fn [evt state0 state our-name]
                  (if (= (:type evt) :mousePressed)
                    (let [boxes (:components state)
                          kys (set/difference (set (keys boxes)) #{our-name})
                          +x (fn [box] (update-in box [:position 0] #(+ % 16)))
                          boxes1 (reduce #(update %1 %2 +x) boxes kys)]
                      (assoc state :components boxes1)) state))]
    (assoc box0 :dispatch-heavy move-fn :render rtext/render)))
