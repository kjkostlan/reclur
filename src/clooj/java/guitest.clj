(ns clooj.java.guitest
  (:use [clooj.java.gui])
  (:require [clooj.utils :as utils]
   [clooj.java.listener :as listener]
   [clooj.java.updater :as updater]
   [clooj.java.widget :as widget]
   [clooj.coder.grammer :as grammer])
  (:import [jcode JFrameClient]))

; Examples and debugging for our functional GUI.

; Keywords (captalized because it's java after all and to differeniate the parts of the state that are only for clojure use):
; :Type = :JButton, 'JPanel, etc. Don't put the javax.swing package. Keyword recommended but symbol and stirng work.
; :Children = vector of stuff to place in our object (optional, only if there are children. Must be a vector even if only one element).
; :PreferredSize, :Visible, etc = type-dependent sets of keys that are always optional and specify the component's structure.
; :Repaint? = force a repaint (rather than our change-detetector). Use if there are primitive arrays in the java structure.
    ; Repaint? gets swapped to false afterwards.
; :Recursive-event? = listen to events that are triggered by changes induced by other events.
;   Warning: this feature may be given an out-of-date state.

;Debugging: 
;  :debug-java-changes = keep track of what changes were made to the java object, helps see what went wrong.

; TODO error catching:
; Bad behavior when forgetting that it's JFrame -> JPanel -> stuff, not
   ; JFrame -> stuff. => warn the user as this is not the correct way uasually in swing.
   ; (when there is more than one object).
; Better error message when the user passes the root atom or it's reference to setup instead of 
;   (:state @x), or maybe even no error message at all (auto-detect).
; Document :debug-java-changes.
; Document which listeners are recognized along with graphics, etc.
; Confusing error message possible (i.e. deep within swing) when a proxy returns an invalid result.
; misspelled keyword check?
; Bad message when oops a single draw command is passed into the :Graphics option rather than a vector of vector commands.
; Scrollpanes can only have one child, split panes must have exactly 2 (or we can hack a multible split pane).
; If we are listening to self event that doesn't match the :Type, nothing happens (insted of making and error).
; If capitalization is wrong.
; If an only child isn't a one-long vector/map with one key.
; Drawing on a frame with no panel.

; TODO bugs/enhancements:
; JFrame should self-delete by default when we close it, but gui/delete! can get rid of it.
; Feature to exaustivly show every thing so the user knows what to do.
; Some tool to edit the components written in this new paradigm of course.
; Currently we don't have superfulous ancestor et al keys for :self events. Is this the best strategy?
; Adding and removing children can shuffle the order they appear in the parent's components.
; Performace notes:
  ; No obvious chockpoint (apart from reflection). 
  ; But still not great: ~15 times slower in setup and ~3 times slower in run for cases with LARGE buttons.

; Testing:
; (do (require '[clooj.java.gui :as gui]) (require '[clooj.java.guitest :as guitest]))
; (def x (guitest/xyz))
(defn re [gui] 
  "Deletes and regenerates the gui. The old atom is now cleared."
  (let [state (get-state gui)]
    (delete! gui)
    (setup (get-state gui))))

(defn hello []
  "Hellow functional Swing. 
  The :debug-java-changes has us keep track of chanes.
  See these changes with (mapv println (gui/debug-munge-java-changes (:debug-java-changes @x) true)).
  Get the object with (:obj (:java @x))"
  (setup {:Type 'JFrame})) ;:PreferredSize [300 300] ; :PreferredSize is optional.
          ;:Resizable false ; This is the only place in the entire code where Res..able appears, but due to reflection it still works.

(defn textarea []
  "A little nesting with a textareas."
  (setup {:Type 'JFrame :Children 
           [{:Type 'JTextArea}]}))

(defn button []
  "A little nesting with a button."
  (setup {:Type 'JFrame :Children 
           [{:Type 'JPanel :Children 
             [{:Type 'JButton :Text "not clicked" :actionPerformed (fn [s e o] (assoc s :Text "clicked"))}
              {:Type 'JCheckBox :Text "I do nothing"}]}]}
               ))

(defn many-buttons []
  "Performance test comparison with java."
  (setup {:Type 'JFrame :Children 
           [{:Type 'JPanel :Children 
             (let [r {:Type 'JButton :Text "Stress test #"}]
               (mapv #(update r :Text (fn [x] (str x % " out of 2000"))) (range 2000))) }]}))

(defn square []
  "A square window (it keeps itself square when resizing)."
  (setup {:Type 'JFrame :Title "I stay square" :PreferredSize [300 300]
          :componentResized
           (fn [s e o]
             (let [m (* 0.5 (+ (first (:Size e)) (second (:Size e))))] 
               (assoc s :PreferredSize [m m])))} {:debug-java-changes true}))

(defn whack-a-mole []
  "Whack-a-mole: The panel listenes for events on the buttons one level below,
   and then updates the buttons."
  (let [n 5 ; requires larger size for larger n. Setup: 50 ms + 1.5*nms (O.K.). Runtime: good up to 2000 buttons.
        test-hashmap? true ; make the children a hash-map.
        mm #(str "MOLE at " %)
        snark (fn [t] (str t (rand-nth ["Not good" "Slow poke" "Speed up!" "Almost missing?" "Wow your low quality"])))        
        game-over-snark (fn [t] (str t (rand-nth ["Game-over" "Pressed the wrong mole!" "No choice but quit" "Restar- no, give up!" "It's all gone."])))
        nr (if test-hashmap? #(keyword (str %)) identity)
        inr (if test-hashmap? #(Integer/parseInt (subs (str %) 1)) identity)
        act (fn [s e o]
              (let [ix (:ix s) pressix (inr (second (:descendent e)))]
                (if (= ix pressix)
                  (let [nxt (mod (inc ix) (count (:Children s)))
                        s (assoc-in s [:Children (nr 1) :Text] (snark ""))
                        s (if (not= ix 1) (assoc-in s [:Children (nr ix) :Text] (str ix)) s)
                        s (assoc-in s [:Children (nr nxt) :Text] (mm nxt))] (assoc s :ix nxt))
                  ; oops, didn't press where a mole is:
                  (assoc-in (if (> ix -1) (assoc (assoc-in s [:Children (nr ix) :Text] (str ix)) :ix -1) s)
                    [:Children (nr 1) :Text] (game-over-snark "")))))
        btns (mapv #(hash-map :Type 'JButton :Text (str "Not whacked yet: " %) :ix %) (range n)) ;:actionPerformed (fn [s e o] s)
        btns (if test-hashmap? (zipmap (mapv nr (range (count btns))) btns) btns) ; Tests hash-map children.
        btns (assoc-in btns [(nr 0) :Text] (mm 0))
        btns (assoc-in (assoc-in btns [(nr 2) :PreferredSize] [100 50]) [(nr 2) :Text] "the Fat mole")] ; this one is fatter.
  (setup {:Type 'JFrame :PreferredSize [300 300] :Children
           [{:Type 'JPanel :Children btns :ix 0
             :below-actionPerformed act ; Makes startup slow but does not slow down the change detectors: :fat-stuff (into [] (range 2e7))
             }]})))

(defn swapout []
  "Tests changes to the hirerchy."
  (let [s "Click me to swap"
        swm {'JButton {:Type 'JCheckBox :Text s :act :swap} 
             'JCheckBox {:Type 'JButton :Text s :act :swap}}
        add-button {:Type 'JButton :Text "add new button" :act :add}
        rm-button {:Type 'JButton :Text "remove me" :act :remove}
        swappy 
         (fn [panel e o]
           (let [ch-ix (second (:descendent e))
                 act (get-in e [:origin-state :act])]
             (println "action: " act " ch-ix: " ch-ix)
             (cond (= act :swap) ; the swap one.
                 (update-in panel [:Children ch-ix] #(get swm (:Type %)))
                 (= act :add)
                 (update-in panel [:Children] #(conj % rm-button))
                 (= act :remove)
                 (update-in panel [:Children] #(grammer/cdissoc % ch-ix))
                 :else panel)))]
    (setup
      {:Type 'JFrame :Title "Swapout"
        :Children 
        [{:Type 'JPanel :below-actionPerformed swappy ;:below-stateChanged swappy
          :Children [(get swm 'JCheckBox) add-button]}]})))

(defn graphics []
  "Tests with custom graphics that go on the JPanel."
  (let [gfx (fn [] [[:drawOval [10 10 (int (* 50 (Math/random))) (int (* 50 (Math/random)))]]])]
    (setup {:Type 'JFrame :Children 
             [{:Type 'JPanel :Graphics (gfx)
               :below-actionPerformed (fn [s e o] (assoc s :Graphics (gfx)))
               :Children [{:Type 'JButton :Text "redraw random"}]}]}
               {:debug-java-changes true})))

(defn scrollpane []
  "Tests the amazingly finicky scrollpane.
   Adding text makes the pane scroll to the bottom but that
   triggers an event and properly updates the state. We have an option to suppress that."
  (let [suppress-to-end? true ; suppess the scroll-to-end behavior.
        make-lines (fn [n] (apply str (mapv (fn [x] (str x "\n")) (range n))))]
    (setup
      {:Type 'JFrame :Size [600 600] :Children
        [{:Type 'JPanel 
          :below-adjustmentValueChanged 
          (fn [s e o]
            ;TODO: why the !@#$ do we need 3 HP?
            (if (and suppress-to-end? (:HP s) (> (:HP s) 0))
             (assoc (update-in s [:Children 2] #(assoc % :View (:text-change-view s))) 
              :HP (dec (:HP s))) s))
          :below-actionPerformed 
            (fn [s e o] (let [act (get-in e [:origin-state :act])]
                        (cond (= act :mod)
                              (assoc (update-in s [:Children 2] 
                                        #(assoc-in % [:Children 0 :Text] (make-lines 500)))
                              :text-change-view (get-in s [:Children 2 :View]) :HP 3) ; store the view so that we can reuse it.
                              (= act :scroll)
                              (update-in s [:Children 2] #(assoc % :View [90 90]))
                              :else s)))
          :Children
          [{:Type 'JButton :Text "Make long text" :act :mod}
           {:Type 'JButton :Text "Scroll-adjust" :act :scroll}
            {:Type 'JScrollPane :View [0 0] :PreferredSize [400 400] 
            :Children 
            [{:Type 'JTextArea :Text (make-lines 50)}]}]}]})))

(defn splitpane []
  "Tests Java's split panes."
  (setup
    {:Type 'JFrame :Size [300 300] :Children
      [{:Type 'JPanel :Children 
        [{:Type 'JSplitPane ; Fixed size buttons, can't be adjusted.
          :Children 
          [{:Type 'JButton :Text "b 1"}
           {:Type 'JButton :Text "b 2"}]}
         {:Type 'JSplitPane ; With textareas, allowing adjustement.
          :Children 
          [{:Type 'JScrollPane :Children [{:Type 'JTextArea :Text "here is some txt 1"}]}
           {:Type 'JScrollPane :Children [{:Type 'JTextArea :Text "here is some txt 2"}]}]}]}]}))

(defn menu []
  "Tests menus."
  (setup
    {:Type 'JFrame
    :Title "See menu items"
    :below-actionPerformed (fn [s e o] (println e) (assoc s :Title (str "Menu choosen: " (:ActionCommand e))))
    :Children
    [{:Type 'JMenuBar
      :Children
      [{:Type 'JMenu :Text "Letters"
        :Children [{:Type 'JMenuItem :Text "a"} {:Type 'JMenuItem :Text "b"} {:Type 'JMenuItem :Text "c"}]}
      {:Type 'JMenu :Text "Numbers"
        :Children [{:Type 'JMenuItem :Text "1"} {:Type 'JMenuItem :Text "2"} {:Type 'JMenuItem :Text "3"}]}]}]}))

(defn tree []
  "Tests trees. Copying is still not working 100%, but at least they are usable."
  (setup 
    {:Type 'JFrame :Title "trees are a BIG challange"
      :below-valueChanged (fn [s e o] 
                             (let [p (first (:jtree-descendents-selected e))
                                   t (get-in s (concat [:Children 0 :Children 0] p [:Text]))]
                               (assoc s :Title (str "selected: " p " -> " t))))
      :Children
      [{:Type 'JScrollPane
        :Children
        [{:Type 'JTree
          :Text "Outer level"
          :Children
          [{:Type 'JTree :Text "qwerty"
            :Children
            [{:Type 'JTree :Text "I am at the top"}
             {:Type 'JTree :Text "I am also at the top"}]}
           {:Type 'JTree :Text "asdfgh"
            :Children
            [{:Type 'JTree :Text "I am in the middle"}
             {:Type 'JTree :Text "I am also in the middle"}]}
           {:Type 'JTree :Text "zxcvbn"}]}]}]}))

(defn animation []
  "Uses the every-frame listener."
  (let [gfx (fn [angle] [[:drawLine [100 100 (int (+ 100 (* 100 (Math/cos angle)))) (int (+ 100 (* 100 (Math/sin angle))))]]])]
    (setup {:Type 'JFrame :Title "it keeps moving."
            ;:Keep-if-headless? true
            ;:Prevent-user-close? true
            :Children 
            [{:Type 'JPanel 
             :Every-frame (fn [s e o] (println "animation: " (:angle s)) 
                            (assoc (update s :angle #(+ % (/ 10 360.0))) 
                                         :Graphics (gfx (:angle s))))
             :angle 0.0}]})))

;(defn atoms []
;  "Tests atoms for bieng fast."
;)