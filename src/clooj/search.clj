(ns clooj.search
  (:import (java.awt Color)
           (java.awt.event ActionListener)
           (java.util.regex Pattern Matcher)
           (javax.swing BorderFactory JCheckBox JTextField))
  (:require [clooj.highlighting :as highlighting]
            [clooj.utils :as utils]
            [clooj.app.state_old :as app_old]
            [clooj.java.file :as jfile]
            [clooj.coder.grammer :as grammer]
            [clooj.java.textarea_old :as jtext_old]
            [clojure.string :as string]))

 (defn configure-search [match-case use-regex]
   (bit-or Pattern/CANON_EQ
           Pattern/UNICODE_CASE
           (if match-case 0 Pattern/CASE_INSENSITIVE)
           (if use-regex 0 Pattern/LITERAL)))

(defn find-all-in-string
  [s t match-case use-regex]
  (try
    (when (pos? (.length t))
      (let [p (Pattern/compile t (configure-search match-case use-regex))
            m (re-matcher p s)]
        (loop [positions []]
          (if (.find m)
            (recur (conj positions [(.start m) (.end m)] ) )
            positions))))
    (catch Exception _ [])))

(defn highlight-found [text-comp posns]
    (doall
      (map #(highlighting/highlight! text-comp (first %) (second %) Color/YELLOW)
        posns)))

(defn next-item [cur-pos posns]
  (or (first (drop-while #(> cur-pos (first %)) posns)) (first posns)))

(defn prev-item [cur-pos posns]
  (or (first (drop-while #(< cur-pos (first %)) (reverse posns))) (last posns)))

(defn update-find-highlight! [atomtext back]
  "atomtext must have search enabled first using add-search!"
  (let [boxedtextarea @atomtext]
    (let [dta (:textarea boxedtextarea)
          match-case (.isSelected (:match-case boxedtextarea))
          use-regex (.isSelected (:regex boxedtextarea))
          sta (:search-box boxedtextarea)
          posns (find-all-in-string (utils/get-text-str dta)
                                    (utils/get-text-str sta)
                                    match-case
                                    use-regex)
          hilights (:highlights boxedtextarea)
          current-pos (:current-pos boxedtextarea)]
      (highlighting/remove-highlights! dta hilights)
      (if (pos? (count posns))
        (let [selected-pos
               (if back (prev-item (dec current-pos) posns)
                        (next-item current-pos posns))
              posns (remove #(= selected-pos %) posns)
              pos-start (first selected-pos)
              pos-end (second selected-pos)]
          (.setBackground sta Color/WHITE)
          (doto dta
            (.setSelectionStart pos-end)
            (.setSelectionEnd pos-end))
          (let [boxedtextareaOut 
                 (assoc boxedtextarea :current-pos pos-start :highlights 
                        (conj (highlight-found dta posns)
                               (highlighting/highlight! dta pos-start
                                                       pos-end (.getSelectionColor dta))))]
            (jtext_old/scroll-to-pos! atomtext pos-start)
            (reset! atomtext boxedtextareaOut)))
      (.setBackground sta Color/PINK)))))

(defn stop-find! [atomtext]
  (let [boxedarea @atomtext
        sta (:search-box boxedarea)
        dta (:textarea boxedarea)]
    (highlighting/remove-highlights! dta (:highlights boxedarea))
    (reset! atomtext (assoc boxedarea :search-highlights nil :current-pos 0))))

(defn escape-find! [atomtext]
  (stop-find! atomtext)
  (.requestFocus (:textarea @atomtext)))

(defn highlight-step! [atomtext back]
  (when-not back
    (swap! atomtext #(assoc % :current-pos (inc (:current-pos %)))))
  (update-find-highlight! atomtext back))

(defn look-in-files [keystr case regex]
  "looks in all src files for a string, options for case sensitive and using regex. Returns a map of files and the location in each file."
  (let [files (jfile/get-texty-files)
        contents (map jfile/load-textfile files)
        hits (zipmap files (map #(find-all-in-string % keystr case regex) contents))]
    (apply dissoc hits (filter #(= (count (get hits %)) 0) (keys hits)))))

(defn _c2l [s ixs] ixs) ; converts a string and character array to line # array. TODO!
(defn _hit-fn [file contents ixs atomtext gotofn!!] ; hit(s) from a single file.
  (jtext_old/append-text! atomtext (str file " "))
  (let [maxi 20 n (count ixs); have some limit.
        strlocs (map #(str % " ") (map inc (grammer/carets2line contents (take maxi (map first ixs)))))]; string representations of locaiton.
    (doall (map #(jtext_old/append-link1! gotofn!! atomtext %1 [0 (dec (count %1))] file %2) strlocs (map first ixs)))
    (jtext_old/append-text! atomtext (str (if (> n maxi) (str "...(" n " hits)") "") "\n"))))
  
(defn search-src!! [gotofn!!] 
  "finds the user's search string in src files. For now we use the app's search area.
   gotofn!! takes in a file name and a caret and navs you there."
  (let [atomtext app_old/repl-out-text-area
        sta (:search-box @app_old/src-text-area)
        case (.isSelected (:match-case @app_old/src-text-area))
        regex (.isSelected (:regex @app_old/src-text-area))
        hits (look-in-files (.getText sta) case regex)]
    (jtext_old/append-text! atomtext (str "\nOccurances of \"" (.getText sta) "\":\n"))
    (doall (map #(_hit-fn % (jfile/load-textfile %) (get hits %) atomtext gotofn!!) (keys hits)))
    (if (= (count (keys hits)) 0) (jtext_old/append-text! atomtext "(none found)\n"))))

(defn _setup-search-elements! [atomtext]
;(println (keys @atomtext))
  (let [txts @atomtext
       sta (doto (:search-box txts) (.setBorder (BorderFactory/createLineBorder Color/DARK_GRAY)))]
      (utils/add-text-change-listener! sta (fn [_] (update-find-highlight! atomtext false)))
      (utils/attach-action-keys! sta ["ENTER" (fn [] (highlight-step! atomtext false))]
                              ["shift ENTER" (fn [] (highlight-step! atomtext true))]
                              ["ESCAPE" (fn [] (escape-find! atomtext))])))
(defn _change-constraints! [atomtext]
  "The search changes the constratints we need."
 (let [boxed @atomtext
       sp (:springs boxed)
       box (:search-box boxed)
       regex (:regex boxed)
       case (:match-case boxed)
       scrollpane (:scrollpane boxed)
       panel (:panel boxed)
       ; Constraints are added for the new components.
       newsp (conj sp
                   [:S box 0 :S panel] [:S regex 0 :S panel] [:S case 0 :S panel]
                   [:W box 0 :W panel] [:E regex 0 :E panel] [:E case 0 :W regex]
                   [:E box 0 :W case]
                   [:S scrollpane -2 :N box])]; replaces the old constraint.

    (reset! atomtext (assoc boxed :springs newsp))))

(defn add-search! [atomtext]
  "Adds search capability to a text-area, you must give it an atom for it to work."
  (let [textarea @atomtext]
    (let [panel (:panel textarea)  match-case (JCheckBox. "Match case")
          search-box (JTextField.) regex (JCheckBox. "Regex")]
      (doto panel 
        (.add search-box)
        (.add match-case)
        (.add regex))
      (reset! atomtext (assoc textarea :search-box search-box :match-case match-case :regex regex
         :current-pos 0 :highlights nil :current-str "")); add the search capabilities into the textarea.
      (_setup-search-elements! atomtext)
      (_change-constraints! atomtext))))

(defn find-history-ix [history0 ind dir stub]
  "Finds the index in history that starts with sub, starting at ind and going direction dir. -1 means failure."
  (let [history (into [] history0) n (count history)]
  (loop [ix ind]
    (if (or (< ix 0) (>= ix n)) -1
      (if (.contains (nth history ix) stub) ix
        (recur (+ ix dir)))))))