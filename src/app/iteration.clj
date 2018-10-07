; Springboarding!
; Tools for working on the next iteration of reclur from the current iteration.
; When the program starts the next is set to the current.
; ctrl+s goes to the next, updating the app in real time (except for top-level code).
; When the next stabilizes the changes should be transferred back to the current.

(ns app.iteration
  (:require [javac.file :as jfile] [javac.warnbox :as warnbox]
    [clojure.set :as set] globals [app.chfile :as chfile]
    [app.multicomp :as multicomp] [app.fbrowser :as fbrowser] [app.orepl :as orepl] [app.siconsole :as siconsole]))

(defn assert-notchild []
  (if (globals/are-we-child?)
    (throw (Exception. "Saving fns and sending children out dont work when we are the child iteration, edit files in the parent.")) true))

(defn assert-canchild []
  (if (globals/can-child?) true
    (throw (Exception. "Copying out our folder into the child folder only works if :enable-child? is true in the conf file."))))

;;;;;;;;;;;;;; Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def iofile "./.junkio.txt")

;;;;;;;;;;;;;; Interfacing with the application ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We use a disk file for IO with the child app.

(defn launch-app!! [shell-code]
  (let [^ProcessBuilder pb (ProcessBuilder. (into-array ["bash" "-c" shell-code]))
        ^Process p (.start pb) ; Two lines of java snuck into a non javac part of the code.
        ;^InputStream app-out (.getInputStream p) ; it's backwards.
        ;^OutputStream app-in (.getOutputStream p)
        ;^InputStreamReader app-out-reader (InputStreamReader. app-out)
        ;^OutputStreamWriter app-in-writer (OutputStreamWriter. app-in)
        ;^BufferedWriter app-in-writerb (BufferedWriter. app-in-writer)
        ]
    p))

(defn launch-child-app!! [folder]
  (launch-app!! (str "cd " folder " ; lein run")))

(defn send! [app s]
  "Sends string s to app. Doesn't wait for any kind of response from the app.
   (We don't use app for now we use files, this may change one day)."
  (assert-notchild)
  (println "sending cmd: " s)
  (let [tmp-file (chfile/us2child (str (gensym iofile)))]
    (jfile/save!!! tmp-file (str s))
    (jfile/rename!!! tmp-file (chfile/us2child iofile)))) ; the rename ensures atomic file manipulation. Not sure if this is necessary.
    
(defn get-input []
  "Blocks until the iofile has stuff in it. Use in a loop on another thread that calls event dispatch fns."
  ; TODO: use a listener instead of repeated checks. Don't know how to do this, the Watcher system still seems to require polling.
    ; i.e. calling key.pollEvents() continuously (I think).
    ; However, this only consumes at most 0.3% of a single CPU core when used with a 100ms sleep interval.
  (if (not (globals/are-we-child?)) (throw (Exception. "For now get-input is only used by the child, we have one-way communication.")))
  (let [iofl iofile tmp-file (str (gensym iofl))]
    (loop []
      (let [txt (if (jfile/exists? iofl)
                  (do (jfile/rename!!! iofl tmp-file) (jfile/open tmp-file)) "")]
        (if (> (count txt) 0) txt
          (do (Thread/sleep 100) (recur)))))))

;;;;;;;;;;;;;; Keeping files up to date ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-ns1!! [msg fname child-process]
  "Returns a message, starting with msg, that should go to the console.
   nil child process updates the namespace on the parent (i.e. after the next version is stabilized)."
  (if child-process
    (let [fname-l (chfile/child2us fname)] ; reload on the child.
      (send! child-process (str "(do (require '[app.orepl :as orepl]) (orepl/reload-file!! \"" fname-l "\"))"))
      (str msg fname " Sent ns-update code to the child process."))
    (let [report (orepl/reload-file!! fname)] ; reload on ourselves.
      (if (:error report)
        (str "Saved:" fname ", Compile error:\n" (:error report)) ; no err in deleted files.
        (str msg fname " Namespaces updated.")))))

(defn save-or-delete1!!! [child-process fname txt] ; nil txt = delete file.
  (assert-notchild)
  (let [del? (nil? txt) clj? (jfile/clj? fname)]
    (if del? (if (jfile/exists? fname) (jfile/delete!!! fname)) (jfile/save!!! fname txt))
    (if clj?
      (update-ns1!! (if del? "Deleted: " "Saved: ") fname child-process)
      (str (if del? "Deleted: " "Saved: ") fname))))

(defn _save-core!!! [s usfiles open2text new-files changed-files deleted-files0 missing-files renamed-map copied-map]
  "Applys the save, with warning dialogues. Returns the modified s. All files are in 'local space' meaning 
   relative to the root of the child folder if child? is true.
   We only call this from the parent."
  ; DEBUG: 
  ;(println "crtl+s results: new: " new-files "changed:" changed-files "deleted if click yes: " deleted-files0 "missing: " missing-files "renamed: " renamed-map "copied: " copied-map)
  (assert-notchild)
  (let [deleted-files (if (and (> (count deleted-files0) 0) (warnbox/yes-no? (str "Delete: " deleted-files0) false)) deleted-files0 #{})
        ; undeleted files refill the tree:
        s (reduce #(multicomp/add-file %1 %2 (jfile/folder? %2)) 
            s (set/difference deleted-files0 deleted-files))
        
        child-process (let [cp (:child-process s)] 
                        (if (or cp (not (globals/can-child?))) cp (throw (Exception. "No child process, should be set up by code"))))
        ;_ (throw (Exception. "Save disabled for safety reasons.")) ; DEBUG safety.
        _  (mapv #(do (jfile/rename!!! %1 %2)) (keys renamed-map) (vals renamed-map))
        rename-msgs1 (mapv #(if (jfile/clj? %) (update-ns1!! "Renamed from: " % child-process) (str "Renamed from:" %)) (keys renamed-map))
        rename-msgs2 (mapv #(if (jfile/clj? %) (update-ns1!! "Renamed to: " % child-process) (str "Renamed from:" %)) (vals renamed-map))
        _ (mapv (fn [fname] 
                  (let [text (jfile/open fname)]
                    (mapv #(jfile/save!!! % text) (get copied-map fname)))) 
            (keys copied-map)) ; No need to namespace-update copied files, as they are always invalid and only when the user changes them.
        copy-msgs [(if (= (count copied-map) 0) "" (str "Copied: " copied-map " No need to update the namespace just yet\n"))]
        new-save-msgs (mapv #(save-or-delete1!!! child-process % (if-let [x (get open2text %)] x "")) (set/union new-files changed-files))
        del-msgs (mapv #(save-or-delete1!!! child-process % nil) deleted-files)
        
        ; The fbrowser was already updated. Thus only missing files or files the user decided not to delete:
        ; All these files are in local space.
        new-fileset (-> usfiles
                        (set/difference missing-files)
                        (set/union (set/difference deleted-files0 deleted-files)))
        s1 (multicomp/set-filetree s (multicomp/wrap-tree new-fileset) true)
        
        all-msgs (apply str (interpose "\n" (concat rename-msgs1 rename-msgs2 copy-msgs new-save-msgs del-msgs)))
        msg (apply str "Save called: " (if (> (count all-msgs) 0) all-msgs "No user changes made => no disk changes."))]
    (siconsole/log s1 msg)))

(defn get-disk [folder only-clj?] ; set/difference, etc is useful for this.
  (assert-notchild)
  (let [disk (multicomp/get-filelist {:components {:tmp (fbrowser/load-from-folder folder) :type :fbrowser}} false nil)]
    (apply hash-set (filterv (if only-clj? jfile/clj? identity) disk))))

(defn file-status [s]
  "What files are new? Which have been renamed, changed, deleted, etc?"
  (let [; disk is in "local folder space" and has ./folder/file.clj format, and is more than just clj files.
        disk (get-disk (globals/get-working-folder) false)

        comps (:components s) codeboxks (filterv #(= (:type (get comps %)) :codebox) (keys comps))
        open (apply hash-set (mapv #(first (:path (get comps %))) codeboxks))
        new2?old (multicomp/new2?old-files s)
        open2text (zipmap (into [] open)
                    (mapv #(multicomp/open-cache s %) (into [] open)))
        get-old (fn [new] (if-let [x (get new2?old new)] x new))
        old2new (dissoc (zipmap (vals new2?old) (keys new2?old)) nil false)

        ; Types of files that weren't left as-is (the new filename is used for files that are both renamed and changed or missing):
        renamed-map (let [diffs (filterv #(let [o (get new2?old %)] (and o (not= o %) (not= (get new2?old o) o))) (keys new2?old))] ; the (get new2?old o) is to exclude directly copying the file.
                      (zipmap (mapv #(get new2?old %) diffs) diffs))
        new-files (apply hash-set (filterv #(and (not (get new2?old %)) (not (jfile/exists? %))) (keys new2?old)))
        missing-files (apply hash-set (filterv #(let [o (get new2?old %)] (and o (not (jfile/exists? o)))) (keys new2?old)))
        changed-files (let [change? (fn [new] (not= (jfile/open (get-old new)) (get open2text new)))]
                        (apply hash-set (filterv change? (set/difference open new-files))))
        copied-map (reduce (fn [acc fname] 
                             (let [old (get new2?old fname)]
                               (if (or (not old) (= (get renamed-map old) fname) (get new-files fname) (= old fname)) acc
                                 (update acc old #(if % (conj % fname) [fname]))))) {} (keys new2?old))
        deleted-files (apply hash-set (filterv #(and (not (get old2new %)) (not (contains? new2?old %))) disk))]
    {:new2?old new2?old
     :open2text open2text
     :new-files new-files
     :changed-files changed-files
     :missing-files missing-files
     :deleted-files deleted-files
     :renamed-map renamed-map
     :copied-map copied-map}))

(defn save-state-to-disk!!! [s]
   "Everything is saved, deletions will be prompted. Child = true will save to the child folder. Child = false won't.
    We only call this from the parent."
  (assert-notchild)
  (let [fs (file-status s)
        new2?old (:new2?old fs)
        open2text (:open2text fs)
        new-files (:new-files fs)
        changed-files (:changed-files fs)
        missing-files (:missing-files fs)
        renamed-map (:renamed-map fs)
        copied-map (:copied-map fs)
        deleted-files (:deleted-files fs)]
    (_save-core!!! s (apply hash-set (keys new2?old)) open2text new-files changed-files deleted-files missing-files renamed-map copied-map)))

;;;;;;;;;;;;;;;;;;;; Copying folders us->child and child->us ;;;;;;;;;;;;;;;;;;;

(defn _copy-folder!!! [s to-child? orig dest]
  "These copies don't affect the sync."
  (assert-notchild)
  (let [files-usl (if (not to-child?) (get-disk chfile/us-folder true)) ; true = only clj fils.
        files-chl (if (not to-child?) (get-disk chfile/child-folder true))
        changes (if (not to-child?) (filterv #(not= (jfile/open %) (jfile/open (chfile/us2child %))) (set/intersection files-usl files-chl)))
        
        _ (jfile/copy!!! orig dest true)
        
        ch-noyes-file (str dest "/config.txt") ; change the file to determine wether or not we are a child.
        ^String txt0 (jfile/open ch-noyes-file)
        ^String s0 (if to-child? "we-are-child? false" "we-are-child? true")
        ^String s1 (if to-child? (.replace s0 "false" "true") (.replace s0 "true" "false"))
        txt1 (.replace txt0 s0 s1)]
    (jfile/save!!! ch-noyes-file txt1)
    (if (not to-child?) ; when copying back the latest cool gadgets we need to update the space.
      (let [news (set/difference files-chl files-usl)
            deletes (set/difference files-usl files-chl)]
        (mapv #(update-ns1!! "New toy: " % nil) news)
        (mapv #(update-ns1!! "Old junk: " % nil) deletes)
        (mapv #(update-ns1!! "Tweaks: " % nil) changes)))))

(defn copy-child-to-us!!! [s]
  "This should be used for each modification of the child app once it is behaving stably. 
   It doesn't affect the app state s because the child folder is the one that is most up-to-date,
   However it does require updating files."
  (assert-notchild)
  (println "COPYING TO US")
  (_copy-folder!!! s false chfile/child-folder chfile/us-folder) s)

;;;;;;;;;;;;;;;;;;;; Other ;;;;;;;;;;;;;;;;;;;
  
(defn ensure-childapp-folder-init!!! [s]
  "Makes sure the child app is started. Only call this if we allow having a child in the first place."
  (assert-notchild) (assert-canchild)
  (if (not (jfile/exists? chfile/child-folder)) (_copy-folder!!! s true chfile/us-folder chfile/child-folder))
  (if (:child-process s) s (assoc s :child-process (launch-child-app!! chfile/child-folder))))