; Larger file changes. Maybe the name "iteration" is a bit misleading.

(ns app.iteration
  (:require [javac.file :as jfile] [javac.warnbox :as warnbox]
    [clojure.set :as set] globals
    [app.multicomp :as multicomp]
    [app.fbrowser :as fbrowser] [app.orepl :as orepl] [app.siconsole :as siconsole] [app.varbox :as varbox]
    [coder.logger :as logger]))

;;;;;;;;;;;;;; Keeping files up to date ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn _update-ns1! [msg fname]
  "Returns a message, starting with msg, that should go to the console."
  (let [report (orepl/reload-file! fname)] ; reload on ourselves.
    (if (:error report)
      (str "Saved:" fname ", Compile error:\n" (:error report)) ; no err in deleted files.
      (str msg fname " Namespaces updated."))))

(defn _save-or-delete1!! [fname txt & is-folder?] ; nil txt = delete file.
  (let [del? (nil? txt) clj? (jfile/clj? fname)]
    (if del? (if (jfile/exists? fname) (jfile/delete!! fname)) (jfile/save!! fname txt (first is-folder?)))
    (if clj?
      (_update-ns1! (if del? "Deleted: " "Saved: ") fname)
      (str (if del? "Deleted: " "Saved: ") fname))))

(defn _save-core!! [s usfiles open2text new-files changed-files deleted-files0 missing-files renamed-map copied-map]
  "Appliess the save, with warning dialogues. Returns the modified s."
  ;(println "crtl+s results: new: " new-files "changed:" changed-files "deleted if click yes: " deleted-files0 "missing: " missing-files "renamed: " renamed-map "copied: " copied-map)
  (let [deleted-files (if (and (> (count deleted-files0) 0) (warnbox/yes-no? (str "Delete: " deleted-files0) false)) deleted-files0 #{})
        ; undeleted files refill the tree:
        s (reduce #(multicomp/add-file %1 %2 (jfile/folder? %2))
            s (set/difference deleted-files0 deleted-files))

        ;_ (throw (Exception. "Save disabled for safety reasons.")) ; DEBUG safety that can be enabled in rare app testing cases.
        _ (mapv #(do (jfile/rename!! %1 %2)) (keys renamed-map) (vals renamed-map))
        rename-msgs1 (mapv #(if (jfile/clj? %) (_update-ns1! "Renamed from: " %) (str "Renamed from:" %)) (keys renamed-map))
        rename-msgs2 (mapv #(if (jfile/clj? %) (_update-ns1! "Renamed to: " %) (str "Renamed from:" %)) (vals renamed-map))
        _ (mapv (fn [fname]
                  (let [text (jfile/open fname)]
                    (mapv #(jfile/save!! % text) (get copied-map fname))))
            (keys copied-map)) ; No need to namespace-update copied files, as they are always invalid and only when the user changes them.
        copy-msgs [(if (= (count copied-map) 0) "" (str "Copied: " copied-map " No need to update the namespace just yet\n"))]

        files-saved (set/union new-files changed-files)
        files2folder?s (multicomp/get-fname2is-folder s)
        folder?s-saved (mapv #(get files2folder?s %) files-saved)

        new-save-msgs (mapv #(_save-or-delete1!! %1 (if-let [x (get open2text %1)] x "") %2) files-saved folder?s-saved)
        del-msgs (mapv #(_save-or-delete1!! % nil) deleted-files)

        ; The fbrowser was already updated. Thus only missing files or files the user decided not to delete:
        ; All these files are in local space.
        new-fileset (-> usfiles
                        (set/difference missing-files)
                        (set/union (set/difference deleted-files0 deleted-files)))
        s1 (multicomp/set-filetree s (multicomp/wrap-tree new-fileset) true)

        all-msgs (apply str (interpose "\n" (concat rename-msgs1 rename-msgs2 copy-msgs new-save-msgs del-msgs)))
        msg (apply str (if (> (count all-msgs) 0) all-msgs "No user changes made => no disk changes."))]
    (siconsole/log s1 msg)))

(defn get-disk [folder only-clj?] ; set/difference, etc is useful for this.
  (let [disk (keys (multicomp/get-fname2is-folder {:components {:tmp (fbrowser/load-from-folder folder) :type :fbrowser}}))]
    (apply hash-set (filterv (if only-clj? jfile/clj? identity) disk))))

(defn file-status [s]
  "What files are new? Which have been renamed, changed, deleted, etc?"
  (let [; disk is in "local folder space" and has ./folder/file.clj format, and is more than just clj files.
        disk (get-disk (globals/get-working-folder) false)

        comps (:components s) codeboxks (filterv #(= (:type (get comps %)) :codebox) (keys comps))
        open (apply hash-set (mapv #(fbrowser/devec-file (:path (get comps %))) codeboxks))
        new2?old (multicomp/new2?old-files s)
        open2text (zipmap (into [] open)
                    (mapv #(multicomp/open-fcache (:components s) %) (into [] open)))
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

(defn save-state-to-disk!! [s]
   "Everything is saved, deletions will be prompted."
  (let [fs (file-status s)
        new2?old (:new2?old fs)
        open2text (:open2text fs)
        new-files (:new-files fs)
        changed-files (:changed-files fs)
        missing-files (:missing-files fs)
        renamed-map (:renamed-map fs)
        copied-map (:copied-map fs)
        deleted-files (:deleted-files fs)]
    (mapv #(if (= (:type %) :varbox) (varbox/save-to-var! %))
      (vals (:components s))) ; Not the disk but same idea.
    (_save-core!! s (apply hash-set (keys new2?old)) open2text new-files changed-files deleted-files missing-files renamed-map copied-map)))