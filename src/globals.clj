; Global singleton atoms.
(ns globals)

; Holds the app state and event queue.
; Changes to this may mutate the disk or be expensive so we can't risk them running more than once.
; Thus we use agents, not atoms.
(defonce app-agent (agent {}))

; Stores information about the mouse, time, etc, as well as some JVM information.
; This singleton is mutated by actions in the physical world.
(defonce external-state-atom (atom {}))

; The logging code needs to mutate some atom from deep within any arbitrary code.
; Thus this lives meta to the app state.
(defonce log-atom (atom {}))

; The undo atom also lives meta to the app state:
(defonce undo-atom (atom {}))

(defn get-working-folder [] "Gets the folder we save files in." ".")