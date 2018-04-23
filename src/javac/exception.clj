; Turns exceptions into clojure objects for easy manipulation.

(ns javac.exception)

(defn clj-stack-elem [sti]
  {:ClassName (.getClassName sti)
   :FileName (.getFileName sti)
   :LineNumber (.getLineNumber sti)
   :MethodName (.getMethodName sti)})

(defn clje [e]
  "Turns a java exception into a cloujure datastructure."
  (let [st (.getStackTrace e)]
    {:Message (.getMessage e)
     :StackTrace (mapv clj-stack-elem st)}))