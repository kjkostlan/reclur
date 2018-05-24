; Turns exceptions into clojure objects for easy manipulation.

(ns javac.exception)

(defn clj-stack-elem [^java.lang.StackTraceElement sti]
  {:ClassName (.getClassName sti)
   :FileName (.getFileName sti)
   :LineNumber (.getLineNumber sti)
   :MethodName (.getMethodName sti)})

(defn clje [^Exception e]
  "Turns a java exception into a cloujure datastructure."
  (let [^"[Ljava.lang.StackTraceElement;" st (.getStackTrace e)]
    {:Message (.getMessage e)
     :StackTrace (mapv clj-stack-elem (into [] st))}))