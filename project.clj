;; per-os jvm-opts code cribbed from Overtone

(defproject reclur "0.0"
  :description "An IDE for clojure that uses clooj as a springboard but has changed a lot. The client folder contains the code for whatever project."
  :url "https://github.com/kjkostlan/reclur"
  ;:license {:name "Eclipse Public License"
  ;          :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main clooj.core
  :java-source-paths ["src/clooj" "src/client"]
  :jvm-opts ^:replace []
;-XX:-OmitStackTraceInFastThrow
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [clj-inspector "0.0.12"]
                 [slamhound "1.2.0"]
                 [com.cemerick/pomegranate "0.0.11"]
                 [com.fifesoft/rsyntaxtextarea "2.5.0"]
                 [org.clojure/tools.nrepl "0.2.3"]
                 ;[org.clojars.charles-stain/lwjgl "3.0"]
                 ;[jogl2 "0.1.0"]
                 ;[net.java.dev.jogl/jogl "1.1.1a"]
		[incanter "1.5.6"]
		[org.apache.commons/commons-math3 "3.0"]
        [org.craigandera/dynne "0.4.1"]
        [prismatic/hiphip "0.2.1"]]
   :resource-paths ["jogl/gluegen-rt-natives-macosx-universal.jar"
                    "jogl/gluegen-rt.jar"
                    "jogl/jogl-all-natives-macosx-universal.jar"
                    "jogl/jogl-all.jar"]
;:plugins [[lein-nodisassemble "0.1.3"]] ; this slows us down on startup quite a bit.
)
