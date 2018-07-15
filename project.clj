;; per-os jvm-opts code cribbed from Overtone

(defproject reclur "0.0"
  :description "An IDE for clojure that a long time ago used clooj as a springboard but has changed a lot."
  :url "https://github.com/kjkostlan/reclur"
  ;:license {:name "Eclipse Public License"
  ;          :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main core
  :java-source-paths ["src"]
  :jvm-opts ^:replace ["-XX:-OmitStackTraceInFastThrow"]
  :dependencies [[org.clojure/clojure "1.9.0"] ; granular-dependencies.clj per-file.
                 [clj-inspector "0.0.12"]      ; clojure spec website read rationale.
                 [slamhound "1.2.0"]           ; spec-ulation rich hickey (recent note).
                 [com.cemerick/pomegranate "0.0.11"]
                 [com.fifesoft/rsyntaxtextarea "2.5.0"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [rewrite-clj "0.6.0"]
                 [org.clojure/core.match "0.2.2"]
                 ;[org.clojars.charles-stain/lwjgl "3.0"]
                 ;[jogl2 "0.1.0"]
                 ;[net.java.dev.jogl/jogl "1.1.1a"]
		;[incanter "1.5.6"]
		;[org.apache.commons/commons-math3 "3.0"]
		; Another example of how to use clojars: [astro-algo/astro-algo "0.1.3"]
		[commons-io/commons-io "2.5"] ; for FileUtils.
		[org.bmillare/dj.project "0.3.3"] ; allows dynamic loading of dependencies.
        ;[org.craigandera/dynne "0.4.1"]
        ;[prismatic/hiphip "0.2.1"]
        ]
   ;:resource-paths ["jogl/gluegen-rt-natives-macosx-universal.jar"
   ;                 "jogl/gluegen-rt.jar"
   ;                 "jogl/jogl-all-natives-macosx-universal.jar"
   ;                 "jogl/jogl-all.jar"]
                                        ;:plugins [[lein-nodisassemble "0.1.3"]] ; this slows us down on startup quite a bit.
  :plugins [[cider/cider-nrepl "0.18.0-SNAPSHOT"]]
)
