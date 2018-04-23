(ns clooj.gl.demo 
 (:import 
 (java.awt Frame BorderLayout) 
  (javax.swing JFrame) 
  (java.awt.event WindowAdapter WindowEvent) 
  (javax.media.opengl GL GLAutoDrawable GLDrawableFactory 
  GLCanvas GLEventListener GLCapabilities) 
  (com.sun.opengl.util Animator)
  
  ))
  
   
 ;;Add a .dispose to this frame's windowClosing event 
(defn window-closer [frame] 
 (proxy [WindowAdapter] [] 
  (windowClosing [event] 
  (.start (new Thread 
  (fn [] 
  (.dispose frame))))))) 
 (defn gl-listener [] 
 (proxy [GLEventListener] [] 
  (init [drawable] 
  (doto (.getGL drawable) 
  (.glClearColor 0.0 0.0 0.0 1.0) 
  (.glColor3f 0.0 0.0 0.0))) 
  (display [drawable] 
  (doto (.getGL drawable) 
  (.glClear GL/GL_COLOR_BUFFER_BIT) 
  (.glColor3f 1, 0, 0) 
  (.glRecti 0 0 1 1) 
  (.glColor3f 0.0 0.0 0.0) 
  (.glMatrixMode GL/GL_MODELVIEW) 
  (.glLoadIdentity) 
  )) 
  (displayChanged [drawable mode device]) 
  (reshape [drawable x y w h]) 
  )) 
 ;; Draw the scene 
(defn main [] 
 (let [frame (new Frame "Hey dere Joe") 
  ;gl-canvas (new GLCanvas) 
  ] 
  ;(.addGLEventListener gl-canvas (gl-listener)) 
  (.setSize frame 600 600) 
  ;(.add frame gl-canvas) 
  (.setVisible frame true) 
  (.addWindowListener frame (window-closer frame))))