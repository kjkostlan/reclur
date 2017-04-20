; Physics simulation. A simple mass-spring model.
(ns clooj.app.claytreephysics
  (require
    [clooj.app.claytreewalk :as clwalk]
    [clooj.app.claytreetext :as cltext]))

; Physics-object datastructure:
; :x0, :y0, :x1, :y1, :vx0, :vy0, :vx1, :vy1 = absolute position of center and velocity of edges in m and m/s, 1 pixel = 1 meter.
; :lock-x, :lock-y, :locked? = the lock location.
; :fx0, :fx1, :fy0, :fy1 = force buffers that are zeroed and accumilated each step.
 ; The 0 and 1 are forces on opposite sizes, the avg causes compression and x1-x0 is the +x motion
; :width0, :height0 = equilibrium width and height, compression may make :x0 and :x1 be less then the width. 
  ; Used to calculate the mass.

(def ^{:dynamic true} *phy-params* 
  {:dt-max 0.01 :comp 200 :comp-damp 10 :repel 400.0 :parent-child-spring 120.0 ; most of these are for a unit mass.
   :global-spring 0.0 :repel-damp 20 :parent-child-spring-damp 7.0 :global-damp 0.1
   :global-static-f 50.0
   :parent-child-spring-length 1.0 :lock-spring 4000.0 :lock-damp 100.0 :overhead-mass 0.01
   :filesize-mass? false})

(def placeholder-physics 
  {:x0 10 :y0 10 :x1 80 :y1 80 :vx0 0 :vx1 0 :vy0 0 :vy1 0 :fx0 0 :fx1 0 :fy0 0 :fy1 0 
   :width0 100 :height0 100 :lock-x 0 :lock-y 0 :locked? false})

(defn update-physical-properties [node]
  "Sets the mass node, using the mass function, etc."
  (if (:physics node) node (assoc node :physics placeholder-physics)))

(defn sp-overlap [a0 a1 b0 b1]
  "Returns the signed overlap, which is the respulse force on a if there is no damping.
   For 2D it's the MINIMUM of the overlaps that controls the force.
   0 means there is no overlap."
  (let [over1 (- b1 a0) over2 (- a1 b0)]
    (cond (or (<= over1 0) (<= over2 0)) 0
      (<= over1 over2) over1
      :else (- over2))))

(defn spring-force [dx dvx dy dvy l0 k d]
  "Calcuates the spring force [fx fy] on the first object if dx is calculated first - second.."
  (let [il (/ 1.0 (max 1e-100 (Math/sqrt (+ (* dx dx) (* dy dy))))) ; inverse length.
        vr (* (+ (* dx dvx) (* dy dvy)) il) ;seperation velocity
        f (+ (- (* vr d)) (* k (- l0 (/ il))))] ; pushing force.
    [(* f il dx) (* f il dy)]))

(defn parent-child-l0 [node child-node]
  "Get's the parent-child spring length."
  (let [p0 (:physics node) p1 (:physics child-node)]
    (* (:parent-child-spring-length *phy-params*) 0.5 
      (+ (:width0 p0) (:height0 p0) (:width0 p1) (:height0 p1)))))

(defn center [node]
  "Center of a node."
  (let [phy (:physics node)]
    [(* 0.5 (+ (:x0 phy) (:x1 phy))) (* 0.5 (+ (:y0 phy) (:y1 phy)))]))

(defn parent-child-xy0 [node child-ix]
  "The [x y] coordinate of the attachment point on the parent to the child.
   This is the center for files and folders but we use spacers for nodes that have been pulled off."
  (let [phy (:physics node)]
    (if (or (= (:type node) :text) (= (:type node) :arbor))
      (let [tbox (:tbox node)
            cursor-ix (cltext/cursor-child-ix-to-ix node child-ix)
            world-xy (cltext/cursor-ix-to-world (assoc-in node [:tbox :cursor-ix] cursor-ix))]
        (if world-xy world-xy (center node))) (center node))))

(defn imass [nd]
  "Inverse mass. Mass = area0 for now."
  (/ (* (:width0 (:physics nd)) (:height0 (:physics nd)))))

(defn calc-forces [tree]
  "Forces on visible nodes due to springs, damping, and collision."
  (let [tree (clwalk/vis-walk tree #(assoc % :physics (assoc (:physics %) :fx0 0 :fy0 0 :fx1 0 :fy1 0))) ; zero the forces.
        phyp *phy-params*
        ; Parent-child-springs are applied to the center:
        pk (:parent-child-spring phyp) pd (:parent-child-spring-damp phyp)
        tree (clwalk/vis-walk-d tree 
               (fn [node] 
                 (if (:children-visible? node)
                   (reduce #(let [nd %1 ch (nth (:children nd) %2) ndp (:physics nd) chp (:physics ch)
                                  l0 (parent-child-l0 nd ch)
                                  xy0 (parent-child-xy0 nd %2) xy1 (center ch)
                                  dx (- (first xy1) (first xy0)) ; child - parent midpoints.
                                  dy (- (second xy1) (second xy0))
                                  dvx (* 0.5 (- (+ (:vx0 chp) (:vx1 chp)) (+ (:vx0 ndp) (:vx1 ndp))))
                                  dvy (* 0.5 (- (+ (:vy0 chp) (:vy1 chp)) (+ (:vy0 ndp) (:vy1 ndp))))
                                  im1 (imass nd) im2 (imass ch)
                                  sforce (spring-force dx dvx dy dvy l0 (/ pk (+ im1 im2)) (/ pd (+ im1 im2)))
                                  fx (* 0.5 (first sforce)) fy (* 0.5 (second sforce)) ; on the child.
                                  chp1 (assoc chp :fx0 (+ (:fx0 chp) fx) :fx1 (+ (:fx1 chp) fx) :fy0 (+ (:fy0 chp) fy) :fy1 (+ (:fy1 chp) fy))
                                  ndp1 (assoc ndp :fx0 (- (:fx0 ndp) fx) :fx1 (- (:fx1 ndp) fx) :fy0 (- (:fy0 ndp) fy) :fy1 (- (:fy1 ndp) fy))]
                              (assoc-in (assoc nd :physics ndp1) [:children %2 :physics] chp1)) 
                     node (range (count (:children node)))) node)))
        treeu (clwalk/vis-tree-unwrap tree) ; unwrap into a nice vector form, we include the spring forces.
        nu (count treeu)
        rspring-weight (:repel phyp) rdamp-weight (:repel-damp phyp)
        ; O(n^2) collision, but n shouldn't be all that large (1 per visible node on the screen).
        treeu (loop [fs treeu ix 0]
                 (if (= ix nu) fs
                   (recur (loop [fsi fs jx (inc ix)]
                     (if (= jx nu) fsi
                       (let [boxi (:physics (nth fsi ix)) boxj (:physics (nth fsi jx))
                             over-x (sp-overlap (:x0 boxi) (:x1 boxi) (:x0 boxj) (:x1 boxj))
                             over-y (sp-overlap (:y0 boxi) (:y1 boxi) (:y0 boxj) (:y1 boxj))
                             imi (imass (nth fsi ix)) imj (imass (nth fsi jx))
                             rspring (/ rspring-weight (+ imi imj)) ; reduced mass
                             rdamp (/ rdamp-weight (+ imi imj))
                             ; Add forces to each box, there are several cases:
                             boxij (if (or (= over-x 0) (= over-y 0)) [boxi boxj] ; No overlap, no spring nor damping.
                                     (if (< (Math/abs over-x) (Math/abs over-y)) ; Repel perpendicular the lesser overlap direction.
                                       (let [i-left? (> (+ (:x1 boxj) (:x0 boxj)) (+ (:x1 boxi) (:x0 boxi))) ; Is the i index left?
                                             sp-f (* rspring over-x)] ; force on i.
                                         (if i-left? [(assoc boxi :fx1 (+ (:fx1 boxi) sp-f)) (assoc boxj :fx0 (- (:fx0 boxj) sp-f))]
                                           [(assoc boxi :fx0 (+ (:fx0 boxi) sp-f)) (assoc boxj :fx1 (- (:fx1 boxj) sp-f))]))
                                       (let [i-top? (> (+ (:y1 boxj) (:y0 boxj)) (+ (:y1 boxi) (:y0 boxi))) ; Is the j index top (lower y)?
                                             sp-f (* rspring over-y)] ; force on i.
                                         (if i-top? [(assoc boxi :fy1 (+ (:fy1 boxi) sp-f)) (assoc boxj :fy0 (- (:fy0 boxj) sp-f))]
                                           [(assoc boxi :fy0 (+ (:fy0 boxi) sp-f)) (assoc boxj :fy1 (- (:fy1 boxj) sp-f))]))))
                             ; Add damping forces (which occur due to overlap in either direction):
                             ; damping forces are applied equally to both sides, we don't model shear or rotation.
                             boxi (first boxij) boxj (second boxij)
                             boxij (if (or (= over-x 0) (= over-y 0)) boxij 
                                     (let [i-left? (> (+ (:x1 boxj) (:x0 boxj)) (+ (:x1 boxi) (:x0 boxi)))
                                           i-top? (> (+ (:y1 boxj) (:y0 boxj)) (+ (:y1 boxi) (:y0 boxi)))
                                           d-fx (* 0.5 rdamp (if i-left? (- (:vx0 boxj) (:vx1 boxi)) (- (:vx1 boxj) (:vx0 boxi))))
                                           d-fy (* 0.5 rdamp (if i-top? (- (:vy0 boxj) (:vy1 boxi)) (- (:vy1 boxj) (:vy0 boxi))))]
                                       [(assoc boxi :fx0 (+ (:fx0 boxi) d-fx) :fx1 (+ (:fx1 boxi) d-fx) :fy0 (+ (:fy0 boxi) d-fy) :fy1 (+ (:fy1 boxi) d-fy))
                                        (assoc boxj :fx0 (- (:fx0 boxj) d-fx) :fx1 (- (:fx1 boxj) d-fx) :fy0 (- (:fy0 boxj) d-fy) :fy1 (- (:fy1 boxj) d-fy))]))
                             boxi (first boxij) boxj (second boxij)]
                         (recur (assoc-in (assoc-in fsi [ix :physics] boxi) [jx :physics] boxj) (inc jx)))))
                      (inc ix))))
        ; Compression, locking, and global springs (locking and global are applied to the center):
        compr (:comp phyp) dmpr (:comp-damp phyp) lock-s (:lock-spring phyp) lock-d (:lock-damp phyp)
        g-spring (:global-spring phyp) g-damp (:global-damp phyp)
        treeu (mapv #(let [phy (:physics %) k (/ compr (imass %)) d (/ dmpr (imass %))
                           dx (- (:x1 phy) (:x0 phy)) dy (- (:y1 phy) (:y0 phy))
                           dvx (- (:vx1 phy) (:vx0 phy)) dvy (- (:vy1 phy) (:vy0 phy))
                           fx-c (+ (* (- dx (:width0 phy)) k) (* dvx d)) ; compression force on the first object.
                           fy-c (+ (* (- dy (:height0 phy)) k) (* dvy d))
                           x (* 0.5 (+ (:x0 phy) (:x1 phy))) y (* 0.5 (+ (:y0 phy) (:y1 phy)))
                           vx (* 0.5 (+ (:vx0 phy) (:vx1 phy))) vy (* 0.5 (+ (:vy0 phy) (:vy1 phy)))
                           fxfy-g (spring-force x vx y vy 0.0 (/ g-spring (imass %)) (/ g-damp (imass %))) ; -g and -l apply on both the same sign.
                           ; The locking is a spring but with damping in both directions:
                           fxfy-l (if (:locked? phy) [(/ (+ (* lock-s (- (:lock-x phy) x)) (* lock-d (* -0.5 (+ (:vx0 phy) (:vy0 phy))))) (imass %))
                                                      (/ (+ (* lock-s (- (:lock-y phy) y)) (* lock-d (* -0.5 (+ (:vy0 phy) (:vy0 phy))))) (imass %))]
                                    [0.0 0.0])
                           fx (* 0.5 (+ (first fxfy-g) (first fxfy-l))) fy (* 0.5 (+ (second fxfy-g) (second fxfy-l)))]
                        (assoc % :physics (assoc phy :fx0 (+ (:fx0 phy) fx-c fx) :fy0 (+ (:fy0 phy) fy-c fy) :fx1 (+ (:fx1 phy) (- fx-c) fx) :fy1 (+ (:fy1 phy) (- fy-c) fy)))) treeu)]
    (reduce (fn [t nu0] 
              (let [p (:path nu0) ch (get-in t (conj p :children))
                    nu (assoc nu0 :children ch)]
                (if (> (count p) 0) (assoc-in t p nu) nu)))
       tree treeu))) ; fold back up.

(defn simulation-step [tree0 dt]
  "Runs a single physics step.
   Physics helps keep the objects from overlapping."
  (let [tree (calc-forces tree0)]
    (clwalk/vis-walk tree ; verlet (sympletic eular when we don't care too much about initial conditions).
      #(let [phy (:physics %) im (imass %) sv (* (:global-static-f *phy-params*) dt)
             vx0 (+ (:vx0 phy) (* dt im (:fx0 phy))) vx1 (+ (:vx1 phy) (* dt im (:fx1 phy))) 
             vy0 (+ (:vy0 phy) (* dt im (:fy0 phy))) vy1 (+ (:vy1 phy) (* dt im (:fy1 phy)))
             ; static friction:
             v0 (Math/sqrt (+ (* vx0 vx0) (* vy0 vy0)))
             vx0 (if (<= v0 sv) 0.0 (+ vx0 (- (* sv (/ vx0 v0)))))
             vy0 (if (<= v0 sv) 0.0 (+ vy0 (- (* sv (/ vy0 v0)))))
             v1 (Math/sqrt (+ (* vx1 vx1) (* vy1 vy1)))
             vx1 (if (<= v1 sv) 0.0 (+ vx1 (- (* sv (/ vx1 v1)))))
             vy1 (if (<= v1 sv) 0.0 (+ vy1 (- (* sv (/ vy1 v1)))))]
         (assoc % :physics 
           (assoc phy :vx0 vx0 :vy0 vy0 :vx1 vx1 :vy1 vy1
             :x0 (+ (:x0 phy) (* vx0 dt)) :x1 (+ (:x1 phy) (* vx1 dt))
             :y0 (+ (:y0 phy) (* vy0 dt)) :y1 (+ (:y1 phy) (* vy1 dt))))))))

(defn _run-for [tree sim-time]
  "Runs the tree a given number of timesteps, making the substeps divide evenly into it."
  (let [nstep (Math/ceil (/ sim-time (:dt-max *phy-params*)))
        dt (/ sim-time nstep)]
    (reduce (fn [c _] (simulation-step c dt)) tree (range nstep))))

(defn run-for [tree sim-time] (_run-for tree sim-time)) ; debug.
