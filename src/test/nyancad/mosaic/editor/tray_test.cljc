; SPDX-FileCopyrightText: 2026 Pepijn de Vos
;
; SPDX-License-Identifier: MPL-2.0

(ns nyancad.mosaic.editor.tray-test
  "Tests for the domain-switched device tray and the picker-first model
   picker (specs/mosaic_device_tray.md, specs/mosaic_model_picker.md).

   Covers `tray-groups` well-formedness (registered types, valid domains,
   unique ids), `visible-tray-groups` per-domain inventories (subcircuits in
   both modes), the `ideal-placement?` exclusion set, `picker-type-match?`
   (ckt catch-all, amp as ckt variant), `arm-device!` (ideal stages directly,
   picker-first opens the picker, confirm arms with the bare model id), and
   `picker-key-down!` (arrow wrapping, Enter confirm, Escape cancel, empty
   result set).

   The split-tray flyout interaction, domain rocker rendering, and picker
   row markup are visual/DOM behavior covered by manual testing — see the
   manual test plan in PR.md."
  (:require [cljs.test :refer [deftest is testing]]
            [reagent.core :as r]
            [nyancad.mosaic.test-dommatrix]
            [nyancad.mosaic.common :as cm]
            [nyancad.mosaic.editor :as e]
            [nyancad.mosaic.editor.platform-test :as pform]))

(defn- ui-reset []
  (swap! e/ui assoc
         ::e/staging nil
         ::e/tool ::e/cursor
         ::e/mouse [0 0]
         ::e/domain ::e/photonic
         ::e/selected #{}
         ::e/dragging nil
         ::e/paste-staging nil))

(deftest tray-groups-well-formed-test
  (let [valid-types (into cm/device-types cm/schematic-only-types)
        ids (map :id e/tray-groups)]
    (is (apply distinct? ids) "group ids are unique")
    (doseq [{:keys [id domain variants]} e/tray-groups]
      (is (keyword? id))
      (is (contains? #{::e/photonic ::e/electronic ::e/both} domain)
          (str id " has a valid domain"))
      (is (seq variants) (str id " has variants"))
      (doseq [{:keys [title type icon]} variants]
        (is (and (string? title) (seq title)) (str type " has a title"))
        (is (contains? valid-types type) (str "unregistered type: " type))
        (is (vector? icon) (str type " has hiccup icon"))))))

(deftest visible-tray-groups-test
  (testing "photonic mode: generic ports, photonic groups, subcircuits"
    (is (= [:photonic-ports :waveguides :couplers :resonators :optical-sources :interface :subcircuits]
           (map :id (e/visible-tray-groups e/tray-groups ::e/photonic)))))
  (testing "electronic mode: the restored upstream toolbar plus subcircuits"
    (is (= [:ports :resistor :inductor :capacitor :diode :sources :transistors :subcircuits]
           (map :id (e/visible-tray-groups e/tray-groups ::e/electronic))))))

(deftest tray-slot-deficit-test
  (testing "items + filler is the same in every domain, so the bar's right edge
            — and the rocker on it — never moves when the domain changes"
    (is (apply = (for [{:keys [id]} e/tray-domains]
                   (+ (count (e/visible-tray-groups e/tray-groups id))
                      (e/tray-slot-deficit e/tray-groups id))))))
  (testing "the widest domain needs no filler, and no domain gets negative filler"
    (let [deficits (map #(e/tray-slot-deficit e/tray-groups (:id %)) e/tray-domains)]
      (is (= 0 (apply min deficits)))
      (is (every? nat-int? deficits))))
  (testing "photonic is one group short of electronic today"
    (is (= 1 (e/tray-slot-deficit e/tray-groups ::e/photonic)))
    (is (= 0 (e/tray-slot-deficit e/tray-groups ::e/electronic)))))

(deftest representative-first-variant-test
  (testing "the first variant of each group is the most representative/common
            component — it is the group's default icon"
    (let [first-of (fn [id] (->> e/tray-groups
                                 (filter #(= id (:id %)))
                                 first
                                 :variants
                                 first
                                 ((juxt :type :variant))))]
      (is (= ["port" nil] (first-of :photonic-ports)))
      (is (= ["straight" nil] (first-of :waveguides)))
      (is (= ["coupler" nil] (first-of :couplers)))
      (is (= ["ring-single" nil] (first-of :resonators)))
      (is (= ["port" "emitter"] (first-of :optical-sources)))
      (is (= ["grating-coupler" nil] (first-of :interface)))
      (is (= ["ckt" nil] (first-of :subcircuits)))
      (is (= ["port" nil] (first-of :ports)))
      (is (= ["vsource" nil] (first-of :sources)))
      (is (= ["nmos" nil] (first-of :transistors))))))

(deftest photonic-coverage-test
  (testing "every photonic-category builtin in the models map is in the tray"
    (let [photonic-types (into #{}
                               (keep (fn [[t entry]]
                                       (when (= "photonic" (e/resolve-field entry ::e/category {:type t}))
                                         t)))
                               e/models)
          tray-photonic (into #{}
                              (comp (mapcat :variants) (map :type))
                              (e/visible-tray-groups e/tray-groups ::e/photonic))]
      (is (seq photonic-types) "models map has photonic builtins")
      (is (every? tray-photonic photonic-types)
          (str "missing from tray: " (remove tray-photonic photonic-types)))
      ;; source-category grating-coupler and the photonic-active devices are
      ;; not ::category \"photonic\" but must still be reachable
      (doseq [t ["grating-coupler" "led" "photodiode" "modulator"]]
        (is (contains? tray-photonic t) (str t " is reachable in photonic mode")))))
  (testing "the generic schematic tools are reachable in photonic mode too"
    (let [tray-photonic (into #{}
                              (comp (mapcat :variants) (map :type))
                              (e/visible-tray-groups e/tray-groups ::e/photonic))]
      (doseq [t ["port" "text"]]
        (is (contains? tray-photonic t) (str t " is reachable in photonic mode"))))))

(deftest electronic-coverage-test
  (testing "the upstream electronic toolbar is restored"
    (let [tray-electronic (into #{}
                                (comp (mapcat :variants) (map :type))
                                (e/visible-tray-groups e/tray-groups ::e/electronic))]
      (doseq [t ["port" "text" "resistor" "inductor" "capacitor" "diode"
                 "vsource" "isource" "nmos" "pmos" "npn" "pnp" "ckt" "amp"]]
        (is (contains? tray-electronic t) (str t " is reachable in electronic mode"))))))

(deftest ideal-placement-test
  (testing "exclusion set stages ideal directly (no picker)"
    (doseq [t ["resistor" "capacitor" "inductor" "vsource" "isource" "text"]]
      (is (true? (e/ideal-placement? t nil)) t))
    (doseq [v [nil "ground" "supply" "text" "emitter" "detector"]]
      (is (true? (e/ideal-placement? "port" v)) (str "port/" v))))
  (testing "everything else is picker-first"
    (doseq [t ["diode" "nmos" "pmos" "npn" "pnp" "ckt" "amp"
               "straight" "bend" "sbend" "taper" "transition" "terminator"
               "crossing" "ring-single" "ring-double" "spiral" "splitter-1x2"
               "coupler" "mmi-1x2" "mmi-2x2" "mzi-1x2" "mzi-2x2"
               "grating-coupler" "led" "photodiode" "modulator"]]
      (is (false? (e/ideal-placement? t nil)) t))))

(deftest picker-type-match-test
  (is (true? (e/picker-type-match? "bend" "bend")))
  (is (false? (e/picker-type-match? "bend" "straight")))
  (testing "ckt matches the catch-all type, including untyped entries"
    (is (true? (e/picker-type-match? "ckt" "ckt")))
    (is (true? (e/picker-type-match? "ckt" nil)))
    (is (false? (e/picker-type-match? "ckt" "bend"))))
  (testing "amp is a ckt variant: amp and ckt entries together"
    (is (true? (e/picker-type-match? "amp" "amp")))
    (is (true? (e/picker-type-match? "amp" "ckt")))
    (is (true? (e/picker-type-match? "amp" nil)))
    (is (false? (e/picker-type-match? "amp" "bend")))))

(deftest picker-narrowing-test
  (let [modeldb {"models:pk.bend_euler" {:name "bend_euler" :type "bend" :tags ["waveguides" "bend"]}
                 "models:pk.bend_circular" {:name "bend_circular" :type "bend" :tags ["waveguides" "bend"]}
                 "models:pk.straight" {:name "straight" :type "straight" :tags ["waveguides" "straight"]}
                 "models:pk.cell_x" {:name "cell_x" :type "ckt" :tags ["cells"]}
                 "models:pk.amp_y" {:name "amp_y" :type "amp" :tags ["cells"]}}
        of-type (fn [t] (into {} (filter (fn [[_ m]] (e/picker-type-match? t (:type m))) modeldb)))]
    (is (= #{"models:pk.bend_euler" "models:pk.bend_circular"}
           (set (keys (of-type "bend")))))
    (is (= #{"models:pk.cell_x" "models:pk.amp_y"}
           (set (keys (of-type "amp"))))
        "amp's picker includes ckt entries")
    (is (= #{"models:pk.cell_x"}
           (set (keys (of-type "ckt")))))
    (testing "search query narrows within the type filter"
      (is (= #{"models:pk.bend_euler"}
             (set (keys (cm/filter-models (of-type "bend") [] "euler"))))))
    (testing "tags narrow within the type filter"
      (is (= #{"models:pk.bend_euler" "models:pk.bend_circular"}
             (set (keys (cm/filter-models (of-type "bend") ["waveguides"] ""))))))))

(deftest arm-device-test
  (ui-reset)
  (reset! cm/modal-content nil)
  (testing "ideal types stage immediately without opening the picker"
    (e/arm-device! "resistor" nil [3 4])
    (is (= "resistor" (:type @e/staging)))
    (is (nil? (:model @e/staging)))
    (is (nil? @cm/modal-content))
    (is (= ::e/device @e/tool)))
  (testing "port variants dispatch to their dedicated placement fns"
    (ui-reset)
    ;; emitter: no baked transform, so it runs under the :test build where
    ;; cm/I is an inert nil stub. The transform-baking variants (ground,
    ;; supply, label, detector) are covered by manual testing — see PR.md.
    (e/arm-device! "port" "emitter" [1 1])
    (is (= "port" (:type @e/staging)))
    (is (= "emitter" (:variant @e/staging)))
    (is (nil? @cm/modal-content)))
  (testing "picker-first types open the picker instead of staging"
    (ui-reset)
    (e/arm-device! "bend" nil [3 4])
    (is (nil? @e/staging))
    (is (some? @cm/modal-content)))
  (testing "confirming arms the staged device with the bare model id"
    (swap! e/ui assoc ::e/mouse [7 8])
    (let [[_ device-type on-select] @cm/modal-content]
      (is (= "bend" device-type))
      (on-select "models:cspdk.si220.cells.bend_euler")
      (is (= "bend" (:type @e/staging)))
      (is (= "cspdk.si220.cells.bend_euler" (:model @e/staging))
          "model stored as bare id")
      (is (= [6 7] [(:x @e/staging) (:y @e/staging)])
          "armed device centers on the cursor via device-dims ([1 1] fallback)")
      (is (= ::e/device @e/tool))))
  (reset! cm/modal-content nil))

;; ---------------------------------------------------------------------------
;; staging-origin
;; ---------------------------------------------------------------------------
;;
;; Contract: the unrounded top-left [:x :y] that puts a device's visual center
;; under a grid point. `draw-background` insets the tetris rect one tile, so a
;; [w h] device's center sits at [:x + 1 + w/2, :y + 1 + h/2] — invert that.
;; Devices with no tetris rect (nil dims: plain ports, text) render one tile at
;; the origin, so their center is [:x + 0.5, :y + 0.5].
;;
;; `add-device` and `drag-staged-device` share this so the ghost cannot jump
;; between arming and the first pointer move; only `add-device` rounds.

(deftest staging-origin-square-device-test
  (testing "a 1x1 tetris device (resistor) centers at cursor - 1.5 on both axes"
    (is (= [8.5 8.5] (e/staging-origin {:type "resistor"} [10 10])))))

(deftest staging-origin-tall-device-test
  (reset! pform/modeldb
          {"models:pk.tall" {:name "tall" :type "mmi-1x2"
                             :ports [{:name "o1" :side "left" :type "photonic"}
                                     {:name "o2" :side "left" :type "photonic"}
                                     {:name "o3" :side "left" :type "photonic"}]}})
  (testing "a 1x3 device offsets by half its height, not its full height"
    ;; The pre-consolidation drag path subtracted the full w/h and padded 0.5,
    ;; which put this at [8.5 6.5] — a whole tile above the cursor.
    (is (= [8.5 7.5] (e/staging-origin {:type "mmi-1x2" :model "pk.tall"} [10 10]))))
  (reset! pform/modeldb {}))

(deftest staging-origin-no-tetris-rect-test
  (testing "text and plain ports put their single tile under the cursor"
    (is (= [9.5 9.5] (e/staging-origin {:type "text"} [10 10])))
    (is (= [9.5 9.5] (e/staging-origin {:type "port"} [10 10])))))

(deftest add-device-centering-test
  (ui-reset)
  (reset! pform/modeldb
          {"models:pk.bend" {:name "bend" :type "bend"
                             :ports [{:name "o1" :side "left" :type "photonic"}
                                     {:name "o2" :side "top" :type "photonic"}]}
           "models:pk.tall" {:name "tall" :type "mmi-1x2"
                             :ports [{:name "o1" :side "left" :type "photonic"}
                                     {:name "o2" :side "left" :type "photonic"}
                                     {:name "o3" :side "left" :type "photonic"}]}})
  (testing "a model-backed photonic device centers on the cursor like a ::bg builtin"
    (e/add-device "resistor" [10 10])
    (is (= [9 9] [(:x @e/staging) (:y @e/staging)]) "resistor centers via ::bg")
    (e/add-device "bend" [10 10] :model "pk.bend")
    (is (= [9 9] [(:x @e/staging) (:y @e/staging)])
        "bend with a model centers via the modeldb port perimeter"))
  (testing "a device taller than one tile offsets by half its height"
    (e/add-device "mmi-1x2" [10 10] :model "pk.tall")
    (is (= [9 8] [(:x @e/staging) (:y @e/staging)])
        "1x3 device: round(10 - 1/2 - 1) = 9, round(10 - 3/2 - 1) = 8"))
  (testing "model-less text keeps its tile under the cursor"
    (e/add-device "text" [10 10])
    (is (= [10 10] [(:x @e/staging) (:y @e/staging)])))
  (reset! pform/modeldb {}))

(deftest picker-key-down-test
  (let [filtered [["models:a" {:name "a"}] ["models:b" {:name "b"}] ["models:c" {:name "c"}]]
        highlight (r/atom 0)
        confirmed (atom nil)
        confirm! #(reset! confirmed %)
        key (fn [k] #js {:key k :preventDefault (fn [])})]
    (testing "arrows move the highlight, wrapping at both ends"
      (e/picker-key-down! (key "ArrowUp") filtered highlight confirm!)
      (is (= 2 @highlight) "ArrowUp wraps to the last row")
      (e/picker-key-down! (key "ArrowDown") filtered highlight confirm!)
      (is (= 0 @highlight) "ArrowDown wraps to the first row")
      (e/picker-key-down! (key "ArrowDown") filtered highlight confirm!)
      (is (= 1 @highlight)))
    (testing "Enter confirms the highlighted row's model id"
      (e/picker-key-down! (key "Enter") filtered highlight confirm!)
      (is (= "models:b" @confirmed)))
    (testing "Enter clamps a highlight left dangling by a shrunken list"
      (reset! highlight 5)
      (reset! confirmed nil)
      (e/picker-key-down! (key "Enter") filtered highlight confirm!)
      (is (= "models:c" @confirmed)))
    (testing "Escape closes the modal without confirming"
      (reset! cm/modal-content [:something])
      (reset! confirmed nil)
      (e/picker-key-down! (key "Escape") filtered highlight confirm!)
      (is (nil? @cm/modal-content))
      (is (nil? @confirmed)))
    (testing "empty result set: arrows and Enter are no-ops"
      (reset! highlight 0)
      (reset! confirmed nil)
      (e/picker-key-down! (key "ArrowDown") [] highlight confirm!)
      (e/picker-key-down! (key "Enter") [] highlight confirm!)
      (is (= 0 @highlight))
      (is (nil? @confirmed)))))

(deftest add-tag-test
  (let [sel (r/atom [])]
    (cm/add-tag! sel "waveguides")
    (cm/add-tag! sel "type:ckt")
    (is (= ["waveguides" "type:ckt"] @sel))
    (testing "re-adding a selected tag is a no-op"
      (cm/add-tag! sel "waveguides")
      (is (= ["waveguides" "type:ckt"] @sel)))))

(deftest selected-tag-badges-test
  (let [sel (r/atom ["waveguides" "type:ckt"])
        chips #(vec (second (cm/selected-tag-badges sel)))
        click! (fn [chip] ((:on-click (second chip))))]
    (is (= 2 (count (chips))))
    (testing "a chip shows its tag and a removal affordance"
      (is (= ["waveguides" " ×"] (vec (drop 2 (first (chips)))))))
    (testing "property tags keep their prop-tag styling"
      (is (= [nil "prop-tag"] (mapv #(:class (second %)) (chips)))))
    (testing "clicking a chip removes only that tag"
      (click! (second (chips)))
      (is (= ["waveguides"] @sel))
      (click! (first (chips)))
      (is (= [] @sel)))))

(def ^:private picker-db
  {"models:pk.bend_euler" {:name "bend_euler" :type "bend"
                           :tags ["waveguides" "bend" "pk"]}
   "models:pk.bend_circular" {:name "bend_circular" :type "bend"
                              :tags ["waveguides" "circular" "pk"]}
   "models:pk.nfet" {:name "nfet" :type "nmos" :tags ["devices" "fet" "pk"]}
   "models:pk.cell_x" {:name "cell_x" :type "ckt" :tags ["cells" "pk"]}})

(deftest matching-tags-test
  (let [bends (into {} (filter (fn [[_ m]] (= "bend" (:type m))) picker-db))]
    (testing "a tag matching at least one entry on its own is kept"
      (is (= ["waveguides" "bend"] (e/matching-tags bends ["waveguides" "bend"]))))
    (testing "a tag matching nothing is dropped, and order is preserved"
      (is (= ["bend" "pk"] (e/matching-tags bends ["bend" "devices" "pk"]))))
    (testing "property tags are tested the same way"
      (is (= ["type:bend"] (e/matching-tags bends ["type:bend" "type:nmos"]))))
    (testing "tags that each match but do not intersect are left standing"
      (is (= ["bend" "circular"] (e/matching-tags bends ["bend" "circular"])))
      (is (empty? (cm/filter-models bends ["bend" "circular"] ""))
          "the empty intersection is the user's own filter, not stale state"))
    (testing "pruning is idempotent"
      (let [once (e/matching-tags bends ["bend" "devices" "pk"])]
        (is (= once (e/matching-tags bends once)))))))

(deftest picker-tag-persistence-test
  (reset! cm/modal-content nil)
  (reset! pform/modeldb picker-db)
  (testing "opening clears the search text and keeps tags that still apply"
    (reset! e/model-popup-filter "eul")
    (reset! e/model-popup-category ["waveguides" "pk"])
    (e/open-model-picker! "bend" (fn [_]))
    (is (= "" @e/model-popup-filter))
    (is (= ["waveguides" "pk"] @e/model-popup-category)))
  (testing "reopening for another device type drops only what cannot match"
    (e/open-model-picker! "nmos" (fn [_]))
    (is (= ["pk"] @e/model-popup-category)
        "pk is on the nfet too; waveguides cannot be"))
  (testing "a type: tag carried in from another picker cannot strand the list"
    (reset! e/model-popup-category ["type:ckt"])
    (e/open-model-picker! "nmos" (fn [_]))
    (is (= [] @e/model-popup-category))
    (is (seq (cm/filter-models (e/picker-models "nmos") @e/model-popup-category ""))))
  (testing "an empty intersection the user built survives the open"
    (reset! e/model-popup-category ["bend" "circular"])
    (e/open-model-picker! "bend" (fn [_]))
    (is (= ["bend" "circular"] @e/model-popup-category))
    (is (empty? (cm/filter-models (e/picker-models "bend") @e/model-popup-category ""))
        "and the chips are the way out of it"))
  (reset! e/model-popup-category [])
  (reset! pform/modeldb {})
  (reset! cm/modal-content nil))
