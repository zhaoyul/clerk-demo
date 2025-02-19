(ns sicmutils
  (:refer-clojure
   :exclude [+ - * / partial ref zero? numerator denominator compare = run!])
  (:require [nextjournal.clerk :as clerk]
            [sicmutils.env :as e :refer :all]
            [sicmutils.expression.render :as xr]))

;; ## Lagrangian（拉格朗日量）
;;
;; 从一个从 `theta1`, `theta2` 到矩形坐标的坐标变换开始。我们将通过将此变换与具有熟悉的 `T - V` 形式的矩形拉格朗日量组合来生成我们的拉格朗日量。
(defn angles->rect [l1 l2]
  (fn [[_ [theta1 theta2]]]
    (let [x1 (* l1 (sin theta1))
          y1 (- (* l1 (cos theta1)))
          x2 (+ x1 (* l2 (sin (+ theta1 theta2))))
          y2 (- y1 (* l2 (cos (+ theta1 theta2))))]
      (up x1 y1 x2 y2))))

;; `T` 描述了两个粒子在矩形坐标系中的动能之和。
(defn T [m1 m2]
  (fn [[_ _ [xdot1 ydot1 xdot2 ydot2]]]
    (+ (* 1/2 m1 (+ (square xdot1)
                    (square ydot1)))
       (* 1/2 m2 (+ (square xdot2)
                    (square ydot2))))))


;; `V` 描述了一个均匀的引力势，系数为 `g`，作用于质量分别为 `m1` 和 `m2` 的两个粒子。同样，这是用矩形坐标写成的。

(defn V [m1 m2 g]
  (fn [[_ [_ y1 _ y2]]]
    (+ (* m1 g y1)
       (* m2 g y2))))

;; 通过从 `(T m1 m2)` 中减去 `(V m1 m2 g)` 来形成矩形拉格朗日量 `L`：

(defn L-rect [m1 m2 g]
  (- (T m1 m2)
     (V m1 m2 g)))

;; 通过将 `L-rect` 与正确转换的 `angles->rect` 坐标变换组合，形成广义坐标（每个段的角度）中的最终拉格朗日量！

(defn L-double-pendulum [m1 m2 l1 l2 g]
  (compose (L-rect m1 m2 g)
           (F->C
            (angles->rect l1 l2))))

;; 拉格朗日量很大且复杂：

(def symbolic-L
  ((L-double-pendulum 'm_1 'm_2 'l_1 'l_2 'g)
   (up 't
       (up 'theta_1 'theta_2)
       (up 'theta_1dot 'theta_2dot))))

;; 让我们简化一下：

(simplify symbolic-L)

;; 更好的是，让我们将其渲染为 LaTeX，并创建一个辅助函数 `render-eq`，以便更容易渲染简化的方程：

(def render-eq
  (comp clerk/tex ->TeX simplify))

(render-eq symbolic-L)

;; 这是系统的运动方程：

(let [L (L-double-pendulum 'm_1 'm_2 'l_1 'l_2 'g)]
  (binding [xr/*TeX-vertical-down-tuples* true]
    (render-eq
     (((Lagrange-equations L)
       (up (literal-function 'theta_1)
           (literal-function 'theta_2)))
      't))))

;; 这些意味着什么？
;;
;; - 系统有两个自由度：$\theta_1$ 和 $\theta_2$。
;; - 在任何时间点 `t`，上述两个方程（充满了位置函数的一阶和二阶导数）将保持为真
;; - 系统可以使用这些方程来一次一个刻度地模拟系统。

;; ## Simulation（模拟）
;;
;; 接下来，让我们使用这些运动方程运行一个模拟，并收集每个坐标演化的数据。
;;
;; 这是练习 1.44 中指定的常量：
;;
;; 质量单位为 kg：

(def m1 1.0)
(def m2 3.0)

;; 长度单位为米：

(def l1 1.0)
(def l2 0.9)

;; `g` 的单位为 m/s^2：

(def g 9.8)

;; 这是两组 `theta1`、`theta2` 角度的初始对，分别对应于混沌和规则的初始条件：

(def chaotic-initial-q (up (/ Math/PI 2) Math/PI))
(def regular-initial-q (up (/ Math/PI 2) 0.0))

;; 将 `Lagrangian->state-derivative` 与 `L-double-pendulum` 组合会产生一个状态导数，我们可以将其与 ODE 求解器一起使用：

(def state-derivative
  (compose
   Lagrangian->state-derivative
   L-double-pendulum))

;; 最后，我们模拟的两个默认参数。我们将以 0.01 秒的步长记录数据，并模拟到 50 秒的时限。

(def step 0.01)
(def horizon 50)

;; `run!` 将返回一个包含 5001 个状态的序列，每个状态对应于模拟中每个测量点。较小arity的版本只传入默认的质量和长度，但如果需要，你可以用较大的arity版本覆盖这些值。

;; （这里的接口可能需要改进：`integrate-state-derivative` 使其更整洁，但我现在想将其公开。）

(defn run!
  ([step horizon initial-coords]
   (run! step horizon l1 l2 m1 m2 g initial-coords))
  ([step horizon l1 l2 m1 m2 g initial-coords]
   (let [collector     (atom (transient []))
         initial-state (up 0.0
                           initial-coords
                           (up 0.0 0.0))]
     ((evolve state-derivative m1 m2 l1 l2 g)
      initial-state
      step
      horizon
      {:compile? true
       :epsilon 1.0e-13
       :observe (fn [_ state]
                  (swap!
                   collector conj! state))})
     (persistent! @collector))))

;; 为每组初始条件运行模拟，并显示最终状态。首先是混沌的：

(def raw-chaotic-data
  (run! step horizon chaotic-initial-q))

;; 看起来不错：

(peek raw-chaotic-data)

;; 接下来，是规则的初始条件：

(def raw-regular-data
  (run! step horizon regular-initial-q))

;; 查看最终状态：

(peek raw-regular-data)

;; ## Measurements, Data Transformation（测量，数据转换）

;; 接下来，我们将绘制这些状态元组序列中捕获的测量值。

;; 该练习要求我们将系统的能量绘制为时间的函数。将 `Lagrangian->energy` 与 `L-double-pendulum` 组合会产生一个新函数（状态元组的函数！），该函数将返回系统中的当前能量：

(def L-energy
  (compose
   Lagrangian->energy
   L-double-pendulum))

;; `energy-monitor` 返回一个 `state` 函数，该函数在其闭包中存储一个初始能量值，并返回每个新状态的能量与原始能量相比的增量。

(defn energy-monitor [energy-fn initial-state]
  (let [initial-energy (energy-fn initial-state)]
    (fn [state]
      (- (energy-fn state) initial-energy))))

;; 最后，是大型的 `transform-data` 函数。我们将使用的图表库喜欢 Clojure 字典；`transform-data` 将我们的原始数据转换为一个字典序列，其中包含我们可能关心的所有值。这包括：

;; - 广义坐标角度
;; - 这些角度的广义速度
;; - 每个钟摆摆锤的矩形坐标
;; - `:d-energy`，模拟中每个点的能量误差

;; 这是 `transform-data`：

#_{:clj-kondo/ignore [:unresolved-symbol]}
(defn transform-data [xs]
  (let [energy-fn (L-energy m1 m2 l1 l2 g)
        monitor   (energy-monitor energy-fn (first xs))
        xform     (angles->rect l1 l2)
        pv        (principal-value Math/PI)]
    (map (fn [[t [theta1 theta2] [thetadot1 thetadot2] :as state]]
           (let [[x1 y1 x2 y2] (xform state)]
             {:t t
              :theta1 (pv theta1)
              :x1 x1
              :y1 y1
              :theta2 (pv theta2)
              :x2 x2
              :y2 y2
              :thetadot1 thetadot1
              :thetadot2 thetadot2
              :d-energy (monitor state)}))
         xs)))

;; 以下形式转换每个初始条件的原始数据，并将结果绑定到 `chaotic-data` 和 `regular-data` 以进行探索。

(def chaotic-data
  (doall
   (transform-data raw-chaotic-data)))

(def regular-data
  (doall
   (transform-data raw-regular-data)))

;; 这是最终的，转换后的混沌状态：

(last chaotic-data)

;; 这是类似的规则状态：

(last regular-data)

;; ## Data Visualization Utilities（数据可视化实用程序）

;; [Vega-Lite](https://vega.github.io/vega-lite/) 允许我们可视化系统。

;; 我不是这方面的专家，但现在这样做就可以了。

;; 首先，一个将我们上面生成的字典转换为 `x, y` 坐标序列的函数，并为每个钟摆摆锤的点标记不同的 ID：

(defn points-data [data]
  (mapcat (fn [{:keys [t x1 y1 x2 y2]}]
            [{:t  t
              :x  x1
              :y  y1
              :id :p1}
             {:t  t
              :x  x2
              :y  y2
              :id :p2}])
          data))

;; `segments-data` 生成钟摆段的端点，从摆锤到摆锤：

(defn segments-data [data]
  (mapcat (fn [{:keys [t x1 y1 x2 y2]}]
            [{:t  t
              :x 0
              :y 0
              :x2 x1
              :y2 y1
              :id :p1}
             {:t  t
              :x  x1
              :y  y1
              :x2 x2
              :y2 y2
              :id :p2}])
          data))

;; ## Visualizations（可视化）

;; 一个应该在 clojure.core 中的辅助函数
(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (when (some identity vs)
      (reduce #(rec-merge %1 %2) v vs))))

;; 有了这些工具，让我们制作一些图表。我将第一个图表称为 `system-inspector`；此函数将返回一个图表，该图表将允许我们使用时间滑块来演化系统，并监视两个角度、能量误差以及钟摆摆锤本身随时间演化的过程。

(defn system-inspector [data]
  {:config {:bar {:binSpacing 1, :discreteBandSize 5, :continuousBandSize 5}},
   :datasets {:points data}
   :vconcat [{:hconcat [{:layer (mapv (partial deep-merge
                                               {:encoding {:y {:field "y", :type "quantitative"},
                                                           :color {:field :id, :type :nominal},
                                                           :opacity {:condition {:test "abs(selected_t - datum['t']) < 0.00001", :value 1},
                                                                     :value 0.3},
                                                           :x {:field "x", :type "quantitative"},
                                                           :y2 {:field "y2", :type "quantitative"},
                                                           :x2 {:field "x2", :type "quantitative"},
                                                           :tooltip [{:field "x", :type "quantitative"}
                                                                     {:field "y", :type "quantitative"}]},
                                                :height 300,
                                                :width 400})
                                      [{:data {:values (points-data data)}
                                        :encoding {:size {:condition
                                                          {:test "abs(selected_t - datum['t']) < 0.00001", :value 200},
                                                          :value 5}
                                                   :opacity {:value 0.3}}
                                        :selection {:selected {:fields [:t],
                                                               :type :single,
                                                               :bind {:t {:min 0.01, :max 49.99, :input :range, :step 0.01}}}}
                                        :mark {:type "circle"}}
                                       {:data {:values (segments-data data)}
                                        :encoding {:opacity {:value 0}}
                                        :mark "rule"}])}
                        {:encoding {:y {:field :d-energy, :type "quantitative"},
                                    :size {:condition
                                           {:test "abs(selected_t - datum['t']) < 0.00001", :value 200},
                                           :value 5},
                                    :x {:field :t, :type "quantitative"},
                                    :y2 {:field "y2", :type "quantitative"},
                                    :x2 {:field "x2", :type "quantitative"},
                                    :tooltip [{:field :t, :type "quantitative"}
                                              {:field :d-energy, :type "quantitative"}]},
                         :mark {:type "circle"},
                         :width 400,
                         :height 300,
                         :data {:name :points}}]}
             {:hconcat (mapv (partial deep-merge {:encoding {:y {:field :unassigned,
                                                                 :type "quantitative",
                                                                 :scale {:domain [(- Math/PI) Math/PI]}},
                                                             :size {:condition {:test "abs(selected_t - datum['t']) < 0.00001", :value 200},
                                                                    :value 5},
                                                             :x {:field :t, :type "quantitative"},
                                                             :y2 {:field "y2", :type "quantitative"},
                                                             :x2 {:field "x2", :type "quantitative"},
                                                             :tooltip [{:field :t, :type "quantitative"}
                                                                       {:field :unassigned, :type "quantitative"}]},
                                                  :mark {:type "circle"},
                                                  :width 400,
                                                  :height 300,
                                                  :data {:name :points}})
                             [{:encoding {:y {:field :theta1},
                                          :tooltip [{:field :theta1}]}}
                              {:encoding {:y {:field :theta2},
                                          :tooltip [{:field :theta2}]}}])}]})

;; 这是混沌初始条件的系统监视器：

(clerk/vl
 (system-inspector chaotic-data))

;; 再次是规则初始条件的系统监视器：

(clerk/vl
 (system-inspector regular-data))

;; ## Generalized coordinates, velocities（广义坐标，速度）

;; `angles-plot` 生成一个绘图，没有动画，显示了 theta 角度及其相关的速度：

(defn angles-plot [data]
  (let [quarter-spec {:encoding {:y {:field :unassigned
                                     :type "quantitative"}
                                 :size {:value 5}
                                 :x {:field :t :type "quantitative"}
                                 :y2 {:field "y2" :type "quantitative"}
                                 :x2 {:field "x2" :type "quantitative"}
                                 :tooltip [{:field :t :type "quantitative"}
                                           {:field :unassigned :type "quantitative"}]}
                      :mark {:type "circle"}
                      :width 400
                      :height 300
                      :data {:name :points}}]
    {:config {:bar {:binSpacing 1 :discreteBandSize 5 :continuousBandSize 5}}
     :datasets {:points data}
     :width 800
     :height 600
     :hconcat [{:vconcat (mapv (partial deep-merge quarter-spec)
                               [{:encoding {:y {:field :theta1
                                                :scale {:domain [(- Math/PI) Math/PI]}}
                                            :tooltip [{:field :theta1}]}}
                                {:encoding {:y {:field :theta2
                                                :scale {:domain [(- Math/PI) Math/PI]}}
                                            :tooltip [{:field :theta2}]}}])}
               {:vconcat (mapv (partial deep-merge quarter-spec)
                               [{:encoding {:y {:field :thetadot1}
                                            :tooltip [{:field :theta1}]}}
                                {:encoding {:y {:field :thetadot2}
                                            :tooltip [{:field :theta2}]}}])}]}))

;; 这是混沌初始条件的角度：

(clerk/vl
 (angles-plot chaotic-data))

;; 这是规则初始条件的角度：

(clerk/vl
 (angles-plot regular-data))

;; ## Double Double Pendulum!（双重双摆！）

;; 这里还没有可视化，但是代码运行良好。

(defn L-double-double-pendulum [m1 m2 l1 l2 g]
  (fn [[t [thetas1 thetas2] [qdots1 qdots2]]]
    (let [s1 (up t thetas1 qdots1)
          s2 (up t thetas2 qdots2)]
      (+ ((L-double-pendulum m1 m2 l1 l2 g) s1)
         ((L-double-pendulum m1 m2 l1 l2 g) s2)))))

(def dd-state-derivative
  (compose
   Lagrangian->state-derivative
   L-double-double-pendulum))

(defn safe-log [x]
  (if (< x 1e-60)
    -138.0
    (Math/log x)))

(defn divergence-monitor []
  (let [pv (principal-value Math/PI)]
    (fn [[_ [thetas1 thetas2]]]
      (safe-log
       (Math/abs
        (pv
         (- (nth thetas1 1)
            (nth thetas2 1))))))))

(defn run-double-double!
  "Two different initializations, slightly kicked"
  [step horizon initial-q1]
  (let [initial-q2    (+ initial-q1 (up 0.0 1e-10))
        initial-qdot  (up 0.0 0.0)
        initial-state (up 0.0
                          (up initial-q1 initial-q2)
                          (up initial-qdot initial-qdot))
        collector     (atom (transient []))]
    ((evolve dd-state-derivative m1 m2 l1 l2 g)
     initial-state
     step
     horizon
     {:compile? true
      :epsilon 1.0e-13 ; = (max-norm 1.e-13)
      :observe (fn [_ state]
                 (swap! collector conj! state))})
    (persistent! @collector)))

(def raw-dd-chaotic-data
  (run-double-double! step horizon chaotic-initial-q))

;; 看起来不错：

(peek raw-dd-chaotic-data)

;; 接下来，是规则的初始条件：

(def raw-dd-regular-data
  (run-double-double! step horizon regular-initial-q))

;; 查看最终状态：

(peek raw-dd-regular-data)
