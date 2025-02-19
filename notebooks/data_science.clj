(ns data-science
  (:require [clojure.set :refer [join rename-keys project]]
            [clojure.string :as str]
            [dk.ative.docjure.spreadsheet :as ss]
            [kixi.stats.core :as kixi-stats]
            [kixi.stats.protocols :as kixi-p]
            [meta-csv.core :as csv]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [nextjournal.clerk :as clerk]))

;; # 一个小型数据科学示例 🔢
^{:nextjournal.clerk/visibility {:code :hide}}

;; # 在数据中探索世界

;; 现实数据科学中的一个挑战是从不同的来源以许多不同的格式获取数据。
;; 在本notebook中，我们将使用从TSV文件、Excel电子表格和数据库查询中获取的数据，
;; 探索一些关于世界的事实。

;; ## 预期寿命

;; 首先，我们将使用 [meta-csv](https://github.com/ngrunwald/meta-csv) 库
;; 读取包含最新 CIA World Factbook 数据的 TSV 文件。
(def cia-factbook
  (csv/read-csv "./datasets/cia-factbook.tsv"))

;; 在数据查看器中展开结果告诉我们，感兴趣的列中存在一些 `nil` 值，
;; 并且我们的 TSV 导入器因此被抛出异常，因此没有将数值列转换为数字类型。

;; 我们将使用普通的 Clojure 序列函数对这个表进行一些后处理，
;; 以过滤掉在我们的感兴趣的列中具有 `nil` 值的行，选择这些行，
;; 将字符串转换为数字，并且——因为我们是 Clojurist——将键转换为关键字。
(def life-expectancy
  (->> cia-factbook
       (remove #(some nil? (map (partial get %) ["Country" "GDP/cap" "Life expectancy"])))
       (map #(sorted-map :country (str/trim (get % "Country"))
                         :gdp (read-string (get % "GDP/cap"))
                         :life-expectancy (read-string (get % "Life expectancy"))))))

;; 在数据结构浏览器中看起来相当不错，但是以表格形式获得概览会更容易。
;; 幸运的是，Clerk 的内置表格查看器能够推断出如何自动处理所有最常见的行和列配置。
(clerk/table life-expectancy)

;; 我们还可以绘制数据图，以查看我们的两个感兴趣的变量（人均 GDP 和预期寿命）之间是否存在任何可见的相关性。
(clerk/vl
 {:data {:values life-expectancy}
  :width 700
  :height 500
  :mark {:type :point}
  :encoding {:x {:field :gdp
                 :type :quantitative}
             :y {:field :life-expectancy
                 :type :quantitative}
             :tooltip {:field :country}}})

;; 毫不奇怪，生活在一个极其贫穷的国家对预期寿命有负面影响。
;; 另一方面，看起来一旦人均 GDP 超过每年 10-15k 美元，情况就开始趋于平缓。
;; 还出现了一些其他有趣的模式：新加坡和日本的预期寿命相似，
;; 尽管前者的 GDP 是后者的两倍，而卡塔尔——数据集中人均 GDP 最富裕的国家——
;; 与多米尼加共和国的平均预期寿命相似。

;; ## 不平等

;; 现在，让我们尝试使用包含每个国家/地区的 GINI 系数（一种广泛使用的收入不平等指标）的电子表格中的信息进行相同的实验。
;; 我们将使用一个名为 [Docjure](https://github.com/mjul/docjure) 的库，该库提供对 Microsoft Office 文件格式的访问。

;; Docjure 的 API 有点底层，并且没有使明显的任务变得容易，
;; 因此我们将使用这个辅助函数使下面的代码更清晰。
;; 查看逐行注释以了解此函数的工作原理。
(defn load-first-sheet
  "将 Excel 电子表格的第一个工作表作为 map 的 seq 返回。"
  [filename]
  (let [rows (->> (ss/load-workbook filename) ; 加载文件
                  (ss/sheet-seq)              ; 文件中工作表的 seq
                  first                       ; 取第一个（也是唯一一个）
                  ss/row-seq                  ; 从中获取行
                  (mapv ss/cell-seq))         ; 每行 -> 单元格的 seq
        ;; 分离标题以生成 map 的 seq
        headers   (mapv (comp keyword ss/read-cell) (first rows))]
    ;; 映射行，创建以标题作为键的新 map
    (mapv #(zipmap headers (map ss/read-cell %)) (rest rows))))

;; 现在，我们将使用几行代码来：
;; 1. 加载电子表格数据。
;; 2. 使用 `clojure.set` 的 `join` 函数将我们新加载的 GINI 电子表格与我们先前准备的预期寿命数据结合起来，
;;    因为它们都是具有 `:country` 键的 map 的序列。
;; 3. 在每个 map 中关联一个 `:gini` 键到世界银行的数字，但回退到 CIA 的估计值。
;;    （这些类型的小型编程任务是数据整理的一个持续特征。）
(def expectancy-and-gini
  (->> (load-first-sheet "datasets/countries-gini.xlsx")
       (join life-expectancy)
       (keep #(if-let [gini (or (:giniWB %) (:giniCIA %))]
                (assoc % :gini gini)
                nil))))

;; 展开 Clojure 数据结构使它看起来可以用于我们的比较。
;; 让我们绘制数据图，以查看从最平等到最不平等的国家/地区的列表：
(clerk/vl
 {:data {:values expectancy-and-gini}
  :width 600
  :height 1600
  :mark {:type :point}
  :encoding {:x {:field :gini
                 :type :quantitative}
             :y {:field :country
                 :type :nominal
                 :sort :x}
             :tooltip {:field :country}}})

;; 现在来看看不平等和预期寿命是否相关：
(clerk/vl
 {:data {:values expectancy-and-gini}
  :mark :rect
  :width 700
  :height 500
  :encoding {:x {:bin {:maxbins 25}
                 :field :life-expectancy
                 :type :quantitative}
             :y {:bin {:maxbins 25}
                 :field :gini
                 :type :quantitative}
             :color {:aggregate :count :type :quantitative}}
  :config {:view {:stroke :transparent}}})

;; 看起来长寿国家的大部分也处于不平等分布的较低三分之二。
;; 稍微过滤一下显示，唯一真正长寿的国家/地区的不平等系数高于 ~50 的是香港。
(clerk/table
 (->> (filter #(< 50 (:gini %)) expectancy-and-gini)
      (sort-by :life-expectancy)))

;; ## 幸福感

;; 让我们来看看幸福感！这次，我们将使用 [jdbc.next](https://github.com/seancorfield/next-jdbc)
;; 对 Sqlite 数据执行 SQL 查询，该数据包含一个国家/地区及其相对幸福度评分的表。
;; 请注意，我们正在使用 `clojure.set` 的 `rename-keys` 函数将列名 `:country_or_region` 更改为 `:country`，
;; 以便此表易于与我们的其他表连接。
(def world-happiness
  (let [_run-at #inst "2021-11-26T08:28:29.445-00:00" ; 更改此值以重新运行查询！
        ds (jdbc/get-datasource {:dbtype "sqlite" :dbname "./datasets/happiness.db"})]
    (->> (with-open [conn (jdbc/get-connection ds)]
           (jdbc/execute! conn ["SELECT * FROM happiness"]
                          {:return-keys true :builder-fn rs/as-unqualified-lower-maps}))
         (map #(rename-keys % {:country_or_region :country})))))

;; 查看幸福感数据，似乎所有常见的嫌疑人——北欧人、西欧人、加拿大人和新西兰人——
;; 都根据自己的估计过着相当美好的生活。
;; 仔细观察，我们发现虽然前 20 个国家/地区都相对富裕，但很明显 GDP 与_该队列内的_幸福感没有很强的相关性。
(clerk/table world-happiness)

;; 接下来，我们使用 [kixi.stats](https://github.com/MastodonC/kixi.stats) 为此数据集计算线性回归。
^{::clerk/viewer {:transform-fn (clerk/update-val kixi-p/parameters)}}
(def linear-regression
  (transduce identity (kixi-stats/simple-linear-regression :score :gdp) world-happiness))

;; 我们将使用此线性回归来扩充我们的数据集，以便每个数据点也获得一个 `:regression` 值。
(def world-happiness+regression
  (mapv (fn [{:as datapoint :keys [score]}]
          (assoc datapoint :regression (kixi-p/measure linear-regression score)))
        world-happiness))

;; 让我们绘制幸福感和 GDP 之间的关系图，以从整体上了解情况。
;; 您可以将鼠标悬停在单个数据点上以获取更多信息：

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/vl
 {:data {:values world-happiness+regression}
  :width 700
  :height 500
  :layer [{:mark {:type :point}
           :encoding {:x {:field :score
                          :type :quantitative
                          :scale {:zero false}}
                      :y {:field :gdp
                          :type :quantitative}
                      :tooltip {:field :country}}}
          {:mark {:type :line :color "#ccc"}
           :encoding {:x {:field :score
                          :type :quantitative
                          :scale {:zero false}}
                      :y {:field :regression
                          :type :quantitative}}}]})

;; 看起来，正如我们可能预料的那样，一般来说，富裕国家比贫穷国家更幸福，尽管存在差异和异常值。
;; 例如，芬兰排名第一，但人均 GDP 与第 58 名日本相似。
;; 也许更引人注目的是，卡塔尔的数据集中人均 GDP 最高，但卡塔尔人的平均幸福感与萨尔瓦多的人民大致相同。
;; 同样，博茨瓦纳的人均 GDP 是马拉维的五倍，但它的人民并没有因此而更幸福。
;; 如果我被迫猜测原因，我可能会推测，一个繁荣的国家，其所有财富都集中在极少数人手中，对于普通人来说仍然是一个相当糟糕的居住地。

;; 调查这种可能性的一种方法是绘制富裕世界中平等与幸福感之间的相关性。
;; 我们将再次使用 `join`，但我们将首先使用 `clojure.set` 的 `project`（类似于 SQL 投影）
;; 从幸福感数据集中仅提取 `:country` 和 `:score`，然后按 GDP 排序并取前 20 个国家/地区。
(clerk/vl
 {:data {:values (->> (project world-happiness [:country :score])
                      (join expectancy-and-gini)
                      (sort-by :gdp >)
                      (take 20))}
  :width 700
  :height 500
  :mark {:type :point}
  :encoding {:x {:field :score
                 :type :quantitative
                 :scale {:zero false}}
             :y {:field :gini
                 :type :quantitative
                 :scale {:zero false}}
             :tooltip {:field :country}}})

;; 至少乍一看，这确实支持了这样一种观点，即最幸福的人——就像寿命最长的人一样——倾向于居住在基尼分布中更平等的部分的国家。

;; 我希望这个例子能给你一些你想要调查的想法。
