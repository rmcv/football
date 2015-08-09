(ns football.core
  (:require [clj-http.client :as c]
            [reaver :as r :refer [parse extract-from text attr]]
            [clojure.data.json :as json]))

(defn- try-parse-as-num [val]
  (if (and val (re-matches #"-?[\d,\.\s]+" val))
    (read-string (clojure.string/replace val #"," ""))
    val))

(def text-or-num (comp try-parse-as-num text))

(def parse-id-in-link (comp last #(re-find #"&id=(\d+)" %) #(or % "") (attr :href)))

(defn- make-keyword [x]
  (->> (.replace x " " "-")
       (clojure.string/lower-case)
       keyword))

(defmacro extract-table-rows [doc selector prefix & fields]
  (let [fnames      (mapv #(if (vector? %) (first %) %) fields)
        fextractors (->> fields
                         (map #(if (vector? %)
                                 (let [[nm d] %]
                                   [(str prefix (or (:selector d) nm))
                                    (or (:extractor d) `text-or-num)])
                                [(str prefix (name %))
                                 `text-or-num]))
                         (apply concat))]
    `(extract-from ~doc ~selector ~fnames ~@fextractors)))

(defn get-all-player-list [pos]
  (let [doc (->> (c/get "https://football.barclayssportshub.com/stats.aspx"
                        {:query-params {:page "playerSearch"
                                        :t    pos}})
                 :body
                 parse)]
    (extract-table-rows doc
                        "table.genericTable tbody tr"
                        "td."
                        [:id {:selector  "playerStatsPlayer a"
                              :extractor parse-id-in-link}]
                        [:playerStatsPlayer {:selector  "playerStatsPlayer a"}]
                        :playerStatsInjury
                        :playerStatsPosition
                        :playerStatsClub
                        :playerStatsCost
                        :playerStatsPlayed
                        :playerStatsScored
                        :playerStatsConceded
                        :playerStatsYellow
                        :playerStatsRed
                        :playerStatsPoints)))

(defn get-player-detail [id]
  (let [d    (->> (c/get "https://football.barclayssportshub.com/stats.aspx"
                         {:query-params {:page "player"
                                         :id   id}})
                  :body
                  parse)
        curr (->> (extract-from d "div.statsPlayerCurrentDetails div.statsPlayerCurrentRow"
                                [:d :v]
                                "div.statsPlayerCurrentHeader" (comp make-keyword text)
                                "div.statsPlayerCurrentValue" text->num)
                  (#(zipmap (map :d %) (map :v %))))
        hist (->> (extract-table-rows d
                                      "table.genericTable tbody tr"
                                      "td."
                                      :statsPlayerHistoricalSeason
                                      :statsPlayerHistoricalPlayed
                                      :statsPlayerHistoricalScored
                                      :statsPlayerHistoricalConceded
                                      :statsPlayerHistoricalYellow
                                      :statsPlayerHistoricalRed
                                      :statsPlayerHistoricalSubbed
                                      :statsPlayerHistoricalCleanSheet
                                      :statsPlayerHistoricalPoints)
                  (remove #(nil? (:statsPlayerHistoricalSeason %))))]
    (assoc curr :historical hist)))


(def ^:dynamic *players*
  (let [roles   {1 :goal-keeper
                 2 :defender
                 3 :mid-fielder
                 4 :striker}
        players (->> (keys roles)
                     (pmap (fn [r] (->> (get-all-player-list r)
                                        (map #(assoc % :role (roles r))))))
                     (apply concat))
        details (->> players
                     (map :id)
                     (pmap get-player-detail))]
    (map merge players details)))


(defn- get-match-report [page]
  (let [doc     (->> (c/get (str "https://football.barclayssportshub.com/" page))
                     :body
                     parse)
        res     (->> (extract-from doc
                                   "#ctl14_SimpleHolder1_TBLResults tbody tr"
                                   [:p1 :p1-id :pt1 :gs1 :pt2 :gs2 :p2 :p2-id]
                                   "td:eq(1) a" text
                                   "td:eq(1) a" parse-id-in-link
                                   "td:eq(2)" text-or-num
                                   "td:eq(3)" text-or-num
                                   "td:eq(4)" text-or-num
                                   "td:eq(5)" text-or-num
                                   "td:eq(6) a" text
                                   "td:eq(6) a" parse-id-in-link)
                     (remove #(nil? (:p1 %))))
        assists (->> (extract-from doc
                                   "#ctl14_SimpleHolder1_TBLAssists tbody tr"
                                   [:p1 :p1-id :a1 :a2 :p2 :p2-id]
                                   "td:eq(1) a" text
                                   "td:eq(1) a" parse-id-in-link
                                   "td:eq(2)" text-or-num
                                   "td:eq(3)" text-or-num
                                   "td:eq(4) a" text
                                   "td:eq(5) a" parse-id-in-link)
                     (remove #(nil? (:p1 %))))]
    {:results res
     :assists assists}))

(let [doc (->> (c/get "https://football.barclayssportshub.com/MatchResult.aspx")
               :body
               parse)]
  (->> (extract-table-rows doc
                           "table.genericTable.matchResult tbody tr"
                           "td."
                     :upcomingMatchDate
                     :upcomingMatchHome
                     :upcomingMatchScore
                     :upcomingMatchAway
                     [:upcomingMatchReport {:selector "upcomingMatchReport a"
                                            :extractor (attr :href)}]
                     )
       (remove (comp nil? :upcomingMatchDate))))

(comment


  (defn- hist-points [p n]
    (let [hs (->> (:historical p)
                  reverse
                  (take n))
          cnt (count hs)
          tp  (reduce + (map :statsPlayerHistoricalPoints hs))]
      (if (= 0 cnt)
        0
        (Math/max  (int 100) (int (* 10000 (/ (double tp) cnt)))))))

  (let [d (->> *players*
               (group-by :playerStatsClub)
               (mapv (fn [[g v]]
                       {:id (Math/abs (hash g))
                        :name g
                        :children (mapv (fn [p] {:id (read-string (:id p))
                                                 :name (:playerStatsPlayer p)
                                                 :size (:playerStatsCost p)}) v)})))
        r {:id 0 :name "" :children d}]
    (->> (json/write-str r)
         (spit "flare.json")))

  (let [d (->> *players*
               (filter #(> (:playerStatsPoints %) 0))
               (group-by :playerStatsClub)
               (mapv (fn [[g v]]
                       {:id (Math/abs (hash g))
                        :name g
                        :children (mapv (fn [p] {:id (read-string (:id p))
                                                 :name (:playerStatsPlayer p)
                                                 :size (:playerStatsPoints p)}) v)})))
        r {:id 0 :name "" :children d}]
    (->> (json/write-str r)
         (spit "flare.json"))))



(defn- create-group [f-group s & keys ]
  (if (empty? keys)
    s
    (let [c (first keys)]
      (for [[k v] (group-by #(get % c) s)]
        (f-group k (apply create-group f-group v (rest keys)))))))

(->> (create-group  (fn [k [v1 & r :as v]]
                      (if (empty? r)
                        {:id (:id v1)
                         :name k
                         :size (or (:playerStatsPoints v1) 0)}
                        {:id 0
                         :name k
                         :children v}))

                    ;; show only players with positive points
                    (->> *players*
                         (filter #(> (:playerStatsPoints %) 0)))

                    ;; grouping criteria
                    :playerStatsClub :playerStatsPlayer)

     (#(hash-map :id 0 :name "" :children %))
     json/write-str
     (spit "flare.json"))
