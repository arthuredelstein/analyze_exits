(ns analyze-exits.core
  (require
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.string :as s]
   [hiccup.page :as page]
   [hiccup.util]
   ))

(defn compress-css
  [css-string]
  (s/replace css-string #"\s+" " "))

(defn date-from-filename
  [filename]
  (let [text (re-find #"[0-9]+_[0-9]+" filename)
        formatter (java.text.SimpleDateFormat. "yyyyMMdd_hhmm")]
    (.parse formatter text)))

(defn utc-string
  [date]
  (let [tz (java.util.TimeZone/getTimeZone "UTC")
        df (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss 'UTC'")]
    (.setTimeZone df tz)
    (.format df date)))


(defn mean
  [numbers]
  (/ (apply + numbers) (count numbers)))

(defn raw-results-files [type]
  (->> (file-seq (io/as-file (if (= type :exits)
                               "../all_exit_results"
                               "../all_relay_results")))
       (map #(.getPath %))
       (filter #(.endsWith % ".json"))
       (remove #(.endsWith % "_latest.json"))
       sort))

(defn read-result-file
  "Load the data from a result file."
  [file]
  (json/read-str (slurp file)))

(defn failure-rates
  [result-data]
  (into {}
        (for [[fp results] (result-data "example.com")]
          (let [fp1 (.replace fp "$" "")
                total-count (count results)
                failure-count (count (filter #(not= (% 0) "SUCCEEDED") results))]
            (when (> total-count 0)
              [fp1 (double (/ failure-count total-count))])))))

(defn connection-times
  [result-data]
  (into {}
        (for [[fp results] (result-data "example.com")]
          (let [fp1 (.replace fp "$" "")
                times (->> results
                           (map #(nth % 2))
                           (filter #(not= -1 %)))]
            (when (pos? (count times))
              [fp1 (mean times)])))))

(def fields
  ["nickname"
   "fingerprint"
   "as_name"
   "country_name"
   "contact"
   "platform"
;   "or_addresses"
   "bandwidth_rate"
   "exit_probability"
   ])

(defn onionoo-exits
  [fields]
  (let [url (str "https://onionoo.torproject.org/details?type=relay&fields="
                 (s/join "," fields))]
    (-> url
        read-result-file
        (get "relays"))))

(defn as-percent
  [val]
  (format "%3.1f" (* 100. val)))

(defn average-failure-rate [exits failure-rates]
  (apply +
         (for [exit exits]
           (* (get exit "exit_probability" 0)
              (or
               (get failure-rates (get exit "fingerprint"))
               0)))))

(defn assemble-data-table
  [exits fields failure-rates connection-times]
  (let [exits-map (group-by #(get % "fingerprint") exits)]
    (for [[fp failure-rate] failure-rates]
      (let [connection-time (connection-times fp)]
        (when-let [exit (first (exits-map fp))]
          (let [failure-percent (as-percent failure-rate)]
            (-> exit
                (assoc "failure_rate" failure-percent)
                (assoc "connection_time" connection-time))))))))

(defn timeout-probability
  [data-row]
  (let [exit-probability (or (get data-row "exit_probability") 0.000001)
        failure-rate (Double/parseDouble (get data-row "failure_rate"))]
    (* exit-probability failure-rate)))

(defn data-table
  [type]
  (let [fields+ (concat ["failure_rate" "connection_time"] fields)
        latest-result-file (last (raw-results-files type))
;        _ (println latest-result-file)
        latest-result (read-result-file latest-result-file)
;        _ (println latest-result)
        exits (or (latest-result "_relays")
                  (onionoo-exits fields))
;        _ (println exits)
        file-date (date-from-filename latest-result-file)
        failure-rates (failure-rates latest-result)
        connection-times (connection-times latest-result)
;        _ (println failure-rates)
        average (average-failure-rate exits failure-rates)
;        _ (println average)
        raw-data-table (assemble-data-table exits fields
                                            failure-rates connection-times)
;        _ (println raw-data-table)
        body (->> raw-data-table
                  (remove nil?)
                  (sort-by #(+ (get % "connection_time")))
                  (sort-by (if (= type :exits) timeout-probability #(Double/parseDouble (get % "failure_rate"))))
                  reverse)]
    [fields+ body average file-date]))

(defn html-table
  [header body]
  [:table
   [:tr (for [item header]
          [:th (hiccup.util/escape-html item)])]
   (for [row body]
     [:tr (for [item header]
            (let [text (hiccup.util/escape-html (get row item))]
              (if (= item "fingerprint")
                [:td [:a {:href (str "https://metrics.torproject.org/rs.html#details/" text)} text]]
                [:td text])))])])

(defn html-page
  [type title header body average file-date]
  (page/html5
   [:head
    [:title title]
    [:meta {:charset "utf-8"}]
    [:style {:type "text/css"} (compress-css (slurp "main.css"))]
    [:link {:href "data:image/x-icon;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQEAYAAABPYyMiAAAABmJLR0T///////8JWPfcAAAACXBIWXMAAABIAAAASABGyWs+AAAAF0lEQVRIx2NgGAWjYBSMglEwCkbBSAcACBAAAeaR9cIAAAAASUVORK5CYII=" :rel "icon" :type "image/x-icon"}]
    ]
   [:body
    [:h2 title]
    [:div {:style "max-width: 600px"}
     [:p (str "Average probability-weighted failure rate: " (as-percent average) "%")]
     [:p (str "Test ran at " (utc-string file-date))]
     [:p {:style "font-weight: bold; color: DarkRed;"}
      "Dear exit relay operator: are connections to your exit failing or slow? Please check for connectivity, latency and DNS timeouts."]
     [:p "Source code: "
      [:a {:href "https://github.com/arthuredelstein/tor_dns_survey"} "scanner"]
      " | "
      [:a {:href "https://github.com/arthuredelstein/analyze_exits"} "table"]]
     [:p "Historical JSON data is "
      [:a {:href (if (= type :exits) "/exits-json/" "/relays-json/")} "here"] ". "
      "Latest JSON results are "
      [:a {:href (if (= type :exits) "/exits-json/exit_results_latest.json" "/relays-json/relay_results_latest.json")} "here"] "."]]
    (html-table header body)
    ]))

(defn write-page!
  [type dir filename]
  (let [[header body average file-date] (data-table type)]
    (spit (str dir (or filename "temp.html"))
          (html-page type
                     (if (= type :exits)
                       "Tor Exit Failures" "Tor Relay Failures")
                     header body average file-date))))

(defn daily-raw-to-summary! [file]
  (let [data (read-result-file file)
        name (.getName (io/as-file file))
        rates (failure-rates data)]
    (when (not-empty rates)
      (let [rates (failure-rates data)]
        (spit (str "../json/" name)
              (json/write-str rates))
        (spit (str "../json/" (if (= type :exits) "exit" "relay")
                                  "_results_latest.json")
              (json/write-str rates))))
    name))

(defn -main
  "Main program."
  []
  (write-page! :exits "../exits/" "index.html")
  (daily-raw-to-summary! (last (raw-results-files :exits)))
  (write-page! :relays "../relays/" "index.html")
  (daily-raw-to-summary! (last (raw-results-files :relays)))
  )

(defn generate-all-daily-summaries! []
  (doseq [file (raw-results-files :exits)]
    (println (daily-raw-to-summary! file))))
