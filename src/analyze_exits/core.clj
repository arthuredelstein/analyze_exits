(ns analyze-exits.core
  (require
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.string :as s]
   [hiccup.page :as page]
   [hiccup.util]
   ))

(defn date-from-filename
  [filename]
  (let [text (re-find #"[0-9]+_[0-9]+" filename)
        formatter (java.text.SimpleDateFormat. "yyyyMMdd_hhmm")]
    (.parse formatter text)))

(defn utc-string
  [date]
  (let [tz (java.util.TimeZone/getTimeZone "UTC")
        df (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm 'UTC'")]
    (.setTimeZone df tz)
    (.format df date)))


(defn raw-results-files []
  (->> (file-seq (io/as-file "../all_exit_results"))
       (map #(.getPath %))
       (filter #(.endsWith % ".json"))
       sort))

(defn read-result-file
  "Load the data from a result file."
  [file]
  (json/read-str (slurp file)))

(defn timeout-rates [result-data]
  (into {}
        (for [[fp results] (result-data "example.com")]
          [fp
           (double (/ (count (filter #(= (% 0) "TIMEOUT")
                                     results))
                      (count results)))])))

(def fields
  ["nickname"
   "fingerprint"
   "as_name"
   "country_name"
   "contact"
   "platform"
;   "or_addresses"
   "bandwidth_rate"
   "consensus_weight"
   ])

(defn onionoo-exits
  [fields]
  (let [url (str "https://onionoo.torproject.org/details?type=relay&flag=exit&fields="
                 (s/join "," fields))]
    (-> url
        read-result-file
        (get "relays"))))

(defn as-percent
  [val]
  (format "%3.1f" (* 100. val)))

(defn assemble-data-table
  [exits fields timeout-rates]
  (let [exits-map (group-by #(get % "fingerprint") exits)]
    (for [[fp timeout-rate] timeout-rates]
      (when-let [exit (first (exits-map fp))]
        (let [timeout-percent (as-percent timeout-rate)]
          (cons timeout-percent (map exit fields)))))))

(defn mean
  [numbers]
  (/ (apply + numbers) (count numbers)))

(defn data-table
  []
  (let [fields+ (cons "dns_timeout_percent" fields)
        latest-result-file (last (raw-results-files))
        latest-result (read-result-file latest-result-file)
        exits (or (latest-result "_relays")
                  (comment (onionoo-exits fields)))
        file-date (date-from-filename latest-result-file)
        timeout-rates (timeout-rates latest-result)
        average (mean (vals timeout-rates))
        raw-data-table (assemble-data-table exits fields timeout-rates)
        body (reverse (sort-by #(Double/parseDouble (first %)) (remove nil? raw-data-table)))]
    [fields+ body average file-date]))

(defn html-table
  [header body]
  [:table
   [:tr (for [item header]
          [:th (hiccup.util/escape-html item)])]
   (for [row body]
     [:tr (for [item row]
            [:td (hiccup.util/escape-html item)])])])

(defn html-page
  [header body average file-date]
  (page/html5
   [:head
    [:title "Tor Exit DNS Timeouts"]
    [:meta {:charset "utf-8"}]
    [:style {:type "text/css"} (slurp "main.css")]
    ]
   [:body
    [:h2 "Tor Exit DNS Timeouts"]
    [:div {:style "max-width: 600px"}
     [:p (str "Average unweighted timeout rate: " (as-percent average) "%")]
     [:p (str "Test ran at " (utc-string file-date))]
     [:p {:style "font-weight: bold; color: DarkRed;"}
      "Dear exit relay operator: is your exit timing out on DNS requests? Fix the problem "
      "by editing your resolv.conf, according to instructions "
      [:a {:href "https://lists.torproject.org/pipermail/tor-relays/2017-November/013531.html"} "here"] "."]]
    (html-table header body)
    ]))

(defn write-page!
  [filename]
  (let [[header body average file-date] (data-table)]
    (spit (str "../exits/" (or filename "temp.html"))
          (html-page header body average file-date))))

(defn -main
  "Main program."
  []
  (write-page! "index.html")
  )
