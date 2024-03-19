(ns core
  (:require [clojure.string]
            [clojure.java.io :as io]
            [hiccup.core :as hiccup]))


(defn- get-comment [line]
  (when (= \# (first line))
    (-> (subs line 1 (count line))
        (clojure.string/trim))))


(def bad-start-chars #{\space \tab})

(defn- get-definition [line]
  (when-not (contains? bad-start-chars (first line))
    (let [match (re-find #"^.+:\s*" line)]
      (when-not (and (> (count line) (count match))
                     (= \= (nth line (count match))))
        match))))


(defn extract-defs [file]
  (with-open [rdr (io/reader file)]

    (->> (line-seq rdr)
         (reduce
           (fn [doc line]
             (let [comment    (get-comment line)
                   definition (get-definition line)]
               (cond
                 comment
                 (update doc :comments conj comment)

                 (not-empty definition)
                 (-> doc
                     (update :definitions conj {:command definition
                                                :doc     (:comments doc)})
                     (assoc :comments []))

                 :else
                 (assoc doc :comments [])))
             )
           {:file        file
            :type        (.getName file)
            :path        (->> (clojure.string/split (.getPath file) #"/")
                              (filter #(not= "." %))
                              (drop-last))
            :definitions []
            :comments    []}))))


(def files-we-care-about #{"justfile" "Makefile"})

(defn extract-files [folder-path]
  (->> (file-seq (io/file folder-path))
       (filter #(contains? files-we-care-about (.getName %)))
       (map extract-defs)))


(defn just-commands-table [folder]
  (->> (extract-files folder)
       (sort-by #(.getPath (:file %)))
       (map (fn [{:keys [type path definitions]}]
              [:details
               [:summary
                {:class (if (= "justfile" type)
                          "has-background-primary"
                          "has-background-warning")}
                [:span "/"]
                (map (fn [p] [:span p "/"]) path)]
               [:table.table.is-striped.is-hoverable.is-fullwidth.is-narrow
                [:tbody
                 (->> definitions
                      (map (fn [{:keys [command doc]}]
                             [:tr
                              [:td (->> (clojure.string/split command #"\s")
                                        (map-indexed (fn [i s]
                                                       (if (zero? i)
                                                         [:strong s " "]
                                                         [:span s " "]))))]
                              [:td
                               (->> doc
                                    (map (fn [d]
                                           [:div d])))]])))]]]))))


(defn wrap-html [& content]
  (hiccup/html
      [:html
       [:head
        [:meta {:charset "utf-8"}]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
        [:title "Matter Cheat Sheet"]
        [:link {:rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"}]]
       [:body
        [:section.section
         [:div.container
          content]]]]))

(comment

  (spit "index.html"
        (wrap-html
          [:h1.is-size-3 "Commands"]
          (just-commands-table "./")))

  )
