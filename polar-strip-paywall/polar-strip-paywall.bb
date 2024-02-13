#!/usr/bin/env bb

(require '[babashka.http-client :as http]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[babashka.cli :as cli])

(defn articles [{:keys [org] :as _cli-opts}]
  (-> "https://api.polar.sh/api/v1/articles/search"
      (http/get {:query-params {:show_unpublished  false
                                :organization_name org
                                :platform          "github"}
                 :headers      {"Authorization" (str "Bearer " (System/getenv "POLAR_API_TOKEN"))}})
      :body
      (json/parse-string true)
      :items))

(defn strip-paywall [s]
  (str/replace s #"</?Paywall[^>]*>" ""))

(defn published-days-ago? [days {:keys [published_at]}]
  (let [days->seconds #(* 86400 %)]
    (.isAfter (.plusSeconds (java.time.Instant/now) (days->seconds (- days)))
              published_at)))


(defn put-article [{:keys [id body]}]
  (http/put (str "https://api.polar.sh/api/v1/articles/" id)
            {:body    (json/encode {"body" body})
             :headers {"Authorization" (str "Bearer " (System/getenv "POLAR_API_TOKEN"))}}))

(defn paywalled-article? [{:keys [body]}]
  (str/includes? body "<Paywall>"))

(defn update-article! [{:keys [dry-run] :as _cli-opts} {:keys [title] :as article}]
  (let [msg (if dry-run "Would update article " "Updating article ")
        msg (str msg (pr-str title))]
    (print msg)
    (when-not dry-run
      (put-article article)
      (print "...done"))
    (println)))

(def cli-spec
  {:restrict [:org :help :dry-run :days-since-publish]
   :spec     {:days-since-publish {:default 7}
              :org                {:desc    "organization_name (required)"
                                   :require true}
              :help               {:coerce :boolean :alias :h}
              :dry-run            {:coerce :boolean
                                   :desc   "Print candidate articles, don't make changes."}}
   :error-fn (fn [{:keys [spec type cause msg option] :as data}]
               (if (= :org.babashka/cli type)
                 (case cause
                   :require
                   (println
                    (format "Missing required argument:\n%s"
                            (cli/format-opts {:spec (select-keys spec [option])})))
                   (println msg))
                 (throw (ex-info msg data)))
               (System/exit 1))})

(defn print-help []
  (println
   (str "Usage: polar-strip-paywall [OPTIONS] \n\nOPTIONS\n"
        (cli/format-opts (assoc cli-spec :order [:org :days-since-publish :dry-run :help]))
        \newline \newline
        "ENVIRONMENT VARIABLES" \newline
        "  POLAR_API_TOKEN    token from settings"
        \newline)))

(defn -main [{:keys [days-since-publish] :as args}]
  (let [instant-parse #(java.time.Instant/parse %)]
    (->> (articles args)
         (map #(update % :published_at instant-parse))
         (filter (partial published-days-ago? days-since-publish))
         (filter paywalled-article?)
         (map #(update % :body strip-paywall))
         (mapv (partial update-article! args)))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [help-spec   {:spec {:help (get-in cli-spec [:spec :help])}}
        parse-args  #(cli/parse-opts *command-line-args* %1)
        print-help? ((some-fn empty? :help) (parse-args help-spec))]
    (if print-help?
      (print-help)
      (-main (parse-args cli-spec)))))


(comment

  (cli/parse-opts '() {:spec {:help (get-in cli-spec [:spec :help])}})
  (put-article {:id "7891ada6-f2d0-46f7-b217-0719e784ecbc" :body "Hello"})

  #_:end)
