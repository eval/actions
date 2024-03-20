#!/usr/bin/env bb

(require '[clojure.string :as str]
         '[babashka.cli :as cli]
         '[polar.api :as pa] :reload :verbose
         '[eval.util :refer [whenp print-error]])

;; lexer
;;
(def code-fences "```")
(def snippet-open "<!--")
(def snippet-close "-->")

(defn- read-char
  "Advance `:pos` by `n` while not exceeding `(count input)`."
  ([l] (read-char l 1))
  ([{:keys [input pos] :as l} n]
   (assoc l :pos (min (+ pos n) (count input)))))

(defn- popq
  "Variant of `pop` that handles an empty queue (i.e. yields an empty queue)."
  [q]
  (cond
    (nil? q)   nil
    (empty? q) (empty q)
    :else      (pop q)))

(defn- assoc-state
  "Acceptable values `state`: var?, :return, :terminate"
  [l state]
  {:pre [(or (var? state) (#{:return :terminate} state))]}
  (cond
    (= state :return)    (assoc-state (update l :return (fnil popq []))
                                      (or (peek (:return l)) :terminate))
    (= state :terminate) (dissoc l :state)
    :else                (assoc l :state state)))

(defn- push-return [l state]
  {:pre [(or (var? state) (#{:current} state))]}
  (let [state (if (= :current state) (:state l) state)]
    (update l :return (fnil conj '()) state)))

(defn- peek-char
  ([l] (peek-char l 1))
  ([{:keys [input pos] :as _l} n]
   (let [end (+ pos n)]
     (cond
       (> end (count input)) nil
       :else                 (subs input pos end)))))

(defn- eos? [l]
  (nil? (peek-char l)))

(defn- accept-char
  [lexer char-set]
  (cond-> lexer
    (char-set (peek-char lexer)) (read-char)))

(defn- chars-read [{:keys [start pos input] :as _l}]
  (subs input start pos))

(defn- chars-read? [lexer]
  (seq (chars-read lexer)))

(defn- reset-start
  "Sets `:start` to the value of `:pos`."
  [{:keys [pos] :as l}]
  (assoc l :start pos))

(defn- emit-token
  "Add token to `:tokens` with `:t` `t` and `:v` the substring from
  `:input` based on the current `:pos` and `:start`. Does not emit
  when value for `:v` is empty."
  ([l t] (emit-token l t identity))
  ([{:keys [start] :as l} t f]
   (if-let [v (not-empty (chars-read l))]
     (let [token {:start start :t t :v v :end (+ start (count v))}]
       (-> l
           (update :tokens (fnil conj []) (f token))
           reset-start))
     l)))

(defn coming-up? [lexer s]
  (= s (peek-char lexer (count s))))


(defn lex-snippet [lexer]
  (cond
    (eos? lexer)                     (-> lexer
                                         (emit-token :text)
                                         (dissoc :state))
    (coming-up? lexer snippet-close) (-> lexer
                                         (read-char (count snippet-close))
                                         (emit-token :snippet)
                                         (assoc-state :return))
    :else                            (read-char lexer)))

(defn lex-code [lexer]
  (cond
    (eos? lexer)                   (-> lexer
                                       (emit-token :text)
                                       (dissoc :state))
    (coming-up? lexer code-fences) (-> lexer
                                       (read-char (count code-fences))
                                       (emit-token :code)
                                       (assoc-state :return))
    :else                          (read-char lexer)))

(defn lex-text [lexer]
  (cond
    (eos? lexer)                   (-> lexer (emit-token :text) (dissoc :state))
    (coming-up? lexer snippet-open) (-> lexer
                                        (emit-token :text)
                                        (push-return :current)
                                        (read-char (count snippet-open))
                                        (assoc-state #'lex-snippet))
    (coming-up? lexer code-fences) (-> lexer
                                       (emit-token :text)
                                       (push-return :current)
                                       (read-char (count code-fences))
                                       (assoc-state #'lex-code))
    :else                          (read-char lexer)))

(defn run [lexer]
  (loop [{lex-fn :state :as lexer} lexer]
    #_(prn lexer)
    (if-not lex-fn
      lexer
      (recur (lex-fn lexer)))))

(def default-lexer {:pos 0 :start 0 :tokens [] :state #'lex-text})

(defn tokenize* [s]
  (run (merge default-lexer {:input s})))

(defn tokenize [s]
  (:tokens (tokenize* s)))

;; /lexer


(defn snippet-atts [snippet]
  (when-let [[_ atts]    (re-find #"<!-- +[A-Z-_]+ (.+) -->" snippet)]
    (let [att-matches (re-seq #"(?x)
                             (?<key>[^=]+)
                             (?:=)
                             (?<quote>\")?
                             (?<val>[^\"]+)(?:\k<quote>?)\ *"
                              atts)]
      (reduce
       (fn [acc [_all k _quote v]]
         (assoc acc k v))
       {} att-matches))))

(defn tag->md-link [base-url tag]
  (let [tag->anchor (fn [t] (-> t str/lower-case
                                (str/replace  #" " "-")
                                (str/replace  #"/" "")))]
    (str "[#" tag "](" base-url (tag->anchor tag) ")")))

(defn inner-tags-block [tags {:keys [tag-base-url]}]
  (str "**Tags:** " (str/join ", " (map #(tag->md-link tag-base-url %) tags))))

(defn blocks [tokens marker]
  (let [snippet?           (comp #(= % :snippet) :t)
        containing-marker? (comp #(re-find (re-pattern (str "<!-- " marker)) %) :v)
        snippet-end?       (comp #(some->> % (re-find (re-pattern (str marker "-END")))) :v)
        snippet-start?     (complement snippet-end?)
        marker-snippets    #(whenp % snippet? containing-marker?)]
    (->> tokens (filter marker-snippets)
         (partition 2 1 (list {}))
         (filter (comp snippet-start? first))
         (reduce (fn [acc [s1 s2]]
                   (let [end (or (some-> s2
                                         (whenp snippet-end?)
                                         :end)
                                 (:end s1))]
                     (conj acc (assoc s1 :end end)))) []))))

(defn complete-blocks
  "Add fillers to cover the total body, i.e. leading&trailing and in-between blocks."
  [blocks body]
  (let [make-filler       #(hash-map :t :filler :start %1 :end %2 :v (subs body %1 %2))
        including-fillers (->> blocks
                               (partition 2 1 (list {}))
                               (reduce (fn [acc [{s1e :end :as s1} {s2s :start :as _s2}]]
                                         (let [item (cond-> (vector s1)
                                                      (and s2s (not= s1e s2s)) (conj (make-filler s1e s2s)))]
                                           (apply conj acc item))) []))
        min-start         (some-> including-fillers
                                  not-empty
                                  (->> (apply min-key :start))
                                  :start
                                  (whenp pos?))
        max-end           (some-> including-fillers
                                  not-empty
                                  (->> (apply max-key :end))
                                  :end
                                  (whenp #(not= % (count body))))
        leading-snippet   (when min-start (make-filler 0 min-start))
        trailing-snippet  (when max-end (make-filler max-end (count body)))]
    (cond-> including-fillers
      leading-snippet  (conj leading-snippet)
      trailing-snippet (conj trailing-snippet)
      :ensure          (->> (sort-by :start)))))

(defn snippet->tags [s]
  (some-> s
          snippet-atts
          not-empty
          (get "tags")
          (str/split #", *")))

(defn expand-tags-snippet [s opts]
  (when-let [tags (snippet->tags s)]
    (str s \newline
         (inner-tags-block tags opts) \newline
         "<!-- POLAR-TAGS-END -->")))

(defn- article-url [{:keys          [slug]
                     {:keys [blog]} :organization}]
  (str blog "/posts/" slug))

(defn expand-tags-list-snippet [s articles-by-tag]
  (let [expand-tag (fn [[tag articles]]
                     (let [a->link      #(str "* [" (:title %) "](" (article-url %) ")" \newline)
                           article-list (str/join \newline (map a->link articles))]
                       (str "## #" tag \newline \newline article-list)))]
    (str s \newline
         \newline
         (str/join \newline (map expand-tag articles-by-tag)) \newline
         \newline
         "<sub>Powered by [eval/actions#polar-tags](https://github.com/eval/actions/blob/main/README.md#polar-tag-posts). Generated at " (java.util.Date.) "</sub>" \newline
         "<!-- POLAR-TAGS-LIST-END -->")))

(defn expand-polar-tags-snippets
  "Expands all '<!-- POLAR-TAGS tags=\"a\" -->' in `body`."
  [body opts]
  (if-let [vals (not-empty (-> (tokenize body)
                               (blocks "POLAR-TAGS")
                               (->> (mapv (fn [b]
                                            (update b :v #(expand-tags-snippet % opts))))
                                    (filter :v))
                               (complete-blocks body)
                               (->> (map :v))))]
    (apply str vals)
    body))

(defn expand-polar-tags-list-snippets
  "Expands all '<!-- POLAR-TAGS-LIST -->' in `body`."
  [body articles-by-tag]
  (if-let [vals (not-empty (-> (tokenize body)
                               (blocks "POLAR-TAGS-LIST")
                               (->> (mapv #(update % :v expand-tags-list-snippet articles-by-tag))
                                    (filter :v))
                               (complete-blocks body)
                               (->> (map :v))))]
    (apply str vals)
    body))

(defn update-article! [{:keys [dry-run] :as _cli-opts} {:keys [title] :as article}]
  #_(prn :cli-opts _cli-opts)
  (let [msg                   (if dry-run "Would update article " "Updating article ")
        msg                   (str msg (pr-str title) " " (article-url article) " ")
        prune-namespaced-keys #(->> % (remove (comp namespace key)) (into {}))]
    (print msg)
    (if dry-run
      (print "...done")
      (pa/put-article (prune-namespaced-keys article)))
    (println)))

(def cli-spec
  {:restrict [:org :help :dry-run]
   :spec     {:org     {:desc    "organization_name (required)"
                        :require true}
              :help    {:coerce :boolean :alias :h}
              :dry-run {:coerce :boolean
                        :desc   "Print articles that would change, don't make changes."}}
   :error-fn (fn [{:keys [opts spec type cause msg option] :as data}]
               (when-not ((some-fn :help :version) opts)
                 (if (= :org.babashka/cli type)
                   (case cause
                     :require
                     (println (format "Missing required argument:\n%s"
                              (cli/format-opts {:spec (select-keys spec [option])})))
                     (println msg))
                   (throw (ex-info msg data)))
                 (System/exit 1)))})

(defn print-help []
  (println
   (str "CLI that...\n 1) replaces `<!-- POLAR-TAGS tags=\"tag1, tag2\" -->` with links to tags-page\n    in all (unpublished) posts" \newline
        " 2) replaces `<!-- POLAR-TAGS-LIST -->` with a list of posts per tag" \newline
        \newline
        "Usage: bb polar-tags.bb [OPTIONS] \n\nOPTIONS\n"
        (cli/format-opts (assoc cli-spec :order [:org :dry-run :help]))
        \newline \newline
        "ENVIRONMENT VARIABLES" \newline
        "  POLAR_API_TOKEN    token from https://polar.sh/settings"
        \newline)))

(defn article->tags [{:keys [body] :as _article}]
  (mapcat (comp snippet->tags :v) (blocks (tokenize body) "POLAR-TAGS")))

(defn tag-base-url [org slug]
  (str "https://polar.sh/" org "/posts/" slug "#"))

(defn expand-tag-snippets! [articles cli-opts]
  (->> articles
       (map (fn [{::keys [tag-base-url] :as article}]
              (let [article-changed? #(not= article %)]
                (whenp (update article :body #(expand-polar-tags-snippets % {:tag-base-url tag-base-url}))
                       (comp seq :body)
                       article-changed?))))
       (remove nil?)
       (mapv (partial update-article! cli-opts))))

(defn articles-by-tag [articles]
  (->> articles
       (map (fn [a]
              (when-let [tags (not-empty (article->tags a))]
                (assoc a :tags tags))))
       (remove nil?)
       (reduce (fn [acc {:keys [tags] :as article}]
                 (merge-with into acc (zipmap tags (repeat [article])))) {})))

(defn update-tag-page! [articles cli-opts]
  (let [articles-by-tag (sort-by (comp str/lower-case key) (articles-by-tag articles))
        tags-article    (-> articles
                            (->> (filter ::tags-article?))
                            first
                            (update :body #(expand-polar-tags-list-snippets % articles-by-tag)))]
    (update-article! cli-opts tags-article)))

(defn tags-article? [{:keys [body]}]
  (seq (blocks (tokenize body) "POLAR-TAGS-LIST")))

(defn org-articles [org]
  (let [articles     (pa/get-articles {:org org :show_unpublished true :limit 100})
        tags-article (first (filter tags-article? articles))]
    (->> articles
         (map (fn [{:keys [published_at] :as article}]
                (assoc article ;; add some app-data
                       ::tag-base-url (tag-base-url org (:slug tags-article))
                       ::published? (some? published_at)
                       ::tags-article? (= article tags-article)))))))

(defn validate-env!
  "Rationale: Having no token won't instantly fail: fetching articles will only get you published items, but an update would eventually fail."
  []
  (when-not (System/getenv "POLAR_API_TOKEN")
    (print-error "Missing env-var POLAR_API_TOKEN")
    (System/exit 1)))

(defn -main [{:keys [org help] :as cli-opts}]
  (if help
    (print-help)
    (do
      (validate-env!)
      (expand-tag-snippets! (org-articles org) cli-opts)
      (update-tag-page! (filter ::published? (org-articles org)) cli-opts))))

(when (= *file* (System/getProperty "babashka.file"))
  (-main (cli/parse-opts *command-line-args* cli-spec)))

(comment
  ;; DONE fail when no env-token
  ;; DONE have command to find page-id
  ;; DONE allow for other user

  (def articles (pa/get-articles {:org "eval" :show_unpublished true :limit 100}))
  (count articles)

  (def articles (org-articles "eval"))
  articles
  (filter tags-article? articles)
  (count articles)
  #_:end)
