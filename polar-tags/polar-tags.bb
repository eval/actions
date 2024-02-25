#!/usr/bin/env bb

(require '[babashka.http-client :as http]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[babashka.cli :as cli])

;; Polar API
;;
(defn get-articles [{:keys [org show_unpublished] :as _cli-opts}]
  (-> "https://api.polar.sh/api/v1/articles/search"
      (http/get {:query-params {:show_unpublished  (boolean show_unpublished)
                                :organization_name org
                                :platform          "github"}
                 :headers      {"Authorization" (str "Bearer " (System/getenv "POLAR_API_TOKEN"))}})
      :body
      (json/parse-string true)
      :items))

(defn put-article [{:keys [id body]}]
  (http/put (str "https://api.polar.sh/api/v1/articles/" id)
            {:body    (json/encode {"body" body})
             :headers {"Authorization" (str "Bearer " (System/getenv "POLAR_API_TOKEN"))}}))

;; helpers
;;
(defn whenp [v & preds]
  (when ((apply every-pred preds) v)
    v))


(letfn [(camel->kebab [s]
          (-> s
              (str/replace #"[A-Z]" (comp #(str "-" %) str/lower-case))
              (str/replace #"^-|-$" "")))]
  (defn re-named-captures
    "Yields captures by group name.

    Examples:
    (re-named-captures #\"(?<foo>Foo)\" \"Foo\") ;; => {\"foo\" \"Foo\"}
  "
    ([re s] (re-named-captures re s nil))
    ([re s {:keys [keywordize] :or {keywordize identity}}]
     (let [keywordize-fn (if (true? keywordize) (comp keyword camel->kebab) keywordize)]
       (when-let [matching (whenp (re-matcher re s) re-find)]
         (reduce (fn [acc [group _]]
                   (if-let [captured (.group ^java.util.regex.Matcher matching group)]
                     (assoc acc (keywordize-fn group) captured)
                     acc))
                 {} (.namedGroups re)))))))

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

(comment
  (re-seq #"(?x)
                             (?<key>[^=]+)
                             (?:=)
                             (?<quote>\")?
                             (?<val>[^\"]+)(?:\k<quote>?)\ *"
          #_"tags=\"Clojure, TIL, Babashka\"  alles=1"
          (second (re-find #"<!-- +[A-Z-_]+ (.+) -->" "<!-- POLAR-TAGS tags=\"Clojure, TIL, Babashka\" alles=1 -->"))))

(defn body-parts [{:keys [body]} marker]
  (let [snippet-re (str "<!--\\ (?<marker>" marker ")\\ [^-]*-->")]
    (when (re-find (re-pattern snippet-re) body)
      (re-named-captures
       (re-pattern (str "(?sx) # '<before><!-- marker --><middle><!-- marker-END --><after>'
                     (?<before>.*)
                     (?<snippet>             # capture '<!-- POLAR-TAGS tags=\"git,Clojure\" -->'
                       (?<!```[^\\n]*\\n)  # unless preseded by codeblock-markers
                       <!--\\ (?<marker>" marker ")\\ [^-]*-->)
                     (?:
                       (?<middle>.*) # absent when snippet just inserted
                       <!--\\ " marker "-END\\ -->)?
                     (?<after>.*)
                    ")) body {:keywordize true}))))

(defn inner-tags-block [tags]
  (let [tag-anchor   (fn [t] (-> t str/lower-case (str/replace  #" " "-")))
        tag->md-link #(str "[#" % "](https://polar.sh/eval/posts/articles-by-tag#" (tag-anchor %) ")")]
    (str "**Tags:** " (str/join ", " (map tag->md-link tags)))))

(defn insert-block [{:keys [before snippet after marker] :as _parts} inner-block]
  (let [close-snippet (str "<!-- " marker "-END -->")]
    (str before \newline
         snippet \newline
         \newline
         inner-block \newline
         \newline
         close-snippet after)))

(defn insert-tags-block [article]
  (when-let [{:keys [snippet] :as parts} (body-parts article "POLAR-TAGS")]
    (when-let [tags-str (not-empty (get (snippet-atts snippet) "tags"))]
      (let [tags (str/split tags-str #" *, *")]
        (assoc article :body (insert-block parts (inner-tags-block tags)))))))


;; lexer
(def code-fences "```")
(def snippet-open "<!--")
(def snippet-close "-->")
#_(def variable-end "}}")
#_(def block-start "{%")
#_(def block-end "%}")
#_(def argument-div ":")
#_(def pipe "|")


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
    (eos? lexer)                     (-> lexer (emit-token :text) (dissoc :state))
    (coming-up? lexer snippet-close) (-> lexer
                                         (read-char (count snippet-close))
                                         (emit-token :snippet)
                                         (assoc-state :return))
    :else                            (read-char lexer)))

(defn lex-code [lexer]
  (cond
    (eos? lexer)                   (-> lexer (emit-token :text) (dissoc :state))
    (coming-up? lexer code-fences) (-> lexer (read-char (count code-fences)) (emit-token :code) (assoc-state :return))
    :else (read-char lexer)))

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
                               (reduce (fn [acc [{s1e :end :as s1} {s2s :start :as s2}]]
                                         (let [item (cond-> (vector s1)
                                                      (and s2s (not= s1e s2s)) (conj (make-filler s1e s2s)))]
                                           (apply conj acc item))) []))
        min-start         (whenp (some->> including-fillers
                                          not-empty
                                          (apply min-key :start)
                                          :start)
                                 some?
                                 pos?)
        max-end           (whenp (some->> including-fillers
                                          not-empty
                                          (apply max-key :end)
                                          :end)
                                 some?
                                 #(not= % (count body)))
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

(defn expand-tags-snippet [s]
  (when-let [tags (snippet->tags s)]
    (str s \newline
         (inner-tags-block tags) \newline
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
         "<sub>Updated at " (java.util.Date.) "</sub>" \newline
         "<!-- POLAR-TAGS-LIST-END -->")))

(defn expand-polar-tags-snippets
  "Expands all '<!-- POLAR-TAGS tags=\"a\" -->' in `body`.
  Yields `nil` if there's nothing to expand."
  [body]
  (if-let [vals (not-empty (-> (tokenize body)
                               (blocks "POLAR-TAGS")
                               (->> (mapv #(update % :v expand-tags-snippet))
                                    (filter :v))
                               (complete-blocks body)
                               (->> (map :v))))]
    (apply str vals)
    body))

(defn expand-polar-tags-list-snippets
  "Expands all '<!-- POLAR-TAGS tags=\"a\" -->' in `body`.
  Yields `nil` if there's nothing to expand."
  [body articles-by-tag]
  (if-let [vals (not-empty (-> (tokenize body)
                               (blocks "POLAR-TAGS-LIST")
                               (->> (mapv #(update % :v expand-tags-list-snippet articles-by-tag))
                                    (filter :v))
                               (complete-blocks body)
                               (->> (map :v))))]
    (apply str vals)
    body))

#_(println (expand-polar-tags-snippets (:body (last arts-after))))
#_(defn expand-article-tags [article])

(comment
  (def arts (get-articles {:org "eval"}))

  (let [article (nth arts 2)]
    (println (expand-polar-tags-list-snippets (:body article) (articles-by-tag arts))))

  (blocks (tokenize "Some introduction here
<!-- POLAR-TAGS-LIST -->") "POLAR-TAGS-LIST")

  (expand-polar-tags-list-snippets "Some introduction here
<!-- POLAR-TAGS-LIST -->" {"Clojure" [{:title "Article 1"}]})
  (complete-blocks [{:start 0, :t :snippet, :v "<!-- POLAR-TAGS tags=\"foo\" -->", :end 30} {:start 31, :t :snippet, :v "<!-- POLAR-TAGS tags=\"bar\" -->", :end 60}])
  ;; snippet:
  ;; [{:pos 0}]
  ;; block-positions

  (max-key :p [{:p 3} {:p 2}])

  (let [body "Hello! ```clojure
<!-- POLAR-TAGS tags=\"Clojure, babashka\" a=\"moar\" -->
```
<!-- POLAR-TAGS tags=\"Clojure, babashka\" a=\"moar\" -->
Tags: link1, link2
<!-- POLAR-TAGS-END -->

<!-- POLAR-TAGS tags=\"foo\" -->

Even more here
"
        body         "Hello!<!-- POLAR-TAGS tags=\"foo,bar\" --> <!-- POLAR-TAGS tags=\"bar\" -->"
        #_#_body "Hello!<!-- POLAR-TAGS tags=\"foo,bar\" -->\nTags: [#foo](https://polar.sh/eval/posts/articles-by-tag#foo), [#bar](https://polar.sh/eval/posts/articles-by-tag#bar)\n<!-- POLAR-TAGS-END --> <!-- POLAR-TAGS tags=\"bar\" -->\nTags: [#bar](https://polar.sh/eval/posts/articles-by-tag#bar)\n<!-- POLAR-TAGS-END -->"
        #_#_body "<!-- POLAR-TAGS tags=\"foo,bar\" -->\nTags: [#foo](https://polar.sh/eval/posts/articles-by-tag#foo), [#bar](https://polar.sh/eval/posts/articles-by-tag#bar)\n<!-- POLAR-TAGS-END -->"
        body "Hello"
        blocks           (blocks (:tokens (run (merge default-lexer {:input body}))) "POLAR-TAGS")
        snippets         (complete-blocks (filter :v (mapv #(update % :v expand-tags-snippet) blocks))
                                          body)
        print-all        #(->> %
                               (map :v)
                               (apply str)
                               #_println)
        #_#_leading-snippet  (let [min-start (:start (apply min-key :start snippets))]
                               {:v (subs body 0 min-start) :start 0 :end min-start})
        #_#_trailing-snippet (let [max-end (:end (apply max-key :end snippets))]
                               {:v (subs body max-end) :start max-end :end (count body)})
        #_#_body-snippets    (sort-by :start (into [leading-snippet trailing-snippet] snippets))]
    #_(conj (cons {:v (subs body 0 (:start (first (min-key :start positions))))} positions)
            {:v (subs body (:start (first (max-key :end positions))))})
    #_(println "----- Body ----")
    #_(println body)
    #_(println "----- Result ----")
    #_(print-all snippets)

    (apply min-key :v [{}])
    blocks
    #_(= body (print-all snippets))
    #_blocks
    #_(complete-blocks blocks body)
    #_trailing-snippet
    #_(apply max-key :end snippets)
    #_(subs body 0 32)
    #_(reduce (fn [{:keys [from to]}]
                (subs)) body positions))

  (def bl *1)
  (blocks bl)
  ((comp #(some->> % (re-find (re-pattern (str "POLAR-TAGS-END")))) :v) (last bl))
  (update {:start 32, :t :snippet, :v "<!-- POLAR-TAGS tags=\"a,b c\" -->", :end 119} :v expand-tags-snippet)

  ;; lexer
  ;; states
  ;; - text
  ;; - code
  ;; - snippet
  ;; - text

  ;; glossary
  ;; (open)snippet  <!-- POLAR-TAGS tags="foo, bar" -->
  ;; closing snippet <!-- POLAR-TAGS-END -->
  ;; (tags)block
  ;; <!-- POLAR-TAGS -->
  ;; Tags: 1, 2
  ;; <!-- POLAR-TAGS-END -->

  (body-insert {"before" "hello" "start" "<!--"})
  ;; fetch 10 articles
  ;;
  ;;

  (put-article (insert-tags-block art))
  ;; gegeven een aantal tags
  ;;

  (tags-snippet ["git"])
  (body-parts  "POLAR-TAGS")

  (snippet-atts "<!-- POLAR-TAGS -->")
  (def art *1)

  (let [article (second arts)]
    (when-let [parts (body-parts article "POLAR-TAGS")]
      (assoc article :body (body-insert parts "hello"))))

  (println (body-insert (body-parts {:body (body-insert {:marker "POLAR-TAGS", :start "<!-- POLAR-TAGS tags=\"Clojure, TIL, Babashka\" -->", :after "", :before "```html\n<!-- POLAR-TAGS tags=\"Clojure, TIL, Babashka\" -->\n```\n\n<hr />\n"} (tags-snippet ["git"]))} "POLAR-TAGS") (tags-snippet ["Clojure" "git"])))

  (body-parts {:body "\n<!-- POLAR-TAGS-PAGE -->\n\n## #Clojure\n\n* [TIL #Clojure :reload-ing when :require-ing](https://polar.sh/eval/posts/til-clojure-reload-ing-when-require-ing) \n* [Let's write a templating library 🔎 Part 1: lexing](https://polar.sh/eval/posts/lets-write-a-templating-library-part-1-lexing)\n\n## #TIL\n\n* [TIL #Clojure :reload-ing when :require-ing](https://polar.sh/eval/posts/til-clojure-reload-ing-when-require-ing) \n\n<sub>Powered by...</sub>\n\n<!-- POLAR-TAGS-PAGE-END -->"} "POLAR-TAGS-PAGE")

  (tags-snippet ["foo" "Rich Hickey"])

  (replace-comment {:body "foobar\n<!-- POLAR-TAGS tags=\"git, \" a=\"1\" -->
This will be replaced!
<!-- POLAR-TAGS-END -->"} "POLAR-TAGS")

  (str/replace "foobar\n<!-- POLAR-TAGS tags=\"git, \" a=\"1\" -->
This will be replaced!
<!-- POLAR-TAGS-END -->" #"(<!-- POLAR-TAGS.+-->).+(<!-- POLAR-TAGS-END -->)" "$1 replace $2")

  (let [marker "POLAR-TAGS"
        {:keys [pre open middle post] :as match}
        (re-named-captures
         (re-pattern
          (str "(?sx)(?<pre>.*)(?<open>(?<!```[^\\n]*\\n)<!--\\ "
               marker
               "\\ [^-]+-->)(?:(?<middle>.*)<!--\\ "
               marker "-END\\ -->)?(?<post>.*)")
          #_(str "(?s)^(.*)((?<!```[^\n]*\n)<!-- "
                 marker
                 " [^-]+-->(?:(.*)<!-- "
                 marker
                 "-END -->)?(.+)"
                 #_"(?s)^(.*)((?<!```[^\n]*\n)<!-- POLAR-TAGS .+-->)([^<]*)(<!-- POLAR-TAGS-END -->)"))
         #_"(?sx)(?<pre>.*)(?<open>(?<!```[^\n]*\n)<!--\\ POLAR-TAGS\\ [^-]+-->)(?:(?<middle>.*)<!--\\ POLAR-TAGS-END\\ -->)?(?<post>.+)"
         #_(re-pattern "(?sx)(?<pre>.*)(?<open>(?<!```[^\\n]*\\n)<!--\\ POLAR-TAGS\\ [^-]+-->)(?:(?<middle>.*)<!--\\ POLAR-TAGS-END\\ -->)?(?<post>.+)")
         #_(re-pattern (str "POLAR-TAGS\\ "))
         #_(re-pattern "(?sx)(?<pre>.*)(?<open>(?<!```[^\\n]*\\n))")
         #_"\n\n<!-- POLAR-TAGS tags=\"Clojure, TIL, Babashka\" -->\n\nTags: [#TIL](https://polar.sh/eval/posts/articles-by-tag#til), [#Clojure](https://polar.sh/eval/posts/articles-by-tag#clojure)\n\n<!-- POLAR-TAGS-END -->"
         "<paywall>\n
```
<!-- POLAR-TAGS tags=\"git, \" a=\"1\" -->
Tags:
<!-- POLAR-TAGS-END -->
```

<!-- POLAR-TAGS tags=\"some,real,tag\" -->
  To be replaced
<!-- POLAR-TAGS-END -->")]
    match) (keys *1)

  (body-parts (count arts) "POLAR-TAGS")

  (def arts (articles {:org "eval"}))

  (:title (second arts))
  (body-parts (:body (first arts)) "POLAR-TAGS-PAGE")

  (second (re-find (re-pattern (str "(?s)^(.*)(<!-- POLAR-TAGS.+-->)([^<]*)(<!-- POLAR-TAGS-END -->)"))
                   (keys (body-parts (second arts) "POLAR-TAGS"))))

  (get-article)

  (re-find #"(?<quote>\"\")")

  (re-find #"(?<quote>\")?.+(\k<quote>?)" "foo")
  (str/split "foo, bar" #",")

  (update (article-comment-attributes {:body "foobar\n<!-- POLAR-TAGS tags=\"git, \" a=\"1\" --> "} "POLAR-TAGS")
          "tags" #(str/split % #", *"))

  (article-polar-tags {:body ""})
  (re-named-captures #"(?x)
(?:\ *)
(?<key>[^=]+)
(?:=)
(?<quote>\")?(?<val>[^\ \"]+)(?:\k<quote>)?" "a=1 foo=bar baz=\"a.b\"")

  (re-seq #"(?x)
(?<key>[^\r\n\t\f\v= '\"]+)  # capture key
(?:=      # literal =
  (?<quote>[\"'])?  # \"
  ((?:.
    (?!\k<quote>?\s+(?:\S+)=|\k<quote>))+.)(?:\k<quote>?))?" "hello=\"foo\" other=moar c=d cd=de")

  (re-find #"[\w-]+(?:=\" .+? \"|\b)(?!<)(?=.*?>)" "a=\"a\"")

  (re-find #"(?:=([\"])?())" "a=\"a\"")

  #_:end)

(defn update-article! [{:keys [dry-run] :as _cli-opts} {:keys [title] :as article}]
  (let [msg (if dry-run "Would update article " "Updating article ")
        msg (str msg (pr-str title) " " (article-url article) " ")]
    (print msg)
    (when-not dry-run
      (put-article article)
      (print "...done"))
    (println)))

(def cli-spec
  {:restrict [:org :help :dry-run :tags-page-id]
   :spec     {:tags-page-id {:desc    "page id of tag listing page"
                             :require true}
              :org          {:desc    "organization_name (required)"
                             :require true}
              :help         {:coerce :boolean :alias :h}
              :dry-run      {:coerce :boolean
                             :desc   "Print candidate articles, don't make changes."}}
   :error-fn (fn [{:keys [opts spec type cause msg option] :as data}]
               (when-not ((some-fn :help :version) opts)
                 (if (= :org.babashka/cli type)
                   (case cause
                     :require
                     (println
                      (format "Missing required argument:\n%s"
                              (cli/format-opts {:spec (select-keys spec [option])})))
                     (println msg))
                   (throw (ex-info msg data)))
                 (System/exit 1)))})

(defn print-help []
  (println
   (str "Usage: polar-tags [OPTIONS] \n\nOPTIONS\n"
        (cli/format-opts (assoc cli-spec :order [:org :days-since-publish :dry-run :help]))
        \newline \newline
        "ENVIRONMENT VARIABLES" \newline
        "  POLAR_API_TOKEN    token from https://polar.sh/settings"
        \newline)))

(defn article->tags [{:keys [body] :as _article}]
  (mapcat (comp snippet->tags :v) (blocks (tokenize body) "POLAR-TAGS")))


(defn expand-tag-snippets! [articles cli-opts]
  (->> articles
       (map (fn [article]
              (let [article-changed? #(not= article %)]
                (whenp (update article :body expand-polar-tags-snippets)
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

(defn article-by-id [articles id]
  (first (filter (comp #(= id %) :id) articles)))

(defn update-tag-page! [articles {:keys [tags-page-id] :as cli-opts}]
  (let [articles-by-tag (sort-by (comp str/lower-case key) (articles-by-tag articles))
        tags-article    (-> (article-by-id articles tags-page-id)
                         (update :body #(expand-polar-tags-list-snippets % articles-by-tag)))]
    (update-article! cli-opts tags-article)))

(defn -main [{:keys [org help] :as args}]
  (if help
    (print-help)
    (let [articles (get-articles {:org org :show_unpublished true})]
      (expand-tag-snippets! articles args)
      (update-tag-page! (filter :published_at articles) args))))

(when (= *file* (System/getProperty "babashka.file"))
  (-main (cli/parse-opts *command-line-args* cli-spec)))


(comment
  (def arts-after (get-articles {:org "eval"}))

  (last arts-after)
  (some-> arts
      first
      insert-tags-block
      put-article)
  (second arts)
  (body-parts (first arts) "POLAR-TAGS")
  (map #(body-parts % "POLAR-TAGS") arts)

  (cli/parse-opts '("-h")  cli-spec)
  (cli/parse-opts '() {:spec {:help (get-in cli-spec [:spec :help])}})
  (put-article {:id "7891ada6-f2d0-46f7-b217-0719e784ecbc" :body "Hello"})

  #_:end)
