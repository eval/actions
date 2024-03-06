(ns polar.api
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]))


(defn get-articles [{:keys [org show_unpublished] :as _cli-opts}]
  {:pre [(some? org)]}
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
