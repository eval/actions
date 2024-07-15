(ns polar.api
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]))


(defn get-articles [{:keys [org limit] :or {limit 10} :as _cli-opts}]
  {:pre [(some? org)]}
  (-> "https://api.polar.sh/api/v1/articles"
      (http/get {:query-params {:is_published    true
                                :organization_id org
                                :limit           limit}
                 :headers      {"Authorization" (str "Bearer " (System/getenv "POLAR_API_TOKEN"))}})
      :body
      (json/parse-string true)
      :items))

(defn put-article [{:keys [id body]}]
  (http/patch (str "https://api.polar.sh/api/v1/articles/" id)
              {:body    (json/encode {"body" body})
               :headers {"Authorization" (str "Bearer " (System/getenv "POLAR_API_TOKEN"))}}))
