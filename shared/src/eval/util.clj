(ns eval.util)

(defn whenp
  "Yields `v` when `v` is truthy as well as `(pred v)` for all `preds`.

  Examples:
  (whenp 1 odd? pos?) ;; => 1
  (whenp coll seq) ;; =>  nil or a collection with at least 1 item"
  ([v] v)
  ([v & preds]
   (when (and v ((apply every-pred preds) v))
     v)))

(defn print-error [& msgs]
  (binding [*out* *err*]
    (apply println msgs)))
