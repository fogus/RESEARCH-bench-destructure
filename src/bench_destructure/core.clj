(ns bench-destructure.core)

(defn go [{:keys [kvs iterations]}]
  (let [seed (case kvs 0 () 1 '(:a) (range (* 2 kvs)))
        last-elem  (to-array [nil])]
    (println "using seed" seed)
    (time
     (dotimes [_ iterations]
       (try
         (let [{:as m} seed]
           (aset last-elem 0 m))
         (catch Exception _
           (aset last-elem 0 nil)))))
    (println "iterations: " iterations " of " (aget last-elem 0))))

(defn bench
  [{:keys [kvs iterations] :as opts :or {kvs 2, iterations 20000}}]
  (println "Benchmarking with " opts)
  (println "  Clojure version " *clojure-version*)
  (go (assoc opts :kvs kvs :iterations iterations)))


