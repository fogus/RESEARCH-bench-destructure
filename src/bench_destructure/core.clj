(ns bench-destructure.core)

(defmacro destructure-n [n]
  (let [vs    (range n)
        ks    (map #(->> % (str "a") keyword) vs)
        names (map (comp symbol name) ks)
        kvs   (interleave ks vs)]
    `(let [{:keys ~(vec names)} ~(list* list kvs)]
       ~(list* + names))))

(defn bench
  [{:keys [iterations] :as opts :or {iterations 20000}}]
  (println "Benchmarking with " iterations "iterations.")
  (println "  Clojure version " *clojure-version*)

  (println "destructure-2")
  (time
   (dotimes [_ iterations]
     (destructure-n 2)))
  
  (println "destructure-4")
  (time
   (dotimes [_ iterations]
     (destructure-n 4)))

  (println "destructure-8")
  (time
   (dotimes [_ iterations]
     (destructure-n 8))))


