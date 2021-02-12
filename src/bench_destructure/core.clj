(ns bench-destructure.core)

(defmacro destructure-n [n iterations & [wrap massage]]
  (let [vs    (range n)
        ks    (map #(->> % (str "a") keyword) vs)
        names (map (comp symbol name) ks)
        kvs   (interleave ks vs)]
    `(time
      (dotimes [x# ~iterations]
        (let [{:keys ~(vec names)} ~(if wrap
                                      (if massage
                                        (list wrap (list massage (list* 'list kvs)))
                                        (list wrap (list* 'list kvs)))
                                      (list* 'list kvs))]
          ~(list* + names))))))

(defn bench-destr [iterations]
  (println "destructure-2")
  (destructure-n 2 iterations)

  (println "destructure-4")
  (destructure-n 4 iterations)

  (println "destructure-8")
  (destructure-n 8 iterations))

(defn bench-phm [iterations]
  (println "PHM.create 2")
  (destructure-n 2 iterations clojure.lang.PersistentHashMap/create seq)

  (println "PHM.create 4")
  (destructure-n 4 iterations clojure.lang.PersistentHashMap/create seq)
  
  (println "PHM.create 8")
  (destructure-n 8 iterations clojure.lang.PersistentHashMap/create seq))

(defn bench-pam [iterations]
  (println "PAM.createAsIfByAssoc 2")
  (destructure-n 2 iterations clojure.lang.PersistentArrayMap/createAsIfByAssoc to-array)

  (println "PAM.createAsIfByAssoc 4")
  (destructure-n 4 iterations clojure.lang.PersistentArrayMap/createAsIfByAssoc to-array)

  (println "PAM.createAsIfByAssoc 8")
  (destructure-n 8 iterations clojure.lang.PersistentArrayMap/createAsIfByAssoc to-array))

(defn bench
  [{:keys [iterations] :as opts :or {iterations 20000}}]
  (println "Benchmarking with " iterations "iterations.")
  (println "  Clojure version " *clojure-version*)

  (bench-destr iterations)

  (println)

  (bench-phm iterations)

  (println)

  (bench-pam iterations))
