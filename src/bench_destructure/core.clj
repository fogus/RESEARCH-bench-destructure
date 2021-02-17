(ns bench-destructure.core)

(defmacro destructure-n [nom n & [wrap massage]]
  (let [vs    (range n)
        ks    (map #(->> % (str "a") keyword) vs)
        names (map (comp symbol name) ks)
        kvs   (interleave ks vs)]
    `(do
       (def ~(->> n (str "kvs") symbol) (quote ~kvs))
       
       (defn ~(-> nom name (str n) symbol) [iterations#]
         (let [~'rhs ~(list* 'list kvs)]
           (loop [acc# 0 n# iterations#]
             (if (< 0 n#)
               (let [{:keys ~(vec names)} ~(if wrap
                                             (if massage
                                               (list wrap (list massage 'rhs))
                                               (list wrap 'rhs))
                                             'rhs)]
                 (recur (+ acc# ~(list* + names))
                        (dec n#)))
               acc#)))))))

(macroexpand-1 '(destructure-n canonical 2))
(macroexpand-1 '(destructure-n phm 2 clojure.core.PersistentHashMap/create seq))
(macroexpand-1 '(destructure-n pam 2 clojure.core.PersistentArrayMap/createAsIfByAssoc to-array))

(destructure-n canonical 2)
(destructure-n canonical 4)
(destructure-n canonical 8)
(destructure-n canonical 16)

(destructure-n pam 2  clojure.lang.PersistentArrayMap/createAsIfByAssoc to-array)
(destructure-n pam 4  clojure.lang.PersistentArrayMap/createAsIfByAssoc to-array)
(destructure-n pam 8  clojure.lang.PersistentArrayMap/createAsIfByAssoc to-array)
(destructure-n pam 16 clojure.lang.PersistentArrayMap/createAsIfByAssoc to-array)

(destructure-n phm 2  clojure.lang.PersistentHashMap/create seq)
(destructure-n phm 4  clojure.lang.PersistentHashMap/create seq)
(destructure-n phm 8  clojure.lang.PersistentHashMap/create seq)
(destructure-n phm 16 clojure.lang.PersistentHashMap/create seq)

(defn exec [iterations fun msg]
  (println msg)
  (dotimes [_ 11]
    (time (fun iterations))))

(defn loop-phm [iterations data]
  (let [data (to-array data)]
    (loop [acc 0 n iterations]
      (if (< 0 n)
        (let [res (clojure.lang.PersistentHashMap/create data)]
          (recur (+ acc (count res))
                 (dec n)))
        acc))))

(defn time-pam [iterations]
  (exec iterations #(loop-phm % kvs2)  "PAM/createAsIfByAssoc DIRECT-2")
  (exec iterations #(loop-phm % kvs4)  "PAM/createAsIfByAssoc DIRECT-4")
  (exec iterations #(loop-phm % kvs8)  "PAM/createAsIfByAssoc DIRECT-8")
  (exec iterations #(loop-phm % kvs16) "PAM/createAsIfByAssoc DIRECT-16"))

(defn time-pam-destr [iterations]
  (exec iterations pam2  "PAM destructure-2")
  (exec iterations pam4  "PAM destructure-4")
  (exec iterations pam8  "PAM destructure-8")
  (exec iterations pam16 "PAM destructure-16"))

(defn time-phm [iterations]
  (exec iterations phm2  "PHM/create-2")
  (exec iterations phm4  "PHM/create-4")
  (exec iterations phm8  "PHM/create-8")
  (exec iterations phm16 "PHM/create-16"))

(defn time-canonical [iterations]
  (exec iterations canonical2  "canonical-2")
  (exec iterations canonical4  "canonical-4")
  (exec iterations canonical8  "canonical-8")
  (exec iterations canonical16 "canonical-16"))

(defn bench
  [{:keys [iterations] :as opts :or {iterations 50000}}]
  (println "Benchmarking with " iterations "iterations.")
  (println "  Clojure version " *clojure-version*)

  (println "\nPAM in destructure cxt\n====================")
  (time-pam-destr iterations)

  (println "\nPAM direct\n===")
  (time-pam iterations))


(comment

    (exec 50000 pam2  "pam-2")

)
