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
             (if (pos? n#)
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
  (loop [res nil n 11]
    (if (pos? n)
      (recur (time (fun iterations)) (dec n))
      res)))

(defn loop-pam [iterations data]
  (let [data (to-array data)]
    (loop [acc 0 n iterations]
      (if (pos? n)
        (let [res (clojure.lang.PersistentArrayMap/createAsIfByAssoc data)]
          (recur (+ acc (count res))
                 (dec n)))
        acc))))

(defn loop-pam-destr-array-seq [iterations data]
  (let [data (clojure.lang.ArraySeq/create (to-array data))]
    (loop [acc 0 n iterations]
      (if (pos? n)
        (let [res (clojure.lang.PersistentArrayMap/createAsIfByAssoc (to-array data))]
          (recur (+ acc (count res))
                 (dec n)))
        acc))))

(defn loop-to-array [iterations data]
  (let [data (clojure.lang.ArraySeq/create (to-array data))]
    (loop [acc 0 n iterations]
      (if (pos? n)
        (let [res (to-array data)]
          (recur (+ acc (alength res))
                 (dec n)))
        acc))))

(defn loop-phm [iterations data]
  (let [data (seq data)]
    (loop [acc 0 n iterations]
      (if (pos? n)
        (let [res (clojure.lang.PersistentHashMap/create data)]
          (recur (+ acc (count res))
                 (dec n)))
        acc))))

(defn time-pam [iterations]
  (exec iterations #(loop-pam % kvs2)  "PAM/createAsIfByAssoc DIRECT-2")
  (exec iterations #(loop-pam % kvs4)  "PAM/createAsIfByAssoc DIRECT-4")
  (exec iterations #(loop-pam % kvs8)  "PAM/createAsIfByAssoc DIRECT-8")
  (exec iterations #(loop-pam % kvs16) "PAM/createAsIfByAssoc DIRECT-16"))

(defn time-to-array [iterations]
  (exec iterations #(loop-to-array % kvs2)  "TA-2")
  (exec iterations #(loop-to-array % kvs4)  "TA-4")
  (exec iterations #(loop-to-array % kvs8)  "TA-8")
  (exec iterations #(loop-to-array % kvs16) "TA-16"))

(defn time-pam-destr [iterations]
  (exec iterations pam2  "PAM destructure-2")
  (exec iterations pam4  "PAM destructure-4")
  (exec iterations pam8  "PAM destructure-8")
  (exec iterations pam16 "PAM destructure-16"))

(defn time-pam-destr-array-seq [iterations]
  (exec iterations #(loop-pam-destr-array-seq % kvs2)  "PAM/caiba, into-array, ArraySeq 2")
  (exec iterations #(loop-pam-destr-array-seq % kvs4)  "PAM/caiba, into-array, ArraySeq 4")
  (exec iterations #(loop-pam-destr-array-seq % kvs8)  "PAM/caiba, into-array, ArraySeq 8")
  (exec iterations #(loop-pam-destr-array-seq % kvs16) "PAM/caiba, into-array, ArraySeq 16"))

(defn time-phm [iterations]
  (exec iterations #(loop-phm % kvs2)  "PHM/create DIRECT-2")
  (exec iterations #(loop-phm % kvs4)  "PHM/create DIRECT-4")
  (exec iterations #(loop-phm % kvs8)  "PHM/create DIRECT-8")
  (exec iterations #(loop-phm % kvs16) "PHM/create DIRECT-16"))

(defn time-phm-destr [iterations]
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
  (time-pam-destr-array-seq iterations)

  (comment
    (println "\nPHM/create in destructure context\n====================")
    (time-phm-destr iterations)

    (println "\nPHM direct\n===")
    (time-phm iterations)

    (println "\nPAM in destructure cxt (to-array list)\n==========================")
    (time-pam-destr iterations)

    (println "\nto-array\n=========")
    (time-to-array iterations)

    (println "\nPAM direct\n===========")
    (time-pam iterations)))


(comment
  (exec 50000 #(loop-pam % kvs16)  "PAM/createAsIfByAssoc DIRECT-16")
  
  (exec 50000 pam2  "pam-2")

  (exec 20000 #(loop-to-array % kvs2)  "TA-2")
)
