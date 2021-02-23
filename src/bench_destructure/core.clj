(ns bench-destructure.core)

(defmacro destructure-n
  "Generates a function taking a loop count and a data structure that
  will be used in the right-hand-side of a map destructure form. Two optional
  functions may be passed one that takes the data structure and returns another
  and one that builds the map used as the input to the binding form. Additionally,
  this macro will build the input data structure and define it in the current
  namespace."
  [nom n & [wrap massage]]
  (let [vs    (range n)
        ks    (map #(->> % (str "a") keyword) vs)
        names (map (comp symbol name) ks)
        kvs   (interleave ks vs)]
    `(do
       (def ~(->> n (str "kvs") symbol) (quote ~kvs))
       
       (defn ~(-> nom name (str n) symbol) [iterations# ~'rhs]
         (loop [acc# 0 n# iterations#]
           (if (pos? n#)
             (let [{:keys ~(vec names)} ~(if wrap
                                           (if massage
                                             (list wrap (list massage 'rhs))
                                             (list wrap 'rhs))
                                           'rhs)]
               (recur (+ acc# ~(list* + names))
                      (dec n#)))
             acc#))))))

(comment
  ;; calling with no processing functions
  
  (macroexpand '(destructure-n canonical 2))

  (defn canonical2 [iterations rhs]
    (loop [acc 0 n iterations]
      (if (pos? n)
        (let [{:keys [a0 a1]} rhs]
          (recur (+ acc (+ a0 a1))
                 (dec n)))
        acc)))

  ;; calling with a static method call and a call to seq
  
  (macroexpand-1 '(destructure-n phm 2 clojure.core.PersistentHashMap/create seq))

  (defn phm2 [iterations rhs]
    (loop [acc 0 n iterations]
      (if (pos? n)
        (let [{:keys [a0 a1]} (clojure.core.PersistentHashMap/create (seq rhs))]
          (recur (+ acc (+ a0 a1))
                 (dec n)))
        acc)))

  ;; calling with a static method call and convertor to array
  
  (macroexpand-1 '(destructure-n pam 2 clojure.core.PersistentArrayMap/createAsIfByAssoc to-array))

  (defn pam2 [iterations rhs]
    (loop [acc 0 n iterations]
      (if (pos? n)
        (let [{:keys [a0 a1]} (clojure.core.PersistentArrayMap/create (to-array rhs))]
          (recur (+ acc (+ a0 a1))
                 (dec n)))
        acc)))
  )

(destructure-n pam-direct 2  clojure.lang.PersistentArrayMap/createAsIfByAssoc to-array)
(destructure-n pam-direct 4  clojure.lang.PersistentArrayMap/createAsIfByAssoc to-array)
(destructure-n pam-direct 8  clojure.lang.PersistentArrayMap/createAsIfByAssoc to-array)
(destructure-n pam-direct 16 clojure.lang.PersistentArrayMap/createAsIfByAssoc to-array)

(destructure-n destr 2)
(destructure-n destr 4)
(destructure-n destr 8)
(destructure-n destr 16)

(destructure-n phm-direct 2  clojure.lang.PersistentHashMap/create seq)
(destructure-n phm-direct 4  clojure.lang.PersistentHashMap/create seq)
(destructure-n phm-direct 8  clojure.lang.PersistentHashMap/create seq)
(destructure-n phm-direct 16 clojure.lang.PersistentHashMap/create seq)

(destructure-n phm-destr 2  seq)
(destructure-n phm-destr 4  seq)
(destructure-n phm-destr 8  seq)
(destructure-n phm-destr 16 seq)

(defn exec [iterations fun data msg]
  (println msg)
  (loop [res nil n 11]
    (if (pos? n)
      (recur (time (fun iterations data)) (dec n))
      res)))

(defn as [data]
  (clojure.lang.ArraySeq/create (to-array data)))

(defn loop-to-array [iterations data]
  (let [data (clojure.lang.ArraySeq/create (to-array data))]
    (loop [acc 0 n iterations]
      (if (pos? n)
        (let [res (to-array data)]
          (recur (+ acc (alength res))
                 (dec n)))
        acc))))

(defn time-to-array [iterations]
  (exec iterations loop-to-array kvs2  "TA-2")
  (exec iterations loop-to-array kvs4  "TA-4")
  (exec iterations loop-to-array kvs8  "TA-8")
  (exec iterations loop-to-array kvs16 "TA-16"))

(defn time-destr [iterations]
  (exec iterations destr2  (as kvs2)  "destructure context-2")
  (exec iterations destr4  (as kvs4)  "destructure context-4")
  (exec iterations destr8  (as kvs8)  "destructure context-8")
  (exec iterations destr16 (as kvs16) "destructure context-16"))

(defn time-pam-direct [iterations]
  (exec iterations pam-direct2  (as kvs2)  "PAM/caiba destructure-2")
  (exec iterations pam-direct4  (as kvs4)  "PAM/caiba destructure-4")
  (exec iterations pam-direct8  (as kvs8)  "PAM/caiba destructure-8")
  (exec iterations pam-direct16 (as kvs16) "PAM/caiba destructure-16"))

(defn time-phm-direct [iterations]
  (exec iterations phm-direct2  (as kvs2)  "PHM/create destructure-2")
  (exec iterations phm-direct4  (as kvs4)  "PHM/create destructure-4")
  (exec iterations phm-direct8  (as kvs8)  "PHM/create destructure-8")
  (exec iterations phm-direct16 (as kvs16) "PHM/create destructure-16"))

(defn bench
  [{:keys [iterations] :as opts :or {iterations 50000}}]
  (println "Benchmarking with " iterations "iterations.")
  (println "  Clojure version " *clojure-version*)

  (if (= *clojure-version* {:major 1, :minor 10, :incremental 2, :qualifier nil})
    (do
      (println "\nto-array of ArraySeq\n===")
      (time-to-array iterations)

      (println "\nPHM direct\n====================")
      (time-phm-direct iterations)

      (println "\nPHM destructure context\n===")
      (time-destr iterations))
    (do
      (println "\nto-array of ArraySeq\n===")
      (time-to-array iterations)

      (println "\nPAM direct\n===")
      (time-pam-direct iterations)
      
      (println "\nPAM destructure context\n===")
      (time-destr iterations)))
)


(comment
  (pam-destr2 100 (as kvs2))
  
  (exec 50000 #(loop-pam % kvs16)  "PAM/createAsIfByAssoc DIRECT-16")
  
  (exec 50000 pam2  "pam-2")

  (exec 20000 #(loop-to-array % kvs2)  "TA-2")
)
