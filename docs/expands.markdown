Expansion of `(destructure-n pam 2 clojure.core.PersistentArrayMap/createAsIfByAssoc to-array)`

	(defn pam2 [iterations__6966__auto__]
	  (let [rhs (list :a0 0 :a1 1)]
	    (dotimes [x__6967__auto__ iterations__6966__auto__]
	      (let [{:keys [a0 a1]} (clojure.core.PersistentArrayMap/createAsIfByAssoc (to-array rhs))]
	        (+ a0 a1)))))
