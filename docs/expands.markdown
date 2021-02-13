Expansion of `(macroexpand-1 '(destructure-n 2 20000))`

	(let [rhs (list :a0 0 :a1 1)] 
	  (time 
	    (dotimes [x__7751__auto__ 20000] 
		  (let [{:keys [a0 a1]} rhs] 
		    (+ a0 a1)))))

Expansion of `(macroexpand-1 '(destructure-n 2 20000 clojure.core.PersistentHashMap/create seq))`

	(let [rhs (list :a0 0 :a1 1)]
	  (time 
	    (dotimes [x__7751__auto__ 20000] 
		  (let [{:keys [a0 a1]} (clojure.core.PersistentHashMap/create (seq rhs))] 
		    (+ a0 a1)))))

Expansion of `(macroexpand-1 '(destructure-n 2 20000 clojure.core.PersistentArrayMap/createAsIfByAssoc to-array))`

	(let [rhs (list :a0 0 :a1 1)] 
	  (time 
	    (dotimes [x__7751__auto__ 10] 
		  (let [{:keys [a0 a1]} (clojure.core.PersistentArrayMap/createAsIfByAssoc (to-array rhs))] 
		    (+ a0 a1)))))

