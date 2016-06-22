(ns etudes.core
	(:require [clojure.browser.repl :as repl]))

(defonce conn
	(repl/connect "http://localhost:9000/repl"))

(enable-console-print!)

(println "Hello world!")
(defn foo [a b]
  (* a b))

(defn radians
  "Convert degrees to radians"
	[degrees]
	(* (/ (.-PI js/Math) 180) degrees))

(defn daylight
	[latitude julian]
	(let [r (radians latitude)
	      part1 (.atan js/Math (* 0.9671396 (.tan js/Math (* .00860 (- julian 186)))))
	      P (.asin js/Math (* .39796
					               (.cos js/Math
													(+ 0.2163108 (* 2 part1)))))
				numerator (+ (.sin js/Math 0.01454) (* (.sin js/Math r)
				                                       (.sin js/Math P)))
			  denominator (* (.cos js/Math r) (.cos js/Math P))
				D (- 24 (* 7.63944 (.acos js/Math (/ numerator denominator))))]
				(* 60 D)))
(defn get-float [field]
(.parseFloat js/window (.-value (.getElementById js/document field))))

(defn testing [evt]
	(let [julian (get-float "julian")
		    latitude (get-float "latitude")
				result (daylight latitude julian)]
				(.alert js/window (clojure.string/join " " ["You clicked me!!!", latitude, julian])
				(set! (.-innerHTML (.getElementById js/document "result")) result)
				)))
(let [btn (.getElementById js/document "calculate")] (.addEventListener btn "click" testing))
