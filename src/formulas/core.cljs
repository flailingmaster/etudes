(ns formulas.core
	(:require [clojure.browser.repl :as repl]))

(defn distance [acc time]
	(/ (* acc time time) 2.0))

(defn kinetic-energy [mass velocity]
	(/ (* mass velocity velocity) 2.0))

(defn centripetal [velocity radius]
	(/ (* velocity velocity) radius))

(def G 6.67384e-11)
(defn gravitational-force [massa massb radius]
	(/ (* G massa massb) (* radius radius)))

(defn monthly-payments [principal apr years]
	(let [mpr (/ (/ apr 100) 12.0)
	      months (* years 12)
				subformula (.pow js/Math (+ 1 mpr) months)]
				(* principal (/ (* mpr subformula) (- subformula 1)))))

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
				(println part1)
				(println P)
				(println numerator)
				(println denominator)
				(* 60 D)))

(defn move-zeros [interspersed]
	(let [filtered-list (filter (fn [x] (not (zero? x))) interspersed)]
	     (concat filtered-list (repeat (- (count interspersed) (count filtered-list)) 0))))
