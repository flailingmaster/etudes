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
