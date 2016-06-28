(ns formulas.core
	(:require [clojure.browser.repl :as repl]
						[clojure.string :as str]))

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
	[latitude date]
	(let [[year month day] (map #(js/parseInt %) (str/split date #"-"))
	      julian (ordinal-day day month year)
		    r (radians latitude)
	      part1 (.atan js/Math (* 0.9671396 (.tan js/Math (* .00860 (- julian 186)))))
	      P (.asin js/Math (* .39796
					               (.cos js/Math
													(+ 0.2163108 (* 2 part1)))))
				numerator (+ (.sin js/Math 0.01454) (* (.sin js/Math r)
				                                       (.sin js/Math P)))
			  denominator (* (.cos js/Math r) (.cos js/Math P))
				D (- 24 (* 7.63944 (.acos js/Math (/ numerator denominator))))]
				(println year)
				(println month)
				(println day)
				(println (legal-year? year))
				(println (legal-month? month))
				(println (legal-day? day month year))
				(println julian)
				(println r)
				(println part1)
				(println P)
				(println numerator)
				(println denominator)
				(* 60 D)))

(defn move-zeros [interspersed]
	(let [filtered-list (filter (fn [x] (not (zero? x))) interspersed)]
	     (concat filtered-list (repeat (- (count interspersed) (count filtered-list)) 0))))

(defn ordinal-day
  "Takes a day, month, and year as its three arguments and returns the ordinal (Julian) day of the year."
	[day month year]
	(if (legal-date? day month year)
	  (let [feb-days (if (leap-year? year) 29 28)
		      num-days [0 31 feb-days 31 30 31 30 31 31 30 31 30 31]
					monthtotal (reduce + (take month num-days))]
					(+ day monthtotal))
		0))

(defn legal-date? [day month year]
	(and (legal-year? year) (legal-month? month) (legal-day? day month year)))

(defn legal-day?
	[day month year]
	(cond
		(and (= month 2) (leap-year? year)) (and (>= day 1) (<= day 29))
		(and (= month 2) (not (leap-year? year))) (and (>= day 1) (<= day 28))
	  (or (= month 9) (= month 4) (= month 6) (= month 11)) (and (>= day 1) (<= day 30))
		(or (= month 1) (= month 3) (= month 5) (= month 7)(= month 8) (= month 10)(= month 12)) (and (>= day 1) (<= day 31))
		:else false
	))

(defn leap-year?
      "Return true if given year is a leap year; false otherwise"
      [year]
      (or (and (= 0 (rem year 4)) (not= 0 (rem year 100)))
        (= 0 (rem year 400))))

(defn legal-year? [year]
	(> year 1584))
(defn legal-month? [month]
	(and (>= month 1) (<= month 12)))
