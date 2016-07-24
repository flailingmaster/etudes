(ns etudes.core
	(:require [clojure.browser.repl :as repl]
		        [goog.dom :as dom]
						[goog.events :as events]
						[clojure.string :as str]))

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

(defn legal-year?
	"Etude requirement is that years are greater than 1584"
	[year]
	(> year 1584))
(defn legal-month? [month]
	(and (>= month 1) (<= month 12)))



(defn daylight
	"Given latitude and a Gregorian date, returns minutes of daylight."
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
				(* 60 D)))
(defn get-float [field]
(.parseFloat js/window (.-value (dom/getElement field))))

(defn testing
	"Mostly bullshit"
	[evt]
	(let [gregorian (.parseFloat js/window (.-value (dom/getElement "gregorian")))
		    latitude (get-float "latitude")
				result (daylight latitude julian)]
				(dom/setTextContent (dom/getElement "result") result)
				))

(defn mean
  "Calculates the arithmetic average of the list"
	[integers]
	(/ (apply + integers) (count integers))
	)

(defn median
	"Calculates the median of the numbers"
	[integers]
	(let [n (count integers)
	      stub (drop (- (int (/ n 2)) 1) integers)]
	    (if (even? n)
			  (/ (+ (first stub) (second stub)) 2)
				(first stub))))

(defn stdev
	"Calculates the standard deviation"
	[integers]
	(let [sum-of-squares (apply + (map #(* % %) integers))
	      square-of-sums (apply + integers)
				n (count integers)]
				(.sqrt js/Math (/ (- sum-of-squares (/ (* square-of-sums square-of-sums) n)) (- n 1)))))

(defn fib
	"Exercise in mental recursion"
	[n]
	(cond (= n 0) 0
	 		  (= n 1) 1
				:else (+ (fib (- n 1)) (fib (- n 2)))))

(defn calculate
  "Event handler"
  [evt]
  (let [numbers (map js/window.parseFloat
    (str/split (.-value (.-target evt)) #"[, ]+"))]
           (dom/setTextContent (dom/getElement "mean") (mean numbers))
           (dom/setTextContent (dom/getElement "median") (median numbers))
           (dom/setTextContent (dom/getElement "stdev") (stdev numbers))))

(def pocket-depths
  [[], [2 2 1 2 2 1], [3 1 2 3 2 3],
  [3 1 3 2 1 2], [3 2 3 2 2 1], [2 3 1 2 1 1],
  [3 1 3 2 3 2], [3 3 2 1 3 1], [4 3 3 2 3 3],
  [3 1 1 3 2 2], [4 3 4 3 2 3], [2 3 1 3 2 2],
  [1 2 1 1 3 2], [1 2 2 3 2 3], [1 3 2 1 3 3], [],
  [3 2 3 1 1 2], [2 2 1 1 3 2], [2 1 1 1 1 2],
  [3 3 2 1 1 3], [3 1 3 2 3 2], [3 3 1 2 3 3],
  [1 2 2 3 3 3], [2 2 3 2 3 3], [2 2 2 4 3 4],
  [3 4 3 3 3 4], [1 1 2 3 1 2], [2 2 3 2 1 3],
  [3 4 2 4 4 3], [3 3 2 1 2 3], [2 2 2 2 3 3],
  [3 2 3 2 3 2]])

(defn contains-pockets
	"Given a vector of values, returns a vector of booleans"
	[sequence]
	(reduce #(or %2 %1) (map #(>= % 4) sequence)))

(defn alert
	"Display tooth numbers where any of the pocket depths is 4 or greater."
	[depths]
	(filter #(not= -1 %) (map-indexed (fn [idx itm] (if itm (+ 1 idx) -1)) (map contains-pockets depths))))

(events/listen (dom/getElement "calculate") "click" testing)
(events/listen (dom/getElement "numbers") "change" calculate)
