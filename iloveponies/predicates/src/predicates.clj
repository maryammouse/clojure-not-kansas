(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(sum-f inc dec 4)
(sum-f inc identity 5)
(sum-f identity - 10)

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k] (== k n)))

(filter (less-than 3) [1 2 3 4 5])
(filter (less-than 4) [-2 12 3 4 0])
(filter (equal-to 2) [2 1 3 2.0])
(filter (equal-to 2) [3 4 5 6])

(defn set->predicate [a-set]
  (fn [x] (contains? a-set x)))

(filter (set->predicate #{1 2 3 nil}) [2 nil 4 nil 6])

(defn pred-and [pred1 pred2]
  :-)

(defn pred-or [pred1 pred2]
  :-)

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  :-)

(defn has-award? [book award]
  :-)

(defn HAS-ALL-THE-AWARDS? [book awards]
  :-)

(defn my-some [pred a-seq]
  :-)

(defn my-every? [pred a-seq]
  :-)

(defn prime? [n]
  :-)
;^^
