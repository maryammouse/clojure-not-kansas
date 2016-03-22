(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        replacements {\A 14,
                      \K 13,
                      \Q 12,
                      \J 11,
                      \T 10}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])


(defn pair? [hand]
  (not (empty?
         (filter
           (fn [x] (== 2 x))
           (vals (frequencies (map rank hand))))))
  )



(defn three-of-a-kind? [hand]
  (not (empty?
         (filter
           (fn [x] (== 3 x))
           (vals (frequencies (map rank hand)))))))


(defn four-of-a-kind? [hand]
  (not (empty?
         (filter
           (fn [x] (== 4 x))
           (vals (frequencies (map rank hand)))))))


(defn flush? [hand]
  (not (empty?
         (filter
           (fn [x] (== 5 x))
           (vals (frequencies (map suit hand)))))))


(defn full-house? [hand]
  (and (not
         (empty?
           (filter
             (fn [x] (== 3 x))
             (vals (frequencies (map rank hand))))))
       (not
         (empty?
           (filter
             (fn [x] (== 2 x))
             (vals (frequencies (map rank hand))))))))

(full-house? three-of-a-kind-hand)
(full-house? full-house-hand)

(defn two-pairs? [hand]
  (or (== 2 (count
              (filter
                (fn [x] (== 2 x))
                (vals (frequencies (map rank hand))))))
      (not
        (empty?
          (filter
            (fn [x] (== 4 x))
            (vals (frequencies (map rank hand))))))))

(two-pairs? two-pairs-hand)
(two-pairs? pair-hand)
(two-pairs? four-of-a-kind-hand)

(defn straight? [hand]
  (let [fixed-ace-ranks (if (empty?
                           (filter
                             (fn [x] (= 2 (rank x)))
                             hand))
                       (map rank hand)
                       (replace {14, 1} (map rank hand)))
        lowest (first (sort fixed-ace-ranks))

        ]
    (= (sort fixed-ace-ranks)
       (range lowest (+ 5 lowest)))))

(straight? straight-hand)
(straight? two-pairs-hand)
(straight? low-ace-straight-hand)
(straight? ["2H" "2D" "3H" "4H" "5H"])
(straight? high-ace-straight-hand)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(straight-flush? straight-hand)
(straight-flush? flush-hand)
(straight-flush? straight-flush-hand)
(straight-flush? low-ace-straight-flush-hand)
(straight-flush? high-ace-straight-flush-hand)

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                    [two-pairs? 2] [three-of-a-kind? 3]
                    [straight? 4] [flush? 5]
                    [full-house? 6] [four-of-a-kind? 7]
                    [straight-flush? 8]}]
    (apply max
           (map second
                (filter
                  (fn [x] (if ((first x) hand) true false))
                  checkers)))))

(value high-seven)
(value pair-hand)
(value two-pairs-hand)
(value three-of-a-kind-hand)
(value straight-hand)
(value flush-hand)
(value full-house-hand)
(value four-of-a-kind-hand)
(value straight-flush-hand)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)



