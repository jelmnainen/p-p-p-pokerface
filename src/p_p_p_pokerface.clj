(ns p-p-p-pokerface)
(def char-rank->value {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [[rnk _]]
  (if
    (Character/isDigit rnk)
    (Integer/valueOf (str rnk))
    (char-rank->value rnk)))

(defn suit [[_ second]]
  (str second))

(defn contains-n-same-cards [hand n])


(defn contains-n-same-cards? [hand n]
  (contains? (set (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (contains-n-same-cards? hand 2))

(defn three-of-a-kind? [hand]
  (contains-n-same-cards? hand 3))

(defn four-of-a-kind? [hand]
  (contains-n-same-cards? hand 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or
    (four-of-a-kind? hand)
    (== 2 (count (filter (fn [x] (= x 2)) (vals (frequencies (map rank hand))))))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        smallest    (apply min sorted-ranks)]
    (or
      (= (sort (replace {14 1} (sort (map rank hand)))) (range 1 6))
      (= sorted-ranks (range smallest (+ 5 smallest))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        eligble-hands (filter (fn [[check _]] (check hand)) checkers)]
    (apply max (map second eligble-hands))))
