(ns four-clojure.problem_178
  "This is my solution to 4Clojure problem #178, 'Best Hand'.
  See http://www.4clojure.com/problem/178
      -- Tyler"

  (:require [clojure.test :refer :all]))


(defn solution
  "Determines the best poker hand that can be made with five cards. The hand
  rankings are as follows:

    1. Straight flush:  All cards in the same suit, and in sequence
    2. Four of a kind:  Four of the cards have the same rank
    3. Full House:      Three cards of one rank, the other two of another rank
    4. Flush:           All cards in the same suit
    5. Straight:        All cards in sequence (aces can be high or low,
                        but not both at once)
    6. Three of a kind: Three of the cards have the same rank
    7. Two pair:        Two pairs of cards have the same rank
    8. Pair:            Two cards have the same rank
    9. High card:       None of the above conditions are met
  "
  [hand]

  (let [suit       {\D :diamond, \H :heart, \C :club, \S :spade}
        rank       (some-fn {\T 8, \J 9, \Q 10, \K 11, \A 12}
                            #(-> % str read-string dec dec))
        card-str-to-card-map
                   ; Same as my solution to Problem 128, titled, 'Recognize
                   ; Playing Cards'.
                   (fn [card-str]
                     {:suit (suit (first card-str)),
                      :rank (rank (last card-str))})
        card-maps  (map card-str-to-card-map hand)
        a-suit     (:suit (first card-maps))
        flush?     (every? #(= a-suit (:suit %)) card-maps)
        ranks      (->> card-maps (map :rank) (sort >))
        of-a-kind  (->> ranks
                        (group-by identity)
                        (map (comp count val))
                        (sort >))
        follow?    (fn [[r1 r2]] (= r1 (inc r2)))
        rank-pairs (partition 2 1 ranks)
        straight?  (and
                    (every? identity
                            (map follow? (rest rank-pairs)))
                    (or (follow? (first rank-pairs))
                        (= [12 0] [(first ranks) (last ranks)])))
        ]
    (cond
     (and straight? flush?)  :straight-flush
     (= 4 (first of-a-kind)) :four-of-a-kind
     (= [3 2] of-a-kind)     :full-house
     flush?                  :flush
     straight?               :straight
     (= 3 (first of-a-kind)) :three-of-a-kind
     (= [2 2 1] of-a-kind)   :two-pair
     (= 2 (first of-a-kind)) :pair
     :else                   :high-card
     )))


(deftest solution-test
  (are [result           arg] (= result (solution arg))
       :high-card        ["HA" "D2" "H3" "C9" "DJ"]
       :pair             ["HA" "HQ" "SJ" "DA" "HT"]
       :two-pair         ["HA" "DA" "HQ" "SQ" "HT"]
       :three-of-a-kind  ["HA" "DA" "CA" "HJ" "HT"]
       :straight         ["HA" "DK" "HQ" "HJ" "HT"]
       :straight         ["HA" "H2" "S3" "D4" "C5"]
       :flush            ["HA" "HK" "H2" "H4" "HT"]
       :full-house       ["HA" "DA" "CA" "HJ" "DJ"]
       :four-of-a-kind   ["HA" "DA" "CA" "SA" "DJ"]
       :straight-flush   ["HA" "HK" "HQ" "HJ" "HT"]
       )
  )

