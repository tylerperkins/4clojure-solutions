(ns four-clojure.problem_128
  "This is my solution to 4Clojure problem #53, 'Recognize Playing Cards'.
  See http://www.4clojure.com/problem/128
      -- Tyler"

  (:require [clojure.test :refer :all]))


(defn solution
  "A standard American deck of playing cards has four suits - spades, hearts,
  diamonds, and clubs - and thirteen cards in each suit. Two is the lowest rank,
  followed by other integers up to ten; then the jack, queen, king, and ace.

  It's convenient for humans to represent these cards as suit/rank pairs, such
  as H5 or DQ: the heart five and diamond queen respectively. But these forms
  are not convenient for programmers, so to write a card game you need some way
  to parse an input string into meaningful components. For purposes of
  determining rank, we will define the cards to be valued from 0 (the two) to 12
  (the ace)

  This function converts (for example) the string 'SJ' into a map of {:suit
  :spade, :rank 9}. A ten will always be represented with the single character
  'T', rather than the two characters '10'."
  [card-str]

  (let [suit {\D :diamond, \H :heart, \C :club, \S :spade}
        rank (some-fn {\T 8, \J 9, \Q 10, \K 11, \A 12}
                      #(-> % str read-string dec dec))
        ]
    {:suit (suit (first card-str)), :rank (rank (last card-str))}))


(deftest solution-test
  (are [arg  result]  (= (solution arg) result)
       "DQ"  {:suit :diamond, :rank 10}
       "H5"  {:suit :heart,   :rank  3}
       "CA"  {:suit :club,    :rank 12}
       )
  (is (= (range 13)
         (map (comp :rank solution str)
              '[S2 S3 S4 S5 S6 S7 S8 S9 ST SJ SQ SK SA])))
  )

