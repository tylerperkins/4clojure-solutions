(ns four-clojure.problem_053
  "This is my solution to 4Clojure problem #53, 'Longest Increasing Sub-Seq'.
  See http://www.4clojure.com/problem/53
      -- Tyler"

  (:require [clojure.test :refer :all]))


(defn solution
  "Given a vector of integers, finds the longest consecutive sub-sequence of
  increasing numbers. If two sub-sequences have the same length, the one that
  occurs first is returned.  An increasing sub-sequence must have a length of
  2 or greater to qualify."
  [the-vector]

  (let [
        lt        ; Given a pair (a b), returns (< a b).
                  (partial apply <)

        pairs     ; List of lists of pairs of integers. The pairs in a
                  ; contained list are all consecutive in the-vector and are
                  ; increasing pairs. So if the-vector is [... 9 2 3 6 8 1 ...],
                  ; then pairs is (... ((2 3) (3 6) (6 8)) ...).
                  (->> (partition 2 1 the-vector)
                       (partition-by lt)
                       (filter (comp lt first)))

        max-count ; Returns the list in pairs and its count if bigger than the
                  ; :count in max-so-far. Else returns max-so-far.
                  (fn [max-so-far sequence-pairs]
                    (let [ct (count sequence-pairs)]
                      (if (> ct (:count max-so-far))
                        {:pairs sequence-pairs, :count ct}
                        max-so-far)))

        [first-pair & rest-pairs]
                  ; The pairs of the biggest sequence
                  (->> pairs (reduce max-count {:count 0}) :pairs)
        ]

    ; Reconstitute the longest sequence from its pairs.
    (concat first-pair (map second rest-pairs))))


(deftest solution-test
  (are [result    the-vector] (= result (solution the-vector))
        [0 1 2 3] [1 0 1 2 3 0 4 5]
        [5 6]     [5 6 1 3 2 7]
        [3 4 5]   [2 3 3 4 5]
        []        [7 6 5 4]
    ))

