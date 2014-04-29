(ns four-clojure.problem_094
  "This is my solution to 4Clojure problem #94, 'Game of Life'.
  See http://www.4clojure.com/problem/94

      -- Tyler"

  (:require [clojure.test :refer :all]
            [clojure.set :refer [union difference]]))


;;;;;;;;;;;;;;;;;;;;;;;; Testable version ;;;;;;;;;;;;;;;;;;;;;;;;


(defn point-halo [[x y]]
  (set (for [dx [-1 0 1] dy [-1 0 1] :when (not= [dx dy] [0 0])]
        [(+ x dx) (+ y dy)])))

(defn halo [points]
  (difference
    (->> points (map point-halo) (reduce union))
    points))

(defn num-neighbors [points p]
  (count (filter (set points) (point-halo p))))

(defn having-neighbors [points pred ps]
  (filter (comp pred (partial num-neighbors points)) ps))

(defn next-gen [points]
  (concat (having-neighbors points #{2 3} points)
          (having-neighbors points #{3} (halo points))))

(defn strings-to-points [strings]
  (for [[st row] (map vector strings (range))
        [ch col] (map vector st (range))
        :when (not= \space ch)
        ]
    [col row]))

(defn points-to-strings
  [char cols rows points]
  (let [blank-row (vec (take cols (repeat \space)))
        blank-board (vec (take rows (repeat blank-row)))
        ]
    (->> points
         (reduce (fn [board [c r]] (assoc-in board [r c] char)) blank-board)
         (map (partial reduce str)))))

(defn next-gen-strings
  [strings]
  (->> strings
       strings-to-points
       next-gen
       (points-to-strings \# (count (first strings)) (count strings))))


;;;;;;;;;;;;;;;;;;;;;;;; Version for 4Clojure ;;;;;;;;;;;;;;;;;;;;;;;;


(defn halo-for-4Clojure [points]
  "We can't require other libraries like clojure.set in a solution submitted to
  4clojure.com. This implementation of the halo function, defined here for
  testing, is used in solution below.
  "
  (->> points
       (map point-halo)
       (apply concat)
       (remove (set points))))

(defn solution [strings]
  (letfn [
           (point-halo [[x y]]
             ; Returns a set of the eight points surrounding [x y].
             (set (for [dx [-1 0 1] dy [-1 0 1] :when (not= [dx dy] [0 0])]
                   [(+ x dx) (+ y dy)])))

           (halo [points]
             ; Returns a seq of [x y] points, each of which is in the point-halo
             ; of one or more of the given points, excluding those among the
             ; given points.
             (->> points
                  (map point-halo)
                  (apply concat)
                  (remove (set points))))

           (num-neighbors [points p]
             ; Returns the number of members of points that are among the
             ; eight points surrounding p.
             (count (filter (set points) (point-halo p))))

           (having-neighbors [points pred ps]
             ; Returns a seq of the members of ps, each of whose number of
             ; neighbors (among points) satisfies the given predicate.
             (filter (comp pred (partial num-neighbors points)) ps))

           (next-gen [points]
             ; Returns a new seq of [x y] points representing the next
             ; "generation" produced from the given points.
             (concat (having-neighbors points #{2 3} points)
                     (having-neighbors points #{3} (halo points))))

           (strings-to-points [strings]
             ; Converts the given vector of strings to a seq of [x y] points.
             ; The points are exactly those for which x-th character of the y-th
             ; string is not a space character.
             (for [[st row] (map vector strings (range))
                   [ch col] (map vector st (range))
                   :when (not= \space ch)
                   ]
               [col row]))

           (points-to-strings [char cols rows points]
             ; Returns a vector of strings representing a grid measuring cols
             ; by rows positions. Each [col row] point will be represented as
             ; character char at the col-th position in the row-th string of
             ; the result.
             (let [blank-row (vec (take cols (repeat \space)))
                   blank-board (vec (take rows (repeat blank-row)))
                   ]
               (->> points
                    (reduce (fn [board [c r]] (assoc-in board [r c] char))
                            blank-board)
                    (map (partial reduce str)))))
           ]

    (->> strings
         strings-to-points
         next-gen
         (points-to-strings \# (count (first strings)) (count strings)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;


(deftest test-point-halo
  (are [p      result] (= (point-halo p) result)
        [0 0]  #{[1 0][1 1][0 1][-1 1][-1 0][-1 -1][0 -1][1 -1]}
        [-3 3] #{[-4 3][-4 4][-3 4][-2 4][-2 3][-2 2][-3 2][-4 2]}
    ))

(deftest test-halo
  (are [points       result] (= (halo points) result)
        [[0 0]]      #{[1 0][1 1][0 1][-1 1][-1 0][-1 -1][0 -1][1 -1]}
        [[0 0][1 1]] #{[1 0][0 1][-1 1][-1 0][-1 -1][0 -1][1 -1][2 1][2 2][1 2][0 2][2 0]}
    )
  (are [points       result] (= (set (halo-for-4Clojure points)) result)
        [[0 0]]      #{[1 0][1 1][0 1][-1 1][-1 0][-1 -1][0 -1][1 -1]}
        [[0 0][1 1]] #{[1 0][0 1][-1 1][-1 0][-1 -1][0 -1][1 -1][2 1][2 2][1 2][0 2][2 0]}
    ))

(deftest test-num-neighbors
  (let [ps #{[1 0] [0 1] [-1 0] [0 -1] [10 10]}]
    (are [points p       result] (= (num-neighbors points p) result)
          ps     [ 0  0] 4
          ps     [ 1  0] 2
          ps     [ 1  1] 2
          ps     [ 0  1] 2
          ps     [-2  0] 1
          ps     [ 0 -3] 0
          ps     [10 10] 0
      )
    ))

(deftest test-next-gen-strings
  (let [boards [
        ["      "
         " ##   "
         " ##   "
         "   ## "
         "   ## "
         "      "]
                ["      "
                 " ##   "
                 " #    "
                 "    # "
                 "   ## "
                 "      "]
        ["     "
         "     "
         " ### "
         "     "
         "     "]
                ["     "
                 "  #  "
                 "  #  "
                 "  #  "
                 "     "]
        ["      "
         "      "
         "  ### "
         " ###  "
         "      "
         "      "]
                ["      "
                 "   #  "
                 " #  # "
                 " #  # "
                 "  #   "
                 "      "]
        ]
        points [
          [[1 1] [2 1] [1 2] [2 2] [3 3] [4 3] [3 4] [4 4]]
            [[1 1] [2 1] [1 2] [4 3] [3 4] [4 4]]
          [[1 2] [2 2] [3 2]]
            [[2 1] [2 2] [2 3]]
          [[2 2] [3 2] [4 2] [1 3] [2 3] [3 3]]
            [[3 1] [1 2] [4 2] [1 3] [4 3] [2 4]]
          ]
        ]
    (is (= (map strings-to-points boards) points))
    (is (= (map #(points-to-strings \# %1 %1 %2) [6 6 5 5 6 6] points) boards))
    (eval `(do
      (are [pts result] (= (set (next-gen pts)) (set result)) ~@points)
      (are [strings result] (= (next-gen-strings strings) result) ~@boards)
      (are [strings result] (= (solution strings) result) ~@boards)
      ))
    ))
