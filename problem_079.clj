(ns four-clojure.problem_079
  "This is my solution to 4Clojure problem #79, 'Triangle Minimal Path'.
  See http://www.4clojure.com/problem/79
      -- Tyler"

  (:require [clojure.test :refer :all]))


(defn- pairwise-mins
  [row]
  (->> row
       (partition 2 1)
       (map #(apply min %))))


(defn- combine-rows
  [children parents]
  (map + (pairwise-mins children) parents))


(defn min-path
  [rows]
  (->> rows reverse (reduce combine-rows) first))


(defn solution [rows]
  (letfn [(pairwise-mins
            ; Given a row of n integers, returns the sequence of n-1 integers
            ; consisting of the minimum of each member of the row and its
            ; successor in row.
            [row]
            (->> row (partition 2 1) (map #(apply min %))))

          (combine-rows
            ; Returns a new row representing the cost from each corresponding
            ; parent node through the "bottom" row of the triangle.
            [descendent-costs parents]
            (map + (pairwise-mins descendent-costs) parents))
          ]
    (->> rows reverse (reduce combine-rows) first)))


(deftest pairwise-mins-test
  (are [result     row] (= result (pairwise-mins row))
        []         []
        []         [0]
        [1]        [1 2]
        [0 3 1 1]  [0 4 3 1 2]
    ))


(defn final [func]
  (are [result triangle] (= result (func triangle))
        7      '([1] [2 4] [5 1 4] [2 3 4 5])
        20     '([3] [2 4] [1 9 3] [9 9 2 4] [4 6 6 7 8] [5 7 3 5 1 4])
    ))


(deftest min-path-test
  (final min-path)
  (final solution))

