(ns four-clojure.problem_084
  "This is my solution to 4Clojure problem #84, 'Transitive Closure'.
  See http://www.4clojure.com/problem/84
      -- Tyler"

  (:require [clojure.test :refer :all]))


(defn assoc-pair
  [m [k v]]
  (assoc m k (conj (set (m k)) v)))


(defn pairs-into
  [a-map pairs]
  (reduce assoc-pair a-map pairs))


(defn make-extender
  [orig-keys-for-val orig-vals-for-key]

  (fn [[added-pairs keys-for-val vals-for-key] [key val]]
    (let [preceding-pairs  (for [k (orig-keys-for-val key)] [k val])
          succeeding-pairs (for [v (orig-vals-for-key val)] [key v])
          ]
      [(-> added-pairs
           (into preceding-pairs)
           (into succeeding-pairs))
       (-> keys-for-val
           (pairs-into (map reverse preceding-pairs))
           (pairs-into (map reverse succeeding-pairs)))
       (-> vals-for-key
           (pairs-into preceding-pairs)
           (pairs-into succeeding-pairs))
       ])))


(defn transitive-clojure
  [pairs]
  (loop [last-pairs
           #{}
         [generated-pairs keys-for-val vals-for-key]
           [pairs (pairs-into {} (map reverse pairs)) (pairs-into {} pairs)]
         ]
    (let [new-pairs (remove last-pairs generated-pairs)]
      (if (empty? new-pairs)
        last-pairs
        (recur (into last-pairs new-pairs)
               (reduce (make-extender keys-for-val vals-for-key)
                       [#{} keys-for-val vals-for-key]
                       new-pairs))))))


(defn solution
  [pairs]
  (letfn [
          (assoc-pair [m [k v]]
            ; Returns an updated version of map m with v an element in a set
            ; associated with key k. A "multimap" populated this way can thus
            ; associate each key with many distict values.
            (assoc m k (conj (set (m k)) v)))

          (pairs-into [a-map pairs]
            ; Populates the given multimap with the given key-value pairs.
            ; Each key k in the resulting multimap is the first element of one
            ; or more of the given pairs. The key is associated with a set
            ; containing all elements v such that [k v] is among the given
            ; pairs.
            (reduce assoc-pair a-map pairs))

          (make-extender [orig-keys-for-val orig-vals-for-key]
            ; Returns a function that adds new pairs -- lets call them "joins"
            ; -- to the set and the two multimaps in the given vector argument
            ; calculated using the given pair. The added joins are those
            ; resulting by transitivity with the given pair. For example, if
            ; orig-keys-for-val contained val 2 mapped to the set of keys
            ; #{... 1 ...}, and if the given pair was [2 3], then [1 3] would
            ; be added. If orig-vals-for-key contained key 3 mapped to the set
            ; of vals #{... 4 ...}, then [2 4] would also be added.

            (fn [[added-pairs keys-for-val vals-for-key] [key val]]
              (let [preceding-joins  (for [k (orig-keys-for-val key)] [k val])
                    succeeding-joins (for [v (orig-vals-for-key val)] [key v])
                    ]
                [(-> added-pairs
                     (into preceding-joins)
                     (into succeeding-joins))
                 (-> keys-for-val
                     (pairs-into (map reverse preceding-joins))
                     (pairs-into (map reverse succeeding-joins)))
                 (-> vals-for-key
                     (pairs-into preceding-joins)
                     (pairs-into succeeding-joins))
                 ])))
          ]

    ; We grow a set of pairs, last-pairs, where at the ith iteration we add only
    ; joins of a certain kind: Each has resulted from transitivity on exactly
    ; i+1 members of the given pairs. (Here, i starts at 1.) We stop when no
    ; joins are generated that are not already in last-pairs. Thus, even if
    ; pairs has cycles, we will terminate correctly.
    (loop [last-pairs
             #{}
           [generated-pairs keys-for-val vals-for-key]
             [pairs (pairs-into {} (map reverse pairs)) (pairs-into {} pairs)]
           ]
      (let [new-pairs (remove last-pairs generated-pairs)]
        (if (empty? new-pairs)
          last-pairs
          (recur (into last-pairs new-pairs)
                 (reduce (make-extender keys-for-val vals-for-key)
                         [#{} keys-for-val vals-for-key]
                         new-pairs)))))))


(defmacro make-test [func-to-test]
  `(deftest ~(symbol (str func-to-test "-test"))

    (are [init-set closed-set] (= (~func-to-test init-set) closed-set)

      #{}
      #{}

      #{[0 0] [0 1]}
      #{[0 0] [0 1]}

      #{[0 1] [1 2]}
      #{[0 1] [1 2] [0 2]}

      #{[8 4] [9 3] [4 2] [27 9]}
      #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}

      #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}
      #{["cat" "man"] ["cat" "snake"] ["man" "snake"] ["spider" "cat"]
        ["spider" "man"] ["spider" "snake"]}

      #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}
      #{["father" "son"] ["father" "grandson"] ["uncle" "cousin"]
        ["son" "grandson"]}
      )

    ; Now test 25 polygon-like relations, e.g., "triangle" #{[0 1] [1 2] [2 0]}.
    ; Their transitive closures should each be the complete relation consisting
    ; of all possible pairs of points. E.g. in the triangle case, the transitive
    ; clojure is #{[2 1] [1 0] [2 2] [0 0] [1 1] [0 1] [1 2] [0 2] [2 0]}.
    (is (every?
          #(apply = %)
          (letfn [(polygon# [n#]
                    (cons (dec n#) (range n#)))
                  (polygon-relation# [n#]
                    (set (partition 2 1 (polygon# n#))))
                  (complete-relation# [n#]
                    (set (for [k# (range n#), v# (range n#)] [k# v#])))
                  ]
            (for [n# (range 1 25)]
              [ (~func-to-test (polygon-relation# n#))
                (complete-relation# n#)
               ])
            )))
  )
)
(make-test transitive-clojure)
(make-test solution)


(deftest pairs-into-test
  (are [init-set result-map] (= (pairs-into {} init-set) result-map)

    #{} {}

    #{[:one 1] [:one 11]}
    {:one #{11 1}}

    #{["one" "ONE"] ["one" "TWO"]}
    {"one" #{"ONE" "TWO"}}

    #{[8 4] [8 3] [9 3] [4 2] [27 9] [27 3]}
    {9 #{3}, 4 #{2}, 8 #{3 4}, 27 #{3 9}} ))
