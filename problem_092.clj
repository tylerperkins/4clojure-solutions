(ns four-clojure.problem_092
  "This is my solution to 4Clojure problem #92, 'Read Roman Numerals'.
  See http://www.4clojure.com/problem/92
      -- Tyler"

  (:require [clojure.test :refer :all]))


(defn solution [roman-str]
  (let [valForChar {\z 0, \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
        step (fn [[total prev-n] c]
               (let [n (valForChar c)]
                 [((if (< prev-n n) - +) total prev-n) n]))
        ]
    (first (reduce step [0 0] (str \z roman-str \z)))))

(deftest solution-test
  (are [result roman-numeral] (= result (solution roman-numeral))
        14     "XIV"
        827    "DCCCXXVII"
        3999   "MMMCMXCIX"
        48     "XLVIII"
  ))