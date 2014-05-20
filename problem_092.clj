(ns four-clojure.problem_092
  "This is my solution to 4Clojure problem #92, 'Read Roman Numerals'.
  See http://www.4clojure.com/problem/92
      -- Tyler"

  (:require [clojure.test :refer :all]))


(defn solution [roman-str]
  (let [valForChar {\z 0, \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
        step       (fn [f c]
                     (let [n     (valForChar c)       ; Current char's value.
                           total (f n)                ; We now know how to calc.
                           ]                          ;  result for PREV values.
                       #((if (< n %) - +) total n)))  ; Wait! Next value is %.
        ]
    ((reduce step identity (str \z roman-str)) 0)))   ; \z inits. total, and
                                                      ;  0 is for the final %.


(deftest solution-test
  (are [result roman-numeral] (= result (solution roman-numeral))
        1      "I"
        90     "XC"
        14     "XIV"
        827    "DCCCXXVII"
        3999   "MMMCMXCIX"
        48     "XLVIII"
  ))