(ns paren_match
  "This is my solution to 4Clojure problem #177, 'Balancing Brackets'.
  See http://www.4clojure.com/problem/177 .
  (Also see my Haskell solution,
  ~/Development/Haskell/Misc.Play/is-string-valid.hs .)
      -- Tyler"

  (:require [clojure.test :refer :all]))


(defn balanced?
  "Decides whether or not the given string is balanced with respect to the
  Char pairs in mates. The nil value is used internally to register failure.
  Consequently, although the given expression may be a collection of characters
  instead of a string, it must not contain nil."
  [expression]

  (let [mates {\( \), \[ \], \{ \}}         ; Characters that must balance.
        is-closing-char (set (vals mates))  ; Truthy iff char arg. closes expr.
        step (fn [targets x]
               (cond
                 (= x (first targets)) (rest targets)         ; Hit the target.
                 (is-closing-char x)   [nil]                  ; Forbidden. FAIL
                 :else                 (if-let [mate (mates x)]
                                         (cons mate targets)  ; New target.
                                         targets)))           ; Ordinary char.
        ]
    (empty? (reduce step [] expression))))


(deftest ballanced?-tests
  (are [expression      result]
                          (= (balanced? expression) result)
        ""              true
        "This string has no brackets."
                        true
        "class Test {
             public static void main(String[] args) {
                 System.out.println(\"Hello world.\");
             }
         }
        "               true
        "(start, end]"  false
        "())"           false
        "[ { ] } "      false
        "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))"
                        true
        "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))"
                        false
        "["             false
    ))

