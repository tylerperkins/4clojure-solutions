(ns four-clojure.problem_073
  "This is my solution to 4Clojure problem #73, 'Analyze a Tic-Tac-Toe Board'.
  See http://www.4clojure.com/problem/73
      -- Tyler"

  (:require [clojure.test :refer :all]))


(defn solution [board-rows]
  ; Internally, we represent the tic-tac-toe board as a seq of nine keywords,
  ; each of which is one of the "players" :x, :o, or :e. To find whether a
  ; player is a winner, we encode the player's board positions as an integer,
  ; then compare this integer to integer "masks" representing the eight ways a
  ; player can win. The least significant binary digit (ones digit) in such an
  ; encoding says whether or not the first position on the board is occupied by
  ; the player in question, the twos digit says whether the second position is
  ; so occupied, etc.
  (letfn [
          (make-encoder [board]
            ; Returns a function for the given seq of players, which takes a
            ; player keyword and returns a unique integer representation of all
            ; the squares on the board occupied by that player.
            (fn [player]
              (->> (iterate (partial * 2) 1)
                   (map #(if (= %1 player) %2 0) board)
                   (reduce +))))

          (make-judge [player-occupied-code]
            ; Returns a "judge" function that takes a win-mask integer, and
            ; returns an integer indicating whether the player has won according
            ; to the win-mask. A win, indicated by 0, is returned iff the given
            ; player-occupied-code integer contains the player positions
            ; represented by win-mask.
            (fn [win-mask]
              (->> player-occupied-code (bit-and win-mask) (bit-xor win-mask))))

          (make-winner-fn [encode masks]
            ; Returns a function that takes a player keyword. It returns its
            ; argument if the board represented by the given encoder function
            ; and the given collection of win-masks together reveal the player
            ; to be a winner. Otherwise, it returns nil.
            (fn [player]
              (if (->> masks
                       (map (-> player encode make-judge))
                       (some zero?))
                player)))
          ]
    (some
      (-> (apply concat board-rows)     ; Board is seq of nine squares.
          make-encoder                  ; --> fn for encoding a player's posns.
          (make-winner-fn [0421 0124 0700 0070 0007 0444 0222 0111]))
                                        ; --> fn returning the player if winner.
      [:x :o])
    ))


(deftest solution-test
  (are [result the-vector] (= result (solution the-vector))
        nil    [[:e :e :e][:e :e :e][:e :e :e]]
        :x     [[:x :e :o][:x :e :e][:x :e :o]]
        :o     [[:e :x :e][:o :o :o][:x :e :x]]
        nil    [[:x :e :o][:x :x :e][:o :x :o]]
        :x     [[:x :e :e][:o :x :e][:o :e :x]]
        :o     [[:x :e :o][:x :o :e][:o :e :x]]
        nil    [[:x :o :x][:x :o :x][:o :x :o]]
        :x     [[:x :x :x][:x :o :x][:o :o :o]]
        :o     [[:o :x :e][:o :o :o][:x :e :o]]
        :x     [[:x :e :e][:o :x :e][:x :e :x]]
    ))

