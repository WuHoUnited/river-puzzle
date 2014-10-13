(ns river-puzzle.core
  "Uses core.logic to solve a japanese river crossing problem located
  at http://www.japaneseiqtest.net/"
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))

;; We will represent the state of the puzzle using a list
;; where each person (and the raft) is an index in the list and their location
;; is represented as the keyword value :left or :right.
;;
;; We do not actually need to represent a person's location as
;; being "In the raft."
;;
;; A move will consist of a keyword indicating who moved and which direction.
;; For instance :cop-right means the cop went right in the raft on his own.
;; :cop-son1-left indicates the the cop and the first son went left together.


;; The locations of items are represented by a list with the following shape.
;; [raft cop prisoner dad son-1 son-2 mom daughter-1 daughter-2]


;; We will enumerate the list of legal moves.
;; This piece is large but very straightforward.
(defne move-o
  "Taking move from start position results in end position."
  [start move end]
       ([[:left :left p d s-1 s-2 m d-1 d-2] :cop-right [:right :right p d s-1 s-2 m d-1 d-2]])
       ([[:left :left :left d s-1 s-2 m d-1 d-2] :cop-prisoner-right [:right :right :right d s-1 s-2 m d-1 d-2]])
       ([[:left :left p :left s-1 s-2 m d-1 d-2] :cop-dad-right [:right :right p :right s-1 s-2 m d-1 d-2]])
       ([[:left :left p d :left s-2 m d-1 d-2] :cop-son1-right [:right :right p d :right s-2 m d-1 d-2]])
       ([[:left :left p d s-1 :left m d-1 d-2] :cop-son2-right [:right :right p d s-1 :right m d-1 d-2]])
       ([[:left :left p d  s-1 s-2 :left d-1 d-2] :cop-mom-right [:right :right p d s-1 s-2 :right d-1 d-2]])
       ([[:left :left p d s-1 s-2 m :left d-2] :cop-daughter1-right [:right :right p d s-1 s-2 m :right d-2]])
       ([[:left :left p d s-1 s-2 m d-1 :left] :cop-daughter2-right [:right :right p d s-1 s-2 m d-1 :right]])
       ([[:left c p :left s-1 s-2 m d-1 d-2] :dad-right [:right c p :right s-1 s-2 m d-1 d-2]])
       ([[:left c p :left s-1 s-2 :left d-1 d-2] :dad-mom-right [:right c p :right s-1 s-2 :right d-1 d-2]])
       ([[:left c p :left :left s-2 m d-1 d-2] :dad-son1-right [:right c p :right :right s-2 m d-1 d-2]])
       ([[:left c p :left s-1 :left m d-1 d-2] :dad-son2-right [:right c p :right s-1 :right m d-1 d-2]])
       ([[:left c p d s-1 s-2 :left d-1 d-2] :mom-right [:right c p d s-1 s-2 :right d-1 d-2]])
       ([[:left c p d s-1 s-2 :left :left d-2] :mom-daughter1-right [:right c p d s-1 s-2 :right :right d-2]])
       ([[:left c p d s-1 s-2 :left d-1 :left] :mom-daughter2-right [:right c p d s-1 s-2 :right d-1 :right]])
       ([[:right :right p d s-1 s-2 m d-1 d-2] :cop-left [:left :left p d s-1 s-2 m d-1 d-2]])
       ([[:right :right :right d s-1 s-2 m d-1 d-2] :cop-prisoner-left [:left :left :left d s-1 s-2 m d-1 d-2]])
       ([[:right :right p :right s-1 s-2 m d-1 d-2] :cop-dad-left [:left :left p :left s-1 s-2 m d-1 d-2]])
       ([[:right :right p d :right s-2 m d-1 d-2] :cop-son1-left [:left :left p d :left s-2 m d-1 d-2]])
       ([[:right :right p d s-1 :right m d-1 d-2] :cop-son2-left [:left :left p d s-1 :left m d-1 d-2]])
       ([[:right :right p d s-1 s-2 :right d-1 d-2] :cop-mom-left [:left :left p d s-1 s-2 :left d-1 d-2]])
       ([[:right :right p d s-1 s-2 m :right d-2] :cop-daughter1-left [:left :left p d s-1 s-2 m :left d-2]])
       ([[:right :right p d s-1 s-2 m d-1 :right] :cop-daughter2-left [:left :left p d s-1 s-2 m d-1 :left]])
       ([[:right c p :right s-1 s-2 m d-1 d-2] :dad-left [:left c p :left s-1 s-2 m d-1 d-2]])
       ([[:right c p :right s-1 s-2 :right d-1 d-2] :dad-mom-left [:left c p :left s-1 s-2 :left d-1 d-2]])
       ([[:right c p :right :right s-2 m d-1 d-2] :dad-son1-left [:left c p :left :left s-2 m d-1 d-2]])
       ([[:right c p :right s-1 :right m d-1 d-2] :dad-son2-left [:left c p :left s-1 :left m d-1 d-2]])
       ([[:right c p d s-1 s-2 :right d-1 d-2] :mom-left [:left c p d s-1 s-2 :left d-1 d-2]])
       ([[:right c p d s-1 s-2 :right :right d-2] :mom-daughter1-left [:left c p d s-1 s-2 :left :left d-2]])
       ([[:right c p d s-1 s-2 :right d-1 :right] :mom-daughter2-left [:left c p d s-1 s-2 :left d-1 :left]]))

(defne not-member-o
  "Item is not a member of coll."
  [item coll]
       ([_ []])
       ([_ [c . cs]]
         (!= c item)
         (not-member-o item cs)))

;; We use ^:tabled to get better answers quickly.
;; This relation reads as follows:
;;   all the following conditions must be true:
;;     1) Either
;;       a) the cop is with the prisoner
;;       b) the prisoner is not with anybody else
;;     2) Either
;;       a) a parent is not with a child of the oposite sex
;;       b) the parents are together
(defne ^:tabled safe-o
  "requires that state is a legal position by satisfying the rules of which
  people are not allowed to be with eachother."
  [state]
  ([[_ cop prisoner dad son-1 son-2 mom daughter-1 daughter-2]]
     (all
      (conde
       [(== cop prisoner)]
       [(not-member-o prisoner [dad son-1 son-2 mom daughter-1 daughter-2])])
      (conde
       [(!= dad daughter-1) (!= dad daughter-2) (!= mom son-1) (!= mom son-2)]
       [(== mom dad)]))))

;; This is the relation that must hold for a river-puzzle.
;; The logic is roughly:
;;   1) ensure that the start and end are both legal
;;   2) If the start is the end, we can get there without moving
;;        Otherwise, make a move and try to get there from our new position
;; We use tabled to get better answers quicker.
(def river-puzzle-o
  "This relation of the form [start moves end] indicates
  that if you begin with start and make moves then you will be at end.
  It ensures that start, moves and end are all legal."
  (tabled [start moves end]
    (safe-o start)
    (safe-o end)
    (matche [start moves end]
            ([start () start])
            ([start [m . ms] end]
              (fresh [interim]
                     (move-o start m interim)
                     (river-puzzle-o interim ms end))))))


;; Begin doing a couple scenarios.

(def START (repeat 9 :left))
(def END (repeat 9 :right))

;; everybody starts at the left and tries to move right.
(run 1 [q]
     (river-puzzle-o START
                     q
                     END))

;; Generate 5 random legal puzzles with solutions their solutions.
;; You will notice that is gives very generic answers.
(run 5 [q]
     (fresh [start moves end]
            (river-puzzle-o start moves end)
            (== q {:start start
                   :moves moves
                   :end end})))

;; Generate 5 random legal puzzles ending with everybody on the right
;; Since we have fixed the board, you will notice that there are no longer
;;   any general answers.
(run 5 [q]
     (fresh [start moves]
            (river-puzzle-o start moves END)
            (== q {:start start
                   :moves moves})))

;; There are numerous other questions you could ask.
;; For instance
;;   - check that a given sequence of moves actually solves a particular puzzle
;;   - check whether a particular setup can be reached
;;   - check whether a particular sequence of moves can ever occur
;;   - see what position you would be in after taking a given set of moves
;;       from a start position.

