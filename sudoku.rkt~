;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; A Digit is one of (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
;; A Sudoku is a (listof (listof Digit)) where each list is 9 elements long.

;; To solve a Sudoku, we will use a backtracking method.
;; We will need a function that, given a coordinate and a number,
;;  will check if there are any conflicts.
;;  There are three kinds of conflicts: row, column, box

;; So we will need three functions, one for each conflict.

;; We will need a backtracking routine -> call it solve-puzzle.

;; (solve-puzzle) will try to find a route from neighbour to neighbour,
;;   starting with a partially filled graph, to a graph that has no
;;   empty spaces.

;; We will need a neighbours function that takes a state, and produces
;;  a list of neighbours to try.

;; When solve-puzzle tries a state from a list of states, if there are no
;;  conflicts, it moves onto the next neighbours for that successful state.

;; Sudoku constants.

(define s1 '((5 7 0 0 0 0 0 6 0)
             (0 9 0 0 2 0 3 5 0)
             (6 0 0 5 7 0 0 0 0)
             (0 0 0 0 3 0 0 0 0)
             (0 1 6 0 0 0 0 9 0)
             (0 0 0 9 0 7 8 0 0)
             (4 0 0 0 0 0 0 3 6)
             (0 5 0 0 0 1 0 0 9)
             (0 0 0 0 0 8 5 0 0)))

;;******************************************************************************

;; (row-safe? digit posn sudoku) consumes a Digit and a Posn and a Sudoku,
;;   and produces true if there are no row conflicts, and false if there are.
;; row-safe?: Digit Posn Sudoku -> Bool
(define (row-safe? digit posn sudoku)
  (local [;; (get-row grid) consumes a Sudoku (grid) 
          ;;   and produces the row of Digits at the Posn.
          ;; get-row: Sudoku -> (listof Digit)
          (define (get-row grid)
            (cond [(= (posn-y posn)
                      (- 9 (length grid)))
                   (first grid)]
                  [else (get-row (rest grid))]))]
    (not (member? digit (get-row sudoku)))))


;; (col-safe? digit posn sudoku) consumes a Digit and a Posn and a Sudoku,
;;   and produces true if there are no column conflicts, false if there are.
;; col-safe?: Digit Posn Sudoku -> Bool
(define (col-safe? digit posn sudoku)
  (local [;; (get-nth row) consumes a row and produces the nth Digit.
          ;; get-nth: (listof Digit) -> Digit
          (define (get-nth row)
            (cond [(= (posn-x posn)
                      (- 9 (length row)))
                   (first row)]
                  [else (get-nth (rest row))]))

          ;; (get-col grid) consumes a Sudoku (grid) and
          ;;   produces the column of Digits at the Posn.
          ;; get-col: Sudoku -> (listof Digit)
          (define (get-col grid)
            (foldr (lambda (x y) (cons (get-nth x) y)) empty grid))
          ]
    (not (member? digit (get-col sudoku)))))


;; (box-safe? digit posn sudoku) consumes a Digit, Posn, and Sudoku and
;;   produces true if there are no box conflicts, false otherwise.
;; box-safe?: Digit Posn Sudoku -> Bool
(define (box-safe? digit posn sudoku)
  (local [(define (get-box 

          

          