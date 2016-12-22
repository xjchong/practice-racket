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
  (local [;; the only clever part about this code.
          ;; gets the leftmost coord for the relevant box.
          (define x (* 3 (quotient (posn-x posn) 3)))
          (define y (* 3 (quotient (posn-y posn) 3)))

          ;; (get-box) produces a list of the digits in the relevant box.
          (define get-box
            (local [;; (get-col grid) consumes a Sudoku and produces a box's
                    ;;   column as a column of rows.
                    ;; get-col: Sudoku -> (listof (listof Digit))
                    (define (get-col grid)
                      (cond [(= (length grid) (- 6 y)) empty]
                            [(<= (length grid) (- 9 y))
                             (append (get-row (first grid))
                                     (get-col (rest grid)))]
                            [else (get-col (rest grid))]))

                    ;; (get-row row) consumes a row and produces the relevant
                    ;;   digits in the row (of the box).
                    ;; get-row: (listof Digit) -> (listof Digit)
                    (define (get-row row)
                      (cond [(= (length row) (- 6 x)) empty]
                            [(<= (length row) (- 9 x))
                             (cons (first row) (get-row (rest row)))]
                            [else (get-row (rest row))]))]
              (get-col sudoku)))
          ]
    (not (member? digit get-box))))

                   

;; Need to write a neighbours function that will give a list of possible states...
;; A State is a (list Sudoku (listof Sudoku))

;; A neighbours function first needs to find the first empty spot on the grid.
;; Then go through all the digits, and find those that don't have a conflict.
;; Then produce a list of Sudoku.


;; (superimpose digit posn sudoku) consumes a Digit, Posn and imposes it at
;;   that Posn on a Sudoku, producing a new Sudoku if possible, false if
;;   the square is already occupied.
;; superimpose: Digit Posn Sudoku -> Sudoku
(define (superimpose digit posn sudoku)
  (local [;; (build-by-row grid) consumes a Sudoku and builds a new
          ;;   Sudoku row by row.
          ;; build-by-row: Sudoku -> Sudoku
          (define (build-by-row grid)
            (cond [(= (posn-y posn)
                      (- 9 (length grid)))
                   (cons (build-by-nth (first grid))
                         (rest grid))]
                  [else (cons (first grid)
                              (build-by-row (rest grid)))]))

          ;; (build-by-nth row) consumes a row of Digit and produces
          ;;   a new row with the digit superimposed at posn-x.
          ;; bvuild-by-nth: (listof Digit) -> (listof Digit)
          (define (build-by-nth row)
            (cond [(= (posn-x posn)
                      (- 9 (length row)))
                   (cons digit (rest row))]
                  [else (cons (first row)
                              (build-by-nth (rest row)))]))
          ]
    (build-by-row sudoku)))


;; (first-empty sudoku) consumes a Sudoku and produces the posn of the
;;   first empty square, from left to right, top to down. Returns false
;;   if there are no empty squares left.
;; first-empty: Sudoku -> (anyof Posn Bool)
(define (first-empty sudoku)
  (local [;; (get-x row) consumes a row of Digit and produces the
          ;;   x value of the first empty square, false if none.
          ;; get-x: (listof Digit) -> (anyof Digit Bool)
          (define (get-x row)
            (cond [(empty? row) false]
                  [(zero? (first row))
                   (- 9 (length row))]
                  [else (get-x (rest row))]))
          
          ;; (get-y grid) consumes a Sudoku and produces the y-value
          ;;   of the first empty square if possible, false otherwise.
          ;; get-y: Sudoku -> (anyof Posn Bool)
          (define (get-y grid)
            (cond [(empty? grid) false]
                  [else
                   (local [(define x (get-x (first grid)))]
                     (cond [(false? x) (get-y (rest grid))]
                           [else (make-posn x (- 9 (length grid)))]))]))]
    (get-y sudoku)))


;; (neighbours sudoku) consumes a Sudoku and produces a list of possible
;;   next moves as a list of Sudoku. Possible next moves are considering
;;   the first empty square only.
;; neighbours: Sudoku -> (listof Sudoku)
(define (neighbours sudoku)
  (local [(define posn (first-empty sudoku))

          ;; digit-safe? produces a function which consumes a Digit
          ;;  and produces true if the Digit can be placed in the grid
          ;;  safely, false otherwise.
          ;; digit-safe?: -> (Digit -> Bool)
          (define digit-safe?
            (lambda (x) (and (row-safe? x posn sudoku)
                             (col-safe? x posn sudoku)
                             (box-safe? x posn sudoku))))
          ]
    (foldr (lambda (x y) (cons (superimpose x posn sudoku) y))
           empty
           (filter digit-safe? '(1 2 3 4 5 6 7 8 9)))))
    


;; (solve-puzzle sudoku) consumes a Sudoku and produces a solved Sudoku if
;;   possible, false otherwise.
;; solve-puzzle: Sudoku -> (anyof Sudoku Bool)
(define (solve-puzzle sudoku)
  (local [;; (find-route/node orig) consumes an origin Sudoku and produces a
          ;;   route from the origin to a solved Sudoku if possible, else false.
          ;; find-route/node: Sudoku -> (anyof (listof Sudoku) Bool)
          (define (find-route/node orig)
            (cond [(false? (first-empty orig)) orig]
                  [else (find-route/list (neighbours orig))]))

          ;; (find-route/list lo-node) consumes a list of nodes and produces
          ;;   a route from orig to dest if possible, false otherwise.
          ;; find-route/list: (listof Node) -> (anyof (listof Node) Bool)
          (define (find-route/list lo-node)
            (cond [(empty? lo-node) false]
                  [else
                   (local
                     [(define route (find-route/node (first lo-node)))]
                     (cond [(false? route) (find-route/list (rest lo-node))]
                           [else route]))]))
          ]
    (find-route/node sudoku)))


;; (convert str) consumes a string of exactly 81 digits, and produces a
;;   Sudoku using those digits.
;; convert: Str -> Sudoku
(define (convert str)
  (local [(define loc (string->list str))

          (define lon
            (map (lambda (x) (string->number
                              (list->string (list x)))) loc))

          (define (get-sudoku lon row-acc grid-acc)
            (cond [(empty? lon)
                   (reverse (cons (reverse row-acc) grid-acc))]
                  [(= (length row-acc) 9)
                   (get-sudoku (rest lon)
                               empty
                               (cons (reverse row-acc) grid-acc))]
                  [else (get-sudoku
                         (rest lon)
                         (cons (first lon) row-acc)
                         grid-acc)]))
          ]
    (get-sudoku lon empty empty)))