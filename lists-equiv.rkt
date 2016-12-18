;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lists-equiv) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; (lists-equiv lst1 lst2) consumes two lists, and produces true if both
;;   lists are exactly the same as each other, except possibly for order.
;; lists-equiv: (listof Any) (listof Any) -> Bool

(define (lists-equiv lst1 lst2)
  (local [(define lst1-unique
            (foldl (lambda (x y) (cond [(member? x y) y]
                                       [else (cons x y)])) empty lst1))

          ;; times-appear: Any (listof Any) -> Nat
          (define (times-appear item lst)
            (cond [(empty? lst) 0]
                  [(equal? item (first lst))
                   (add1 (times-appear item (rest lst)))]
                  [else (times-appear item (rest lst))]))

          ;; check: (listof Any) -> Bool
          (define (check lst)
            (cond [(empty? lst) true]
                  [(= (times-appear (first lst) lst1)
                      (times-appear (first lst) lst2))
                   (check (rest lst))]
                  [else false]))]
    
    (and (= (length lst1) (length lst2))
         (check lst1-unique))))

(check-expect (lists-equiv '(1 2 3 4) '(1 2 3 4)) true)
(check-expect (lists-equiv '(1 2 3 4) '(1 2 3 5)) false)
(check-expect (lists-equiv '(1 2 3 4) '(1 2 3)) false)
(check-expect (lists-equiv '(1 2 3) '(1 2 3 4)) false)
(check-expect (lists-equiv '(1 2 2 3) '(2 3 2 1)) true)
(check-expect (lists-equiv '(1 1 1 2 2) '(2 2 2 1 1)) false)
(check-expect (lists-equiv empty empty) true)
(check-expect (lists-equiv empty '(1 2)) false)
(check-expect (lists-equiv '(1 2) empty) false)
