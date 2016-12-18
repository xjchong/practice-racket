;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname find-route) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; A Node is a Sym
;; A Graph is a (listof (list Node (listof Node)))

;; Sample graph in adjacency list form.
(define g1 '((A (C D E))
             (B (K G))
             (C ())
             (D (F))
             (E (B))
             (F (H))
             (G (H))
             (H ())))


;; (find-route orig dest g) consumes an origin node, a destination node,
;;   and a graph to traverse, and produces a route from orig to dest if
;;   possible, otherwise produces false.
;; find-route: Node Node Graph -> (anyof (listof Node) Bool)

(define (find-route orig dest g)
  (local [;; (neighbours node) consumes a node and a graph that contains it,
          ;;   and produces a list of neighbours for that node.
          ;;   (If no node found in graph, assumes no neighbours.)
          ;; neighbours: Node Graph -> (listof Node)
          (define (neighbours node g)
            (cond [(empty? g) empty]
                  [(symbol=? node (first (first g))) (second (first g))]
                  [else (neighbours node (rest g))]))

          ;; (find-route/node orig) consumes an origin node and produces a
          ;;   route from orig to dest if possible, false otherwise.
          ;; find-route/node: Node -> (anyof (listof Node) Bool)
          (define (find-route/node orig)
            (cond [(symbol=? orig dest) (list orig)]
                  [else
                   (local
                     [(define route (find-route/list (neighbours orig g)))]
                     (cond [(false? route) route]
                           [else (append (list orig) route)]))]))

          
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
    (find-route/node orig)))