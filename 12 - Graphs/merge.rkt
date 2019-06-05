;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname merge) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; merge sort

;; (listof Number) -> (list of Number)
;; produces sorted list on ascending order using merge sort
(check-expect (merge-sort empty) empty)
(check-expect (merge-sort (list 2)) (list 2))
(check-expect (merge-sort (list 1 2)) (list 1 2))
(check-expect (merge-sort (list 4 3)) (list 3 4))
(check-expect (merge-sort (list 6 5 3 1 8 7 2 4 9)) (list 1 2 3 4 5 6 7 8 9)) ;!!!

;(define (merge-sort lon) lon) ;stub

;; template according to generative recursion

(define (merge-sort lon)
  (cond [(empty? lon) empty]
        [(empty? (rest lon)) lon]
        [else
         (merge (merge-sort (take lon (quotient (length lon) 2)))
                (merge-sort (drop lon (quotient (length lon) 2))))]))

;; (listof Number) (listof Number) -> (list of Number)
;; produces list of numbers merging provided lists in asc order by first element
(check-expect (merge empty empty) empty)
(check-expect (merge empty (list 1)) (list 1))
(check-expect (merge (list 1) empty) (list 1))
(check-expect (merge (list 1) (list 2)) (list 1 2))
(check-expect (merge (list 2) (list 1)) (list 1 2))
(check-expect (merge (list 2 3) (list 1 4)) (list 1 2 3 4))

;(define (merge lon1 lon2) empty) ;stub

(define (merge lon1 lon2)
  (cond [(empty? lon1) lon2]
        [(empty? lon2) lon1]
        [else
          (if (< (first lon1) (first lon2))
              (cons (first lon1) (merge (rest lon1) lon2))
              (cons (first lon2) (merge lon1 (rest lon2))))]))

;; (listof Number) Number -> (list of Number)
;; produces list of first given elements
;; ASSUME: count is always less than list length
(check-expect (take (list 1) 1) (list 1))
(check-expect (take (list 1 2 3 4 5) 3) (list 1 2 3))

;(define (take lon count) empty) ;stub

(define (take lon count)
  (cond [(zero? count) empty]
        [else (cons (first lon)                 
                    (take (rest lon) (sub1 count)))])) 

;; (listof Number) Number -> (list of Number)
;; produces list of last given elements
;; ASSUME: count is always less than list length
(check-expect (drop (list 1) 1) empty)
(check-expect (drop (list 1 2 3 4 5) 3) (list 4 5))

;(define (drop lon count) empty) ;stub
(define (drop lon count)
  (cond [(zero? count) lon]
        [else (drop (rest lon) (sub1 count))]))