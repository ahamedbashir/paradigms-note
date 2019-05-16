(define atom?
  (lambda (a)
    (and (not (null? a))
         (not (pair? a)))))


(atom? 'a)
(atom? '(a b))
(atom? '(a))
(atom? '())

(define subst*
  (lambda (lst old new)
    ( cond ( (null? lst) lst)
           ( (atom? (car lst))
             (cond ( (eq? (car lst) old) (cons new (subst* (cdr lst) old new)))
                   ( else (cons (car lst) (subst* (cdr lst) old new)))))
           (else
            ( cons (subst* (car lst) old new) (subst* (cdr lst) old new))))))


(define list1 '(a b c d a d))

(define l1 '(a b))
(define l2 '(**))

(define list2 (list l1 list1 (list l1 l2 list1)))

(subst* list2 'a l2)



(define same-shape
  (lambda (l1 l2)
    ( cond ( (or (not (pair? l1)) (not (pair? l2))) (eq? l1 l2))
           (else (and (same-shape (car l1) (car l2)) (same-shape (cdr l1) (cdr l2)))))))


(same-shape l1  '(a b))


;;;;;;;;;;;;;;;;;;;;;;

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define left_operand
  (lambda (aexp)
    (car aexp)))
(define right_operand
  (lambda (aexp)
    (car (cdr (cdr aexp)))))



(define plus_aexp
  (lambda (aexp)
    (eq? (operator aexp) '+)))


(define subt_aexp
  (lambda (aexp)
    (eq? (operator aexp) '-)))

(define times_aexp
  (lambda (aexp)
    (eq? (operator aexp) '*)))

(define power_aexp
  (lambda (aexp)
    (eq? (operator aexp) '^)))


(define natural_num?
  (lambda (x)
    (and (integer? x) (>= x 0))))


(define evaluate_aexp
  (lambda (aexp)
    (cond ( (natural_num? aexp) aexp)
          ( else
            (cond ((plus_aexp aexp) (plus (evaluate_aexp (left_operand aexp)) (evaluate_aexp (right_operand aexp))))
                  ((subt_aexp aexp) (subt (evaluate_aexp (left_operand aexp)) (evaluate_aexp (right_operand aexp))))
                  ((times_aexp aexp) (times (evaluate_aexp (left_operand aexp)) (evaluate_aexp (right_operand aexp))))
                  ((power_aexp aexp) (power (evaluate_aexp (left_operand aexp)) (evaluate_aexp (right_operand aexp)))))))))


(define plus
  (lambda (x y)
    (+ x y)))

(define subt
  (lambda (x y)
    (- x y)))

(define times
  (lambda (x y)
    (* x y)))

(define power      ; not taking negative power
  (lambda (x y)
    ( cond ( (= y 0) 1)
           (else (times x (power x (- y 1)))))))




; length of list

(define list_length
  (lambda  (lst)
    ( cond ( (null? lst) 0)
           (else (+ 1 (list_length (cdr lst)))))))


(length l1)
(list_length l1)

; nth element of list

(define (n_th_element n lst)
  (define (helper count lst)
    ( cond ( (null? lst) '())
           ( (= n count) (car lst))
           ( else (helper (+ 1 count) (cdr lst)))))
  (helper 0 lst))

(n_th_element 4 list1)

(define (nth_ele n lst)
  (define (helper n lst)
    (cond ( (= n 0) (car lst))
          ( else (helper (- n 1) (cdr lst)))))
  (helper n lst))



;; last element of list

(define (last_elem lst)
  (define (helper lst)
    ( cond ((null? (cdr lst)) (car lst))
           (else (helper (cdr lst)))))
  (helper lst))

(last_elem list1)
;; remove last element

(define (remove_last lst)
  (define (helper lst)
    ( cond ( (null? (cdr lst)) '())
           (else (cons (car lst) (helper (cdr lst))))))
  (helper lst))

(remove_last list1)


;; append to last

(define (append_elem lst elem)
  ( define (helper lst)
     ( cond ( (null? lst) (list elem))
            ( else (cons (car lst) (helper (cdr lst))))))
  (helper lst))

(append_elem l1 'x)
; append list to another list

(define (append_list lst1 lst2)
  (define (helper lst1)
    ( cond ( (null? lst1) lst2)
           ( else (cons (car lst1) (helper (cdr lst1))))))
  (cond ((null? lst1) lst2)
        ((null? lst2) lst1)
        (else (helper lst1))))

(define l3 (list 'x 'y))
(append_list l1 l3)


; reverse list

(define  (reverse_list lst)
   ( cond ( (null? lst) '())
          ( else (cons (last_elem lst) (reverse_list (remove_last lst))))))

(define (reverse_lst lst)
  (define (helper lst result)
    (cond ( (null? lst) result)
          ( else (helper (cdr lst) (cons (car lst) result)))))
  (helper lst '()))



; mapping

(define mapping
  (lambda (lst func)
    ( cond ( (null? lst) '())
           ( else ( cons (func (car lst)) (mapping (cdr lst) func))))))

(define il1 (list 1 2 3 4 5))

(mapping il1 (lambda (x) (* x x)))




;; accum

(define (accum lst op init)
  (define (helper lst)
    ( cond ( (null? lst) init)
           (else (op (car lst) (helper (cdr lst))))))
  (helper lst))

(accum il1 * 1)
(accum (mapping il1 (lambda (x) (* x x x))) + 0)


;; filter

(define (filter pred lst)
  ( cond ( (null? lst) lst)
         ( (pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
         ( else (filter pred (cdr lst)))))

(filter (lambda (x) (= (modulo x 2) 1)) il1)




;; deep reverse

(define (reverse_deep lst)
  ( cond ( (null? lst) lst)
         ( (atom? (car lst)) (append (reverse_deep (cdr lst)) (list (car lst))))
         (else (append (reverse_deep (cdr lst)) (list (reverse_deep (car lst)))))))

(define test (list l1 l2 (list l1 l2 (list l1))))
(display  "original : ")
test

(display  "reversed : ")
(reverse_deep test)


; depth of tree
(define (depth tree)
  ( cond ( (null? tree) 0)
         ( (atom? tree) 0)
         (else ( max (+ 1 (depth (car tree))) (depth (cdr tree))))))

(define tree '(a ((b c) ( (c d) f))))

(depth tree)


; leaves
(define (count-leaves tree)
  (cond ( (null? tree) 0)
        ( (atom? tree) 1)
        (else (+ (count-leaves (car tree)) (count-leaves (cdr tree))))))

(count-leaves tree)


; fringe

(define (fringe tree)
  (cond ((null? tree) '())
        ((atom? tree) (list tree))
        ( else (append (fringe (car tree)) (fringe (cdr tree))))))

(fringe tree)


;; number of nodes

(define (count-nodes tree)
  ( cond ( (null? tree) 0)
         ( (atom? tree) 1)
         ( else (+ (count-nodes (car tree)) (count-nodes (cdr tree))))))

(count-nodes tree)



;; enumeration

(define (enumerate-interval low high)
  ( if ( > low high) '()
       (cons low (enumerate-interval (+ 1 low) high))))

(enumerate-interval 1 10)


;;
(define (enumerate-tree tree)
  ( cond ( (null? tree) '())
         ( (not (pair? tree)) (list tree))
         ( else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))

(enumerate-tree tree)


;; signal processing

(define tree-2 (list 1 (list (list 2 3) (list 4 5)) (list (list 6))))

(mapping (filter (lambda (x) (= (modulo x 2) 1)) il1) (lambda (x) (* x x)))

(define (sum-odd-square tree)
  (accum (mapping (filter (lambda (x) (= (modulo x 2) 1)) il1) (lambda (x) (* x x))) + 0))


(sum-odd-square tree-2)



;;
(define (fib n)
  ( define (helper current prev count)
     ( cond ( (= count 0) prev)
            ( else (helper (+ current prev) current (- count 1)))))
  (helper 1 0 n))

(fib 6)


(define (fib-all n)
  (accum (map fib (enumerate-interval 0 n)) cons '()))

(fib-all 10)

(define (fib-even n)
  (accum (filter (lambda (x) (= (modulo x 2) 0)) (map fib (enumerate-interval 0 n))) cons '()))

(fib-even 10)



;;;;;;;;;;;

; nested mappings

; given a positive integer n, find all ordered pairs of distinct positive integers i and j,
; where 1 <= i < j <= n
 
(define (ordered-pair n)
  (accum
   (map (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval (+ i 1) n)))
          (enumerate-interval 1 (- n 1)))
                               append '()))
(ordered-pair 4)


(define (flatmap proc lst)
  (accum (map proc lst) append '()))

;;;;; permutation

(define (permutation lst)
  ( if (null? lst) (list '())
       (flatmap (lambda (x) (map (lambda (y) (cons x y)) (permutation (remove x lst)))) lst)))

(define (remove x lst)
  (filter (lambda (y) (not (= x y))) lst))

(display "permutation : ")
(permutation (list 1 2 3))

;; union

(define (union l1 l2)
  ( cond ( (null? l1) l2)
         ( (null? l2) l1)
         ( else ( cons (car l1) (union (cdr l1) (remove (car l1) l2))))))

(define set1 (list 1 2 3 4))
(define set2 (list 3 4 5 6))
(display "union : ")
(union set1 set2)
;; intersection

(define (intersect l1 l2)
  ( cond (( or (null? l1) (null? l2)) '())
         (else (append (filter (lambda (x) ( = x (car l2))) l1 ) (intersect l1 (cdr l2))))))

(display "intersect : ")
(intersect set1 set2)
;; subset


;; power set

;; k-subset


; quick sort

; select first element as pivot
; partion into two list using that pivot element by filter function
; once (cdr lst) is null, return that
; left_list pivot right_list

(define (quicksort lst)
  (cond ( (null? lst) lst)
        ( else (let* ( (pivot (car lst))
                       (left (quicksort (filter (lambda (x) (< x pivot )) (cdr lst))))
                       (right (quicksort (filter (lambda (x) (>= x pivot)) (cdr lst)))))
                 (append left (list pivot) right)))))


(define il2 '(7 3 6 0))

(quicksort il2)

; merge sort

; find mid point

(define (getLeft lst)
   ( define (helper lst tmp)
      ( cond ( (null? tmp) tmp)
             (else (cons (car lst) (helper (cdr lst) (cdr (cdr tmp)))))))
    (helper lst lst))


; selection sort

; insertion sort

; bubble sort