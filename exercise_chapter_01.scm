;;;;;;;;; chapter 1 exercise

;; 1.1
10
;;; 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 1 3))))) (* 3 (- 6 2) (- 2 7)))

;;; 1.3

(define sum3 (lambda ( a b c) (+ a b c)))
(sum3 1 2 3)

(define (sum3_1 a b c) (+ a b c))

(sum3_1 1 2 (- 3))


;;;                                                      1.4     problem


;; 1.5

;it will be in infinite loop becase the applicative order evaluates
; the arguments first before evaluation the call function


;; 1.6

(define (new-if predicate then-clause else-claue)
  ( cond ( predicate then-clause)
         (else else-clause)))

(define (sqrt-iter guess x)
;  (new-if (good-enough? guess x) guess
  (if (good-enough? guess x) guess
          (sqrt-iter (improve guess x) x)))

; new-if will be stuck in infinite loop as the arguments are evaluated in
; applicative order
; new-if will only work if there is no recursive call;

(define (good-enough? guess x)
  ( < (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt1 x)
  (sqrt-iter 1.0 x))

(sqrt1 8)


;;;;;;;;;;;;;;                                   1.7             problem

;;;;;;;;;;;;;;;;;;;;;; 1.8

(define (cube x) (* x x x))
(define (improveCube guess x)
  (/ (+ ( / x (square guess)) (* 2 guess)) 3.0))

(define (cbrt-iter guess x)
  (if (good-enough2? guess x) guess
          (cbrt-iter (improveCube guess x) x)))

(define (good-enough2? guess x)
  ( < (abs (- (cube guess) x)) 0.0001))

(define (cubicRoot x)
  (cbrt-iter 1.0 x))


(cubicRoot 64)



;;;;;;;;;;                       1.9



;;;;;;;;;;;;;;;;;;;
(define (count-change amount)
  (define (coin-change amount kinds-of-coin)
    (cond ( (= amount 0) 1)
          ( ( or (= kinds-of-coin 0) (< amount 0)) 0)
          ( else (+ (coin-change amount (- kinds-of-coin 1))
                    (coin-change (- amount (first-denomination kinds-of-coin) ) kinds-of-coin)))))
  (define (first-denomination coin)
    (cond ( (= coin 1) 1)
          ( (= coin 2) 5)
          ( (= coin 3) 10)
          ( (= coin 4) 25)
          ( (= coin 5) 50)))
  (coin-change amount 5))


;;;;;;;;;;;;;;;      1.11

;;;;;;;;;;;;;;;      1.12


;;;;;;;;;;;;;;;;;;;;

(define (exponent base power)
  (define (exponent-iter base power product)
    (cond ( (= power 0) product)
          ( else (exponent-iter base (- power 1) (* product base)))))
  (cond ( (= base 0) 0)
        ( (< power 0) (/ 1.0 (exponent-iter base (- power) 1)))
        ( else (exponent-iter base power 1))))


;;;;;;;;;;; faster power

(define (exponent-fast base power)
  (cond ( (= power 0) 1)
        ( (even? power) (square (exponent-fast base (/ power 2))))
        ( else (*  base (exponent-fast base (- power 1))))))

;;;;;;;;;;;;; gcd


(define (gcd1 a b)
  (if (= b 0)
      a
      (gcd1 b (remainder a b))))

;;;;;;;;;;;;;;;;;

(define (smallest-divisor n)
  (define (find-divisor d n)
    (cond ( ( > (square d) n) n)
          ( (divides? d n) d)
          ( else (find-divisor (+ d 1) n))))
  (define (divides? d n)
    (= (remainder n d) 0))

  (find-divisor 2 n))


(define (perfect-number? n)
  (define (divisors d n total)
    (cond ( (> (square d) n) (= total n))
          ( (divides? d n) (divisors (+ d 1) n (+ total d (quotient n d))))
          ( (not (divides? d n)) (divisors (+ d 1) n total))))
  (define (divides? d n)
    (= (remainder n d) 0))
  (divisors 2 n 1))


(perfect-number? 10)
(perfect-number? 14)
(perfect-number? 28)

(define (dis i n term)
  ;(display i)
  ;(display " : ")
  ;(display (term i))
  ;(display "\n")
  (display ( if (term i) i ""))
  (display ( if (term i) "\n" ""))
  (cond (( > i n) (display "done\n"))
        (else (dis (+ i 1) n term))))

(dis 0 100 perfect-number?)

(define (loop start n term)
    (display start)
    (display (term start))
    (display "\n")
    ( if ( >= start n) (display "done\n")
         (one (+ start 1) n term)))

;;;;;;;;;;;;;;;;;
(define (prime? n)
  (= (smallest-divisor n) n))

(dis 0 10 prime?)


;;;;;;;;;;;;;;;;;;                   1.29              problem 


(define (sum term a next b)
  ( if (> a b)
       0
       (+ (term a)
          (sum term (next a) next b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;           1.29            works
(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (sum start end)
    ( if (> start end) 0
           ( + (* (f (+ a (* start h))) ( cond ( (or (= start 0) (= start end)) 1 )
                                               ( (= (remainder start 2) 0) 2)
                                               ( (= (remainder start 2) 1) 4)) ) (sum (+ start 1) end))))
  (* (/ h 3.0) (sum 0 n)))

(define (identity x ) x)
(integral cube 0 1 100)
(integral identity 0 10 100)




;;;;;;;;;;;;;;;;;;;;;;;;;;                1.30
(display "exercise 1.30\n")

(define (sum-iter term a next b)
  (define (iter a result)
    ( if ( > a b)
         result
         (iter (next a) (+ result (term a)))))
  (iter a 0))


(sum-iter identity 0 (lambda (x) (+ x 1)) 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                1.31 a (recursive prcess)         b( iterative process)


(define (prod term a next b)
  (if ( > a b) 1
      (* (term a) (prod term (next a) next b))))



(define (prod-iter term a next b)
  (define (iter a product)
    (cond ( ( > a b) product)
          (else (iter (next a) (* product (term a))))))
  (iter a 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;            1.31      factorial


(define (factorial2 n)
  (prod (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(factorial2 3)



;;;;;;;;;;;;;;;;;;;;;;;;;;;            1.31       PI        using Wallis formula

(define (PI_ n)
  (define (sq_div x) (/ ( square x) ( * (- x 1) (+ x 1))))
  (define (next x) (+ x 2))
  (* 2 (prod sq_div 2.0 next n)))





;;;;;;;;;;;;;;;;;;;;;;;;                  1.34


;;;;;;;;;;;;;;;;;;;;;;;;;;;                     1.37         problem            no idea how to do it



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       1.40              problem




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      1.41

(define (double f)
  (lambda (x) (f (f x))))


(define (inc a) (+ a 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;          1.42
(define (compose f g)
  ( lambda (x) ( f (g x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;           1.43

(define (repeated f n)
  (lambda (x) (cond (( <= n 1) (f x))
                      ( else ((repeated f (- n 1)) (f x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;          1.43 using compose(1.42)          problem

