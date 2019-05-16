;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum-of-digits x)
  (define sum 0)
  (sum-digits-iter sum x))

(define (sum-digits-iter sum x)
  (if (< x 10) (+ sum (remainder x 10))
        (sum-digits-iter (+ sum (remainder x 10)) (quotient x 10))))

  
(sum-of-digits 345)


(define (sum-of-digits-rec x)
  (if (< x 10) x
      (+ (remainder x 10) (sum-of-digits-rec (quotient x 10)))))

(sum-of-digits-rec 345)


 (define (increasing-order? x)
   (increasing_order (quotient x 10) (remainder x 10)))
  
(define (increasing_order x y)
  (cond ((and (< x 10) (>= x y)) #f)
        ((and (< x 10) (< x y)) #t)
        ((>= (remainder x 10) y) #f)
        ((< (remainder x 10) y) (increasing_order (quotient x 10) (remainder x 10)))))

(increasing-order? 0124)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; coin change ;

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ( (or (< amount 0) (= kinds-of-coins 0)) 0)
        (else ( + (cc amount (- kinds-of-coins 1))
                  (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ( (= kinds-of-coins 1) 1)
        ( (= kinds-of-coins 2) 5)
        ( (= kinds-of-coins 3) 10)
        ( (= kinds-of-coins 4) 25)
        ( (= kinds-of-coins 5) 50)))

(count-change 40)
(count-change 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; f(n) = n, if n < 3;
; else f(n) = f(n-1) + 2f(n-2) + 3f(n-3)

;;;;;;;;;;;;recursive;;;;;;;;;;;;;;;;;;;
(define (Fn n)
  (cond ( (< n 3) n)
        (else (+ (Fn (- n 1)) (* 2 (Fn (- n 2))) (* 3 (Fn (- n 3)))))))

(Fn 0)
(Fn 1)
(Fn 2)
(Fn 3)
(Fn 4)
(Fn 5)

;;;;;;;;;;;iterative;;;;;;;;;;;;;;;;;;;;

(define (Fn2 n)
  (Fn-iter 0 1 2 2 n))

(define (Fn-iter f-3 f-2 f-1 count n)
  (cond ( (< n 3) n)
        ( (= count n) f-1)
        ( else (Fn-iter f-2 f-1 (+ (* 3 f-3) (* 2 f-2) f-1 ) (+ count 1) n))))

(Fn2 0)
(Fn2 1)
(Fn2 2)
(Fn2 3)
(Fn2 4)
(Fn2 5)



;;;;;;;;;;;;;; 1.2.4
; exponent linear recursive

(define (power b n)
  (if (= n 0) 1
      (* (power b (- n 1)) b )))

(power 2 3)
(power 2 5)

; linear iterative

(define (power2 b n)
  (power-iter b n 1))

(define (power-iter b counter product)
  (if (= counter 0) product
      (power-iter b (- counter 1) (* product b))))

(power2 2 3)
(power2 2 5)

;;;;;;;;;;;;;;;;;;;
; GCD

(define (GCD_ a b)
  (if (= b 0) a
      (GCD_ b (remainder a b))))

(GCD 2 10)
(GCD 12 28)
(GCD 40 6)
(GCD_ 2 10)
(GCD_ 12 28)
(GCD_ 40 6)

;;;;;;;; searching for divisors

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ( (> (square test-divisor) n) n)
        ( (divides? test-divisor n) test-divisor)
        ( else (find-divisor n (+ test-divisor 1)))))

(define (square a)
  (* a a))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 4)
(smallest-divisor 5)
(smallest-divisor 199)



;;;;;;;;;;; is prime

(define (isPrime? n)
  (= (smallest-divisor n) n))

(isPrime? 3)
(isPrime? 4)
(isPrime? 11)
(isPrime? 33)


;;;;;;;;;;;;;;;;;;;; procedue arguements

(define sum5 (lambda (y) (+ 5 y)))


;;;;;;;;

(define (sum term next a b)
  ( if ( > a b) 0
       (+ (term a) (sum term next (next a) b))))



(define (inc a) (+ a 1))
(define (cube a) (* a a a))

(define (identity a) a)


(define (pi-sum a b)
  (define (pi-term a) (/ 1.0 (* a (+ a 2))))
  (define (pi-next a) (+ a 4))
  (sum pi-term pi-next a b))

(* 8 (pi-sum 1 1000))



;;;;;;;;;; iterative sum

(define (sum-iter term next a b)
  (define (iter a result)
    (if ( > a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


;;;;;;;;; lambda

(define (square-sum a b)
  (sum (lambda (a) (* a a)) (lambda (a) ( + a 1)) a b))

(square-sum 1 3)

;;;;;;;;;;; let

(define (f x y)
  ( (lambda ( a b) ( + (* x (square a)) (* y b) (* a b)))
     ( + 1 (* x y)) (- 1 y) ))


(define (f-let x y)
  (let ( (a (+ 1 (* x y)))
         (b (- 1 y)) )
    (+ (* x (square a)) (* y b) (* a b))))

;;;;;;;;;;;;;;;;;

(define (quo d)
  (lambda (n) ( if ( < n d) 0 (+ 1 ( (quo d) (- n d))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;