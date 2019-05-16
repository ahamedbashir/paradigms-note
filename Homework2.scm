

; Second Homework Set
; CSc 335
;Spring 2019


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This is a long problem set - you will want to set aside some hours.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Review lectures 3 and 4 before starting ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Proofs must be given for all programs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1.  Abelson and Sussman, Problems 1.11 and 1.12

; 2.  Write iterative and recursive scheme functions to return the sum of the digits within
; a non-negative integer.  For example, (sum-of-digits 345) is 12.

; 3.  Write iterative and recursive scheme programs to test whether the digits in a non-negative
; integer are in increasing order.  For example, the digits of 12348 are in increasing order, while
; those of 12343 are not.

; You may find the built-in functions quotient, remainder, truncate, zero? -- and perhaps others --
; helpful as you design your solutions for problems 2 and 3.  Have a look at the Scheme
; manual.


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