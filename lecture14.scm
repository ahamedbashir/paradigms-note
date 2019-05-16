; CSc 335
; lecture14.scm
; Thursday May 2 2019



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; We now consider how one can define recursive functions in tls-scheme.

; Thus far, in drscheme, one would use the special form 'define'.  For example

(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))

; By some magic which we will understand later, the second occurrence of fact refers
; to the first one, and fact is called repeatedly on a diminishing argument.

; You may have noticed, however, that tls-scheme does not include 'define'.  It
; sets up special forms quote, cond, and lambda, but no define. 

; So if we can in fact define recursive functions in tls-scheme, it must be that
; we can do so without using 'define' itself. 

; Indeed, using a device known as the 'Y-combinator', it is possible to do precisely
; this. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; computing without define

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; We note first that we do not need define for non-recursive code.  For example, in
;
;; (add1 2)
;
;; we can eliminate the reference to add1 using inline coding
;
;; ((lambda (x) (+ x 1)) 2)


;; why can't we do something similar for recursion?  for example, why not

((lambda (f n)
   (f n))
 (lambda (n)
   (if (= n 0) 1 (* n (f (- n 1)))))
 5)

;; (here I assume that the special form if has been added to tls-scheme)

;; to figure out why this is not a way to define the factorial function, you will
;; want to work through its evaluation.  The main question is this: when evlis is
;; called to evaluate the arguments which yield the bindings for f and n, what
;; environment (table) does it use?  Does _that_ table know about _this_ f?


;; So we need another approach.  Following Friedman and Felleisen, we focus
;; on showing how to implement a particular recursive function - length - as
;; a reasonably accessible way of indicating the general treatment. 

;; The development given here is due to Friedman and Felleisen, with local annotations. 


; to start, here is a function which never returns

(define (eternity x)
  (eternity x))


; and here is the length function we seek to recreate without using define

(define length
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 (length (cdr l)))))))



; we will start by constructing a sequence of partial functions, each of which is
; a better approximation to the actual length function than the one before it. 

;; is everyone familiar with the concept of partial function?  If D and E are sets,
;; with D <= E, then a function f which is defined on D but not on E\D, is said to be
;; partial on E. So: f is partial on E if its domain D is a subset of E.

;; the function eternity - the empty function - is as partial as possible: it converges
;; nowhere. 

; Here is a slightly better approximation, which we might call length-0

(lambda (l)
  (cond 
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))

; what happens when we use this (call it length-0) on a non-empty list?  Well, it returns the
; correct result for just the empty list - for any nonempty list, it never returns. 



; next we have length-1

(lambda (l)
  (cond
    ((null? l) 0)
    (else
     (add1
      ((lambda (l)
         (cond
           ((null? l) 0)
           (else (add1
                  (eternity (cdr l))))))
       (cdr l))))))

; same question: what does length-1 compute?  (try applying length-1 to some short lists!).  Do you
; see that it is the partial function which agrees with length on all list inputs of length at most 1?

; one more, length-2 -- this one agreeing with length on all list inputs of length at most 2:

(lambda (l)
  (cond 
    ((null? l) 0)
    (else 
     (add1
      ((lambda (l)
         (cond
           ((null? l) 0)
           (else
            (add1
             ((lambda (l)
                (cond 
                  ((null? l) 0)
                  (else (eternity (cdr l)))))
              (cdr l))))))
       (cdr l))))))


; at some point we will grow weary of this, and note that all these programs contain 
; a function that looks somewhat like length.  We can abstract (add a parameter for) this function, so 
; that length-0, length-1 and length-2 look like these:

; length-0

((lambda (length)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)


; length-1 : function-that-looks-like-length applied to length-0

((lambda (length-0)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (add1 (length-0 (cdr l)))))))
 
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))  
  eternity))


; length-2 : function-that-looks-like-length applied to length-1

((lambda (length-1)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length-1 (cdr l)))))))
 
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  
  ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   
   eternity)))



; again, we see a lot of duplication.  abstracting once more,

; length-0

(
 (lambda (mk-length)
   (mk-length eternity))
 
; the value for the parameter mk-length is -- the function that looks like length
 (lambda (length)
   (lambda (l)
     (cond 
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
 )


; length-1

((lambda (mk-length)
   (mk-length
    (mk-length eternity)))

 ; again, the value for the parameter mk-length is -- the function that looks like length
 (lambda (length)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))



; if you don't see that this version of length-1 is the same as that given above, just work 
; out the expansion


(lambda (l)
  (cond ((null? l) 0)
        (else (add1
               ((mk-length eternity) (cdr l))))))

; where

(mk-length eternity)

; is

(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1
           (eternity (cdr l))))))




; similarly, 

; length-2

((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))



; the first expansion of length-2 has length bound to (mk-length (mk-length eternity)), and yields

(lambda (l)
  (cond ((null? l) 0)
        (else (add1 ( (mk-length (mk-length eternity)) (cdr l))))))


; to continue the expansion, note that (mk-length (mk-length eternity)) is just length-1





; if we could find a way of automatically producing these mk-length towers, we would have 
; what we want.

; let us go back and look at the last version of length-0:

((lambda (mk-length)
   (mk-length eternity))
 
 (lambda (length)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

; note what happens when we change eternity to mk-length

((lambda (mk-length)
   (mk-length mk-length))
 
 (lambda (length)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

; do you see that this is still very much like length-0?  The only change is that now we have a type error
; when the list has length more than 0, rather than the infinite recursion set up
; by the call to eternity -- the original function did not return for lists of non-zero length
; by dint of an infinite recursion; this one does not return for lists of non-zero length
; by dint of a type error.

; As partial functions, one might say that they are essentially the same: neither returns a
; value (errors are not here considered values). 

; Do you see where the type error comes from?  The parameter length is bound to mk-length,
; which expects a function as argument.  Our function gives it a list - namely, (cdr l),
; instead.  The function eternity does not constrain its argument in any way.

; Another type error: add1 expects a number, and our code gives it a procedure

; We cannot leave a type error in place - its presence indicates that the function is
; malformed. Our next steps remove it. 



; Note first that changing the length parameter to mk-length formally in the inner lambda does not
; change the meaning of the function:

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (add1 (mk-length (cdr l))))))))


; (similarly, (lambda (x) x) and (lambda (y) y) are the same function: we have changed the formal parameter consistently, 
; but nothing else.  On the other hand, (lambda (x) y) is something very different.  )

; Knowing that mk-length expects a single function argument, we try

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (add1 ((mk-length eternity)
                    (cdr l))))))))

; what function is this? if you (very carefully and patiently) expand this, you can see
; that we have an additional recursive call, and that this is now 
; length-1? (See the file lecture14-sample-expansion.scm to see how one might carry out the
; expansion)


; we can we do this again, to get length-2:


((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (add1 ((mk-length (mk-length eternity))
                    (cdr l))))))))


; it seems we could keep doing this -- (( ... (mk-length (mk-length ... (mk-length eternity) ...)) ...)
; we might have better and better approximations to the length function.

; let's look at this.  When we replace (mk-length eternity) by (mk-length (mk-length eternity)), are we
; not essentially asserting something like (f x) = (f (f x))?  

; And hence, (f x) = (f (f x)) = (f (f (f x))) = (f (f (f (f x)))) = ... 

; Since f is unary, we can omit the argument here and just write

; f = ff = fff = ffff = ...

; which suggests that f is a fixed point of f, and that we can achieve a 'perfect' approximation  
; to length by writing, in place of any form (( ... (mk-length (mk-length ... (mk-length eternity) ...)) ...)
; just (mk-length mk-length)


((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (add1 ((mk-length mk-length)
                    (cdr l))))))))



;
; this keeps passing mk-length to itself -- and, by expanding it a few times, it
; becomes quite plausible that it really is the length function.




; let's try it on an example


(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond 
        ((null? l) 0)
        (else (add1 ((mk-length mk-length)
                     (cdr l)))))))) 
 '(1 2 3 4 5))



; let's try it on another example


(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond 
        ((null? l) 0)
        (else (add1 ((mk-length mk-length)
                     (cdr l)))))))) 
 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))


; would you like another?


(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond 
        ((null? l) 0)
        (else (add1 ((mk-length mk-length)
                     (cdr l)))))))) 
 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30))



; we already have recursion, it seems, without define!


; we could stop here, but further transformation exposes what is really going on,
; and will allow us to make more general use of the pattern we are
; uncovering -- let's keep going for just a few more steps



; do you recall from an earlier lecture that if f is a unary function, then 
; f is the same as (lambda (x) (f x))?

; noting above that (mk-length mk-length) is a unary function, we can therefore replace it
; with (lambda (x) ((mk-length mk-length) x)), giving

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (add1 ((lambda (x) ((mk-length mk-length) x))
                    (cdr l))))))))

; equivalently

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (
    (lambda (length)                                   ;
      (lambda (l)                                      ;
        (cond                                          ;
          ((null? l) 0)                                ;
          (else (add1 (length (cdr l)))))))            ;  all of this is to be packaged as le
                                                       ;  in the next form
    
    (lambda (x)
      ((mk-length mk-length) x))))
 )

; and equivalently again

((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (
       le 
       (lambda (x)
         ((mk-length mk-length) x)))))
   )
 
 (lambda (length)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 )




; let's agree that

(lambda (length)
  (lambda (l)
    (cond 
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))

; will continue to be described as 'the function that looks like length',
; and that the operator

(lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (
       le 
       (lambda (x)
         ((mk-length mk-length) x)))))
   )


; will be described as 'the function that makes length from the function 
; that look like length'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; the function that makes length from the function that looks like length is called the 
; (applicative) y-combinator, and we have (roughly)

;   length = (y-combinator the-function-which-looks-like-length) 

; which you may check by just computing with

((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l)))))))
 )


; for example, try

(((lambda (le)
    ((lambda (mk-length)
       (mk-length mk-length))
     (lambda (mk-length)
       (le (lambda (x)
             ((mk-length mk-length) x))))))
  
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l)))))))
  )
 
 '(a b c d e f))




; but the real surprise comes up when we apply 'the function that makes length
; from the function that looks like length' (ie, the y-combinator) to
; a function which looks like factorial, as in


(((lambda (le)
    ((lambda (mk-length)
       (mk-length mk-length))
     (lambda (mk-length)
       (le (lambda (x)
             ((mk-length mk-length) x))))))
  
  (lambda (f)
    (lambda (x)
      (cond ((= 0 x) 1)
            (else (* x (f (- x 1)))))))
  )
 5)


; and find that this returns 5!, or 120.


; as they say - 'whoa!'.  where does that 120 come from?  clearly, something quite general is going on here. 

; it seems that the function which makes length from the function that looks like length is also the
; function that makes factorial from the function that looks like factorial.  

; you might also notice that the function that looks like factorial itself looks a lot like the
; the function which looks like length


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; we must of course at this point exclaim that these computations can be carried out
; in tls-scheme: we have recursive functions without using define!

; to compute the length of '(a b c d e f), for example, we could write

(value '(((lambda (le)
            ((lambda (mk-length)
               (mk-length mk-length))
             (lambda (mk-length)
               (le (lambda (x)
                     ((mk-length mk-length) x))))))
          
          (lambda (length)
            (lambda (l)
              (cond ((null? l) 0)
                    (else (add1 (length (cdr l)))))))
          )
         
         '(a b c d e f)))


; to use the function which looks like factorial with tls-scheme, you will need to 
; add a primitive or two to the base language


; so we don't need define. 

; define, however, was very convenient -- let's bring it back to name the function obtained 
; from 'the function that makes length from the function that looks like length' by replacing 
; mk-length by f (of course, this is the same function -- only its expression is different.  we
; say that y, below, is an 'alpha variant' of 'the function that makes length from the function
; that looks like length'. 


(define y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))


; so now we might write (in full scheme)

(define length
  (y
   (lambda (len)
     (lambda (l)
       (cond 
         ((null? l) 0)
         (else (add1 (len (cdr l)))))))))


; and

(define factorial
  (y
   (lambda (f)
     (lambda (n)
       (cond
         ((zero? n) 1)
         (else (* n (f (- n 1)))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



















