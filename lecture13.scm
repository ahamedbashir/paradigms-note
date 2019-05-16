
; CSc 335
; lecture13.scm
; Tuesday April 30 2019


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; We continue building the TLS interpreter -- you will want to evaluate the definitions of this file in a scheme session
; into which those of lecture12.scm have already been loaded


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; to go beyond the simple calculations demonstrated so far, to examples involving variables and user-defined functions, we
; will require means for associating values with variables.

; to this end, we first install the environment subsystem which was discussed in lecture11.scm - I presented it separately
; to allow you to familiarize yourself with its properties apart from its role in the overall system.

; for convenience, I copy the entire subsystem here.  


(define lookup-in-table
  (lambda (name table table-f)
    (cond 
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))


(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (names entry)
                          (vals entry)
                          entry-f)))



(define lookup-in-entry-help
  (lambda (name names vals entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car vals))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr vals)
                                  entry-f)))))


(define new-entry build)

(define names
  (lambda (entry) (car entry)))

(define vals
  (lambda (entry) (cadr entry)))


(define initial-table '())

(define table-f
  (lambda (name)
    (car (quote ()))))

(define extend-table cons)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; finally, we add the lambda subsystem -- in TLS, the only way we associate values with variables is via lambda


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))


(define myapply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))

; with this last definition, we pause to make sure everyone understands the concept of closure


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; we now actually have a turing-powerful programming system - we will start
; by looking at lambda without recursion, and then see how one can use a device called the Y-combinator
; to implement arbitrary recursive functions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; examples for class discussion, and which I would ask you to work through - carefully, and in detail - by hand

; construct the environments! work out the closures! draw diagrams!


(value '((lambda (x) (add1 x)) 3))


(value '((lambda (x) (add1 x))
	 ((lambda (x) (add1 x)) 4)))


(value '(((lambda (y)
            (lambda (x) (cons x y)))
          3)
         4))


(value '((lambda (x z)
           (cons x
                 ((lambda (x y) (cons z x))
                  3 4)
                 ))
         1 2))


(value '((lambda (f y)
          (f y))
        (lambda (x) (add1 x))
        4))


(value '((lambda (f y)
	   (f y))
	 ((lambda (x) (cond ((number? x) add1)
			    (else (lambda (y) (cons x y)))))
	  (quote z))

	 3))


; next, let's translate the following simple illustration of a closure to tls-scheme

(let ((x 3))
  (let ((x 4)
	(f (lambda (y) (+ y x))))
    (f 2)))
	      

; to get

(value '((lambda (x) 
	   ((lambda (x f) (f 2))
	    4
	    (lambda (y) (+ y x))))
	 3))




; you see that tls-scheme features lexical scope and first-class functions --
; after you have had some time to think about how one might go about certifying that TLS correctly implements
; lexical scope and first class functions, I will describe proof outlines in class, and then ask you to write
; up the arguments for homework.  Big surprise: induction is involved.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


					;

We now consider how one can define recursive functions in tls-scheme.

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



; we will start in the next class by constructing a sequence of partial functions, each of which is
; a better approximation to the actual length function than the one before it. 





