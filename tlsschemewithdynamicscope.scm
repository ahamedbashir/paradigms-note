; CSc 335

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; tls-scheme, from chapter 10 of tls, modified for dynamic scope

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; auxiliary functions

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define first car)

(define second cadr)

(define third caddr)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))



; environment implemented as a global table

(define global-table '())

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

(define extend-table cons)



(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (names entry)
                          (values entry)
                          entry-f)))



(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr values)
                                  entry-f)))))




(define new-entry build)

(define names
  (lambda (entry) (car entry)))

(define values
  (lambda (entry) (cadr entry)))




; the top level of the interpreter


(define meaning
  (lambda (e)
    ((expression-to-action e) e)))

(define value meaning)



; supporting functions for the intepeter

; syntax-directed dispatch on expression

(define expression-to-action
  (lambda (e)
    (cond 
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote plus)) *const)
      ((eq? e (quote mul)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))


(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond 
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
          *lambda)
         ((eq? (car e) (quote cond))
          *cond)
         (else *application)))
      (else *application))))


; operational semantics -- the definitions of the action functions

(define *const
  (lambda (e)
    (cond 
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))


(define *quote
  (lambda (e)
    (text-of e)))

(define text-of second)




(define *identifier
  (lambda (e)
    (lookup-in-table e global-table initial-table)))


(define initial-table
  (lambda (name)
    (car (quote ()))))



; no packing away of the table
(define *lambda
  (lambda (e)
    (build (quote non-primitive)
           (cdr e))))



(define formals-of first)

(define body-of second)


; cond is a special form that takes any number of 
; cond-lines ...  if it sees an else-line, it treats
; that cond-line as if its question part were true.

(define evcon
  (lambda (lines)
    (cond 
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))))
      
      ((meaning (question-of (car lines)))
       
       (meaning (answer-of (car lines))))
      
      (else (evcon (cdr lines))))))


(define else?
  (lambda (x)
    (cond 
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)



(define *cond 
  (lambda (e)
    (evcon (cond-lines-of e))))

(define cond-lines-of cdr)



(define evlis
  (lambda (args)
    (cond 
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args))
             (evlis (cdr args)))))))



(define *application
  (lambda (e)
    (apply
     (meaning (function-of e))
     (evlis (arguments-of e)))))

(define function-of car)

(define arguments-of cdr)




(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))



(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (apply-non-primitive
        (second fun) vals)))))


(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       (add1 (first vals)))
      ((eq? name (quote plus))
       (+ (first vals) (second vals)))
      ((eq? name (quote mul))
       (* (first vals) (second vals)))
      ((eq? name (quote sub1))
       (- (first vals) 1))
      ((eq? name (quote number?))
       (number? (first vals))))))


(define :atom?
  (lambda (x)
    (cond 
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive))
       #t)
      ((eq? (car x) (quote non-primitive))
       #t)
      (else #f))))


(define apply-non-primitive
  (lambda (fun vals)
    (set! global-table
          (extend-table 
           (new-entry 
            (formals-of fun)
            vals)
           global-table))
    (let ((v (meaning (body-of fun))))
      (set! global-table (cdr global-table))
      v)
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; try some simple computations:

;; (value '7)

;; (value '(add1 6))

;; (value '(cons (quote x) (quote ()))

;; (value '(cons x (quote ())) ) -- for an error

;; (value '((lambda (x) (add1 x)) 3))

;; (value '((lambda (x) (add1 x)) 
;;          ((lambda (x) (add1 x)) 4)))

;; (value '(((lambda (y)
;;            (lambda (x) (cons x y)))
;;          3)
;;         4))

; and here is one example, discussed in class, which gives one result
; under dynamic scoping, and another under lexical (or static) scoping.

(value '((lambda (a)
          ((lambda (p a)
             (cons a (p 2)))
           (lambda (x) (cons x a))
           5))
        3))

; and here is another

(value '((lambda (fact)
	   ((lambda (fact) 
	      (fact 5))
	    (lambda (n)
	      (cond ((zero? n) 1)
		    (else (mul n (fact (sub1 n))))))))
	 (lambda (n) (add1 n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;















































































