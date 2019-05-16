
; CSc 335
; lecture15.scm
; May 14 2019



; We prepare to build a scheme interpreter which implements definitions (define) by introducing
; some additional features of scheme.  These include letrec (recursive let), set! (assignment),
; and call/cc (continuations).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; let, let* and letrec

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; We have seen many examples of let over the course of this semester; we know that let can always
; be replaced by an equivalent lambda 


; for instance



; the equivalent lambda form is

((lambda (a b) (+ a b))
 1 2)


;;

; for

(let ((a 1))
  (let ((b 2))
    (+ a b)))

; the equivalent lambda form is

((lambda (a)
   ((lambda (b) (+ a b))
    2))
 1)

;;

; for

(let ((a 1) (b 2))
  (+ a (let ((a 3) (c 4))
         (* a b c))
     ))

; the equivalent lambda form is

((lambda (a b)
   (+ a ((lambda (a c)
           (* a b c))
         3 4)))
 1 2)


;;


;for an error

(let ((a 1)
      (b (+ a 2)))
  (+ a b))

; this is expected once we consider the equivalent lambda formulation:

((lambda (a b)
   (+ a b))
 1
 (+ a 2))


; let* (iterated let) can do what we want here

(let* ((a 1)
       (b (+ a 2)))
  (+ a b))




; we have noted previously that let can be used to bind closures, as in


(let ((fact (lambda (n) (add1 n))))
  (fact 4))


; but that there is a problem when we attempt to name a recursive procedure this way


(let ((fact ((lambda (n) (add1 n)))))
  (let ((fact (lambda (n)
		(cond ((zero? n) 1)
		      (else (* n (fact (- n 1)))))))
	(fact 5))))



; we need letrec
(letrec ((f (lambda (n) 
              (if (= n 0)
                  1
                  (* n (f (- n 1)))))))
  (f 4))

;

(letrec ((a 1)
	 (b (+ a 2)))
  (+ a b))


; Interestingly, letrec is based on assignment (set! in scheme).  Before considering its implementation,
; however, it makes sense to get a better idea of what it is used for.  (The following is indebted
; again to Friedman and Felleisen - this time to the sequel text 'The Seasoned Schemer')



; Recall the code for multirember

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) ())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))


; we could remove the requirement that the parameter 'a' be carried each time by exploiting scope and the 
; Y-combinator, as follows

(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond 
              ((null? lat) (quote ()))
              ((eq? a (car lat)) (mr (cdr lat)))
              (else (cons (car lat)
                          (mr (cdr lat))))))))
     lat)))

; here the argument to Y reminds us of the argument we passed to Y
; when we wanted to compute factorial

; that is, it is 'almost' multirember

; Y being something of an inconvenience, we can use letrec
; to accomplish the same thing


(define multirember
  (lambda (a lat)
    (letrec (
             (mr (lambda (lat)
                   (cond
                     ((null? lat) (quote ()))
                     ((eq? a (car lat))
                      (mr (cdr lat)))
                     (else 
                      (cons (car lat)
                            (mr (cdr lat)))))))
             )
      (mr lat))))


; as you see from these examples, we can use (letrec ...) and scope to remove arguments that do not 
; change for recursive applications: mr is a function of just the one parameter, lat -- the parameter
; a is held constant, and not passed directly to the recursive calls to mr. 


; a similar use of letrec is indicated in the context of currying

(define multirember-f
  (lambda (test?)
    (letrec
        (
         (m-f
          (lambda (a lat)
            (cond
              ((null? lat) (quote ()))
              ((test? (car lat) a)
               (m-f a (cdr lat)))
              (else
               (cons (car lat)
                     (m-f a (cdr lat)))))))
         )
      m-f)))  


; eg,

(define myfunc (multirember-f eq?))

(my-func 'a '(a b c a b c))


; it is always good to see examples!


; letrec applied to union

; we had previously designed the function to pass set2, 
; even though this parameter never changes

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))


; a version using letrec solves the awkwardness

(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond 
                ((null? set) set2)
                ((member? (car set) set2) (U (cdr set)))
                (else (cons (car set)
                            (U (cdr set))))))))
      (U set1))))


; worried that you may not recall the precise (library) definition of member?  write a private
; member? function enclosed in union

(define union
  (lambda (set1 set2)
    (letrec (
             (U 
              (lambda (set)
                (cond 
                  ((null? set) set2)
                  ((member? (car set) set2) (U (cdr set)))
                  (else (cons (car set)
                              (U (cdr set)))))))
             
             (member?
              (lambda (a lat)
                (cond
                  ((null? lat) #f)
                  ((eq? (car lat) a) #t)
                  (else (member? a (cdr lat)))))))
      
      (U set1))))


; thus letrec provides a way of hiding functions



; a brief puzzle closes this section.  Can you explain why

(letrec ((f g)
         (g 1)
         (h g))
  (list f g h))

; evaluates to (#<undefined> 1 1)

; while

(letrec ((f (lambda () g))
         (g 1)
         (h g))
  (list (f) g h))

; evaluates to (1 1 1) ?





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; assignment, objects and the environment model of evaluation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; we next introduce state - and assignment.

; you are asked to read sections 3.1 and 3.2 in Abelson & Sussman for an introduction to
; assignment in scheme, which is accomplished using the primitive set! (we pronounce this
; as 'set-bang')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; let's begin by trying to develop a counter -- we want somehow to have a function
; which remembers how many times it has been called

; what about this?


(define (counter)
  (let ((count 0))
    (begin
      (set! count (+ count 1))
      count)))


; as you can see from experimentation, successive calls return the same value, 1.

; so we have more thinking to do.  
; can we somehow exploit closures (and, of course, assignment)
; to do the job we have in mind?

; consider

(define (make-counter)
  (define count 0)
  (define (increment)
    (begin
      (set! count (+ count 1))
      count)) 
  increment)


; now (make-counter) returns a closure -- <closure count increment> -- and calling
; this closure should do the job

; we name the closure

(define f (make-counter))

; and ask you now to compute (f), and then again (f), and then again (f) ... returning
; in sequence 1, 2, 3 ... 

; it seems we have a counter!  


; to get an intuitive understanding, some further experiments are called for

; try 

(define g (make-counter))

; and convince yourself that f and g name different closures, each with its own private copy
; of count

; observe that (define count 96) at the top level has no impact on the values returned by either
; (f) or (g)


; this is a good place to display the use of letrec, as make-counter can be written

(define make-counter
  (lambda ()
    (letrec (
             (count 0)
             (increment (lambda () 
                          (begin
                            (set! count (+ count 1))
                            count)))
             )
      
      increment)))


; yet another way

(define (make-counter)
  (let (( count 0))
    (define (increment)
      (begin
        (set! count (+ count 1))
        count)) 
    increment))




; this more involved example is from Chapter 3 of Abelson and Sussman:


(define (make-account)
  
  (define balance 0)
  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        (error "insufficient funds")))
  
  (define (deposit amount)
    (begin
      (set! balance (+ balance amount))
      balance))
  
  (define (dispatch m)
    (cond ((eq? m 'balance) balance)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "unimplemented method"))))
  
  dispatch)


; here you will observe that the value returned by (make-bank-account) is 
; the closure, dispatch.  If  

(define myaccount (make-account))

(define youraccount (make-account))

; you should experiment to confirm the independence of the balances.  Observe
; that typical usage here would be

((myaccount 'deposit) 20)

((myaccount 'withdraw) 12)


; what is the relation between make-account and your understanding of classes?  
; between myaccount and your understanding of objects? 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ; introduction at board to the environment model of evaluation - abelson & sussman, chapter 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; explanation of make-counter in terms of the environment model

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; explanation of make-account in terms of the environment model --

; we draw a few diagrams on the board -- if you have
; trouble reproducing them, come talk to me: we'll work them out again together






