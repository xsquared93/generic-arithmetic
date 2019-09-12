(load "help.scm")
(load "table.scm")

;;; main procedures

;; simplify:
(define (simplify radical)
  (apply-generic 'simplify radical))


(define (add x y)
  (apply-generic 'add x y))

(define (sub x y)
  (apply-generic 'sub x y))

(define (mul x y)
  (apply-generic 'mul x y))

(define (div x y)
  (apply-generic 'div x y))

(define (make-radical coeff rad)
  ((get 'make-radical 'radical) coeff rad))

(define (make-rat numer denom)
  ((get 'make 'fraction) numer denom))

(define (make-integer x)
  ((get 'make 'integer) x))

;;; packages

(define (install-radical-package)
  ;; internal procedures
  
  (define mul-radicals
    (lambda (radicalA radicalB)
      (let ((rad (make-radical (* (coefficient radicalA)
				  (coefficient radicalB))
			       (* (radicand radicalA)
				  (radicand radicalB)))))
	(simplify rad))))

  ;; div-radicals
  
  (define add-radicals
    (lambda (radicalA radicalB)
      (operate radicalA radicalB +)))

  (define sub-radicals
    (lambda (radicalA radicalB)
      (operate radicalA radicalB -)))

  (define operate
    (lambda (radicalA radicalB term)
      (let ((radical1 (simplify radicalA))
	    (radical2 (simplify radicalB)))
	(cond ((same-radicand? radical1 radical2)
	       (make-radical (term (coefficient radical1)
				     (coefficient radical2))
			     (radicand radical1)))
	      ((both-numbers? radical1 radical2)
	       (term radical1 radical2))
	      (else (make-addition radical1 radical2))))))

  (define both-numbers?
    (lambda (x y)
      (and (number? x) (number? y))))

  ;; This procedure simplifies a given radical -- it is based on the
  ;; following 3 observations:
  ;;     -
  ;;     -
  ;;     -
  ;; pair -> number or pair
  (define simplify
    (lambda (radical)
      (define (iter a b max)
	(cond ((>= a max) (make-radical (* 1 b) max))
	      ((perfect-square? max) (* b (sqrt max)))
	      ((= (remainder max a) 0)
	       (let ((n (/ max a)))
		 (if (perfect-square? n)
		     (make-radical (* b (sqrt n)) a)
		     (iter (+ a 1) b max))))
	      (else (iter (+ a 1) b max))))
      (iter 2 (coefficient radical) (radicand radical))))

  (define perfect-square?
    (lambda (n)
      (define (iter a b max)
	(cond ((>= a max) #f)
	      ((= (* a b) max) #t)
	      (else (iter (+ a 1) (+ b 1) max))))
      (iter 1 1 n)))
  
  ;; representation of radicals
  
  (define make-addition
    (lambda (radicalA radicalB)
      (cons radicalA radicalB)))
  
  (define make-radical
    (lambda (coefficient radicand)
      (cons coefficient radicand)))
  
  (define coefficient
    (lambda (radical)
      (car radical)))
  
  (define radicand
    (lambda (radical)
      (cdr radical)))
  
  (define same-radicand?
    (lambda (radicalA radicalB)
      (and (radical? radicalA)
	   (radical? radicalB)
	   (eq? (radicand radicalA) (radicand radicalB)))))
  
  (define radical?
    (lambda (r)
      (pair? r)))

  ;; interface to the rest of the system

  (define tag
    (lambda (x) (attach-tag 'radical x)))
  
  (put 'simplify '(radical)
       (lambda (x) (tag (simplify x))))
  
  (put 'add '(radical radical)
       (lambda (rad1 rad2) (tag (add-radicals rad1 rad2))))
  
  (put 'sub '(radical radical)
       (lambda (rad1 rad2) (tag (sub-radicals rad1 rad2))))
  
  (put 'mul '(radical radical)
       (lambda (rad1 rad2) (tag (mul-radicals rad1 rad2))))
  
  (put 'make-radical 'radical
       (lambda (c r) (tag (make-radical c r))))
  
  'installed!)

(define (install-fractions-package)
  ;; internal procedures
  
  (define mul-fractions
    (lambda (frac1 frac2)
      (operate frac1 frac2 numer denom)))

  (define div-fractions
    (lambda (frac1 frac2)
      (operate frac1 frac2 denom numer)))

  (define operate
    (lambda (frac1 frac2 selectorA selectorB)
      (make-rat (* (numer frac1) (selectorA frac2))
		(* (denom frac1) (selectorB frac2)))))
    
  (define add-fractions
    (lambda (f1 f2)
      (compute f1 f2 +)))
  
  (define sub-fractions
    (lambda (f1 f2)
      (compute f1 f2 -)))
  
  (define compute
    (lambda (frac1 frac2 op)
      (make-rat (op (* (numer frac1) (denom frac2))
		    (* (numer frac2) (denom frac1)))
		(* (denom frac1) (denom frac2)))))

  (define make-rat
    (lambda (numerator denominator)
      (let ((g (gcd numerator denominator)))
	(cons (/ numerator g) (/ denominator g)))))
  
  (define numer
    (lambda (fraction)
      (car fraction)))

  (define denom
    (lambda (fraction)
      (cdr fraction)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'fraction x))

  (put 'add '(fraction fraction)
       (lambda (x y) (tag (add-fractions x y))))

  (put 'sub '(fraction fraction)
       (lambda (x y) (tag (sub-fractions x y))))

  (put 'mul '(fraction fraction)
       (lambda (x y) (tag (mul-fractions x y))))

  (put 'div '(fraction fraction)
       (lambda (x y) (tag (div-fractions x y))))

  (put 'make 'fraction
       (lambda (n d) (tag (make-rat n d))))

  'installed!)

(define (install-matrix-package)
  ;; internal procedures

  (define add-matrices
    (lambda (m1 m2)
      (compute-matrices m1 m2 +)))

  (define subtract-matrices
    (lambda (m1 m2)
      (compute-matrices m1 m2 -)))
  
  (define compute-matrices
    (lambda (matrix1 matrix2 op)
      (map (lambda (x y) (map op x y))
	   matrix1
	   matrix2)))

  ;; interface to the rest of the system
  (define tag
    (lambda (x) (attach-tag 'matrix x)))

  (put 'add '(matrix matrix)
       (lambda (x y) (tag (add-matrices x y))))
  
  (put 'sub '(matrix matrix)
       (lambda (x y) (tag (subtract-matrices x y))))
  
  'installed!)

(define (install-integer-package)
  ;; internal procedures

  ;; interface to the rest of the system
  (define tag
    (lambda (x) (attach-tag 'integer x)))

  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))

  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))

  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))

  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))

  (put 'make 'integer
       (lambda (x) (tag x)))

  'installed)
  

;; install packages
(install-radical-package)
(install-fractions-package)
(install-matrix-package)
(install-integer-package)

;;; tests
(define (check-expect check expect)
  (if (not (equal? check expect))
      (begin (display "check-expect failed; got: ")
	     (display check)
	     (display "; expected: ")
	     (display expect)
	     (newline))
      (begin (display "test passed!")
	     (newline))))
    

(define (tag x) (attach-tag 'radical x))
(define (tag-fraction x) (attach-tag 'fraction x))
(define (tag-matrix x) (attach-tag 'matrix x))
(define (tag-integer x) (attach-tag 'integer x))

(define (tests)

  ;;; testing the generic `add` procedure
  
  ;; radicals
  (define radical1 (make-radical 2 3))
  (check-expect (add radical1 radical1) (tag (cons 4 3)))

  (define rad2 (make-radical 3 4))
  (define rad3 (make-radical 2 4))
  (check-expect (sub rad2 rad3) (tag 2))

  (check-expect (mul rad2 rad3) (tag 24))

  ;; fractions
  (define f1 (make-rat 4 6))
  (define f2 (make-rat 2 3))
  (define f3 (make-rat 2 6))

  (check-expect (add f1 f2) (tag-fraction (cons 4 3)))

  (check-expect (sub f1 f3) (tag-fraction (cons 1 3)))

  (check-expect (mul f1 f2) (tag-fraction (cons 4 9)))

  (check-expect (div f1 f2) (tag-fraction (cons 1 1)))

  ;; matrices
  (define matrix1 (tag-matrix (list (list 2 3 4) (list 3 4 5))))
  (define matrix2 (tag-matrix (list (list 10 15 20) (list 7 8 9))))

  (check-expect (add matrix1 matrix1) (tag-matrix (list (list 4 6 8) (list 6 8 10))))
  (check-expect (sub matrix2 matrix1) (tag-matrix (list (list 8 12 16) (list 4 4 4))))

  ;; integers
  (define int1 (make-integer 8))
  (define int2 (make-integer 5))
  (define int3 (make-integer 4))

  (check-expect (add int1 int2) (tag-integer 13))
  (check-expect (sub int1 int2) (tag-integer 3))
  (check-expect (mul int1 int2) (tag-integer 40))
  (check-expect (div int1 int3) (tag-integer 2))

  'done)

(tests)
