#lang scheme

; First sections examples and exercises, in no special order:

(define (abs x) (if (< x 0) (- x) x))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

 (define (abs1 x) (cond ((< x 0) (- x)) (true
                                   x)))
 
 (define (sum-squares-largest x y z) 
   (cond ((and (< x y) (< x z)) (+ (* y y) (* z z))) 
         ((and (< y x) (< y z)) (+ (* x x) (* z z))) 
         (else (+ (* x x) (* y y))))) 
 
 (define (a-plus-abs-b a b) ((if (> b 0) + -) a b))
 
 (define (p) (p))
 
 
 (define (test x y) (if (= x 0) 0 (delay y)))

 ;;;;;;;;;; Newton's method for square roots, in scheme; Scala version below:
 
 (define (sqr-iter guess x) (if (good-enough? guess x) guess (sqr-iter (improve guess x) x)))
 
 (define (improve guess x) (average guess (/ x guess)))
 
 (define (average x y) (/ (+ x y) 2))
 
 (define (good-enough? guess x) (< (/ (abs (- (* guess guess) x)) x) 0.001))
 
 (define (sqrt x) (sqr-iter 1.0 x))

 ;;;;;;;;;;
 
 (define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
 
 (define (cube-root x) (cube-iter 1.0 x))
 
 (define (cube-iter guess x) (if (cube-good-enough? guess x) guess (cube-iter (cube-improve guess x) x)))
 
 (define (cube-improve guess x) (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
 
 (define (cube-good-enough? guess x) (< (/ (abs (- (cube guess) x)) x) 0.001))

 (define (sqrt-new x)
   (define (good-enough? guess) (< (/ (abs (- (* guess guess) x)) x) 0.001))
   (define (sqr-iter guess) (if (good-enough? guess) guess (sqr-iter (improve guess))))
    (define (improve guess) (average guess (/ x guess)))
   
     (sqr-iter 1.0)
   )
 (define (factorial n)
   (define (fact-iter product counter) (if (= counter n) (* product counter) 
                                                   (fact-iter (* product counter) 
                                                    (+ counter 1))))
   
   (fact-iter 1 1))
 
 ; (define (fact-iter product counter max-count) (if (> counter max-count) product 
 ;                                                 (fact-iter (* product counter) 
 ;                                                 (+ counter 1) max-count)))
                                                                                  
 ;   exercise 1.9:
 ;   (+ 4 5)
 ;   (inc (+ (dec 4) 5))
 ;   (inc (inc (+ (dec 3) 5)))
 ;   (inc (inc (inc (+ (dec 2) 5))))
 ;   (inc (inc (inc (inc (+ (dec 1) 5)))))
 ;   (inc (inc (inc (inc 5))))
 ;   (inc (inc (inc 6)))
 ;   (inc (inc 7))
 ;   (inc 8)
 ;    9
 ;    The process is recursive.
 ;
 ;   (+ 4 5)
 ;   (+ (dec 4) (inc 5))
 ;   (+ (dec 3) (inc 6))
 ;   (+ (dec 2) (inc 7))
 ;   (+ (dec 1) (inc 8))
 ;   (+ 0 9)
 ;   9
 ;   The process is iterative.
 ;
 ;
 ; exercise 1.10
 ; Ackermann's function:
 (define (A x y) (cond ((= y 0) 0) 
                       ((= x 0) (* 2 y))
                       ((= y 1) 2)
                       (else (A (- x 1) (A x (- y 1))))))
 
 ; (A 1 10)
 ; (A (- 1 1) (A 1 (- 10 1)))
 ; (A 0 (A 1 9)) 
 ; (* 2 (A 1 9))
 ; (* 2 (A (- 1 1) (A 1 (- 9 1))))
 ; (* 2 (A 0 (A 1 8)))
 ; (* 2 (* 2 (A 1 8)))
 ; => (* 2 (* 2 (* 2 (A 1 7))))
 ; => (* 2 (* 2 (* 2 (* 2 (A 1 6)))))
 ; => (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 5))))))
 ; => (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 4)))))))
 ; => (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 3))))))))
 ; => (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 2)))))))))
 ; => (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 1))))))))))
 ; => (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2)))))))))
 ; => (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 4))))))))
 ; => (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 8)))))))
 ; => (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 16))))))
 ; => (* 2 (* 2 (* 2 (* 2 (* 2 32)))))
 ; => (* 2 (* 2 (* 2 (* 2 64))))
 ; => (* 2 (* 2 (* 2 128)))
 ; => (* 2 (* 2 256))
 ; => (* 2 512)
 ; => 1024
 ;
 ; 
 ;    (A 2 4)
 ; => (A 1 (A 2 3)) 
 ; => (A 0 (A 1 (A 2 2)))
 ; => (* 2 (A 1 (A 2 2)))
 ; => (* 2 (A 0 A )
 ; => (* 2 (* 2 (A 1 1)))
 ; => (* 2 (* 2 2))
 ; => (* 2 4)
 ; => 8
 ;
 ;
 
 (define (fib n) (fib-iter 1 0 n))
 
 (define (fib-iter a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))
 
 ; Exercise 1.11 
 
 ; Recursive Version:
 
 (define (f-1.11-recursive n) (cond ((< n 3) n) (else (+ (f-1.11-recursive (- n 1)) (* 2 (f-1.11-recursive (- n 2))) (* 3 (f-1.11-recursive (- n 3)))))))
 
 ; Iterative Version:
 
 (define (f-1.11 n) (f-1.11-iter 2 1 0 n))
 
 (define (f-1.11-iter a b c count) (if (= count 0) c (f-1.11-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
 
 ;
 ; 
 
 ; Exercise 1.12:
 
 ; Inputs are the row and column for the desired entry; output is the number in Pascal's triangle
 ; at that row/column
 ; Scheme Version:
 
  (define (pascal col row) (cond ((= row col) 1) ((= col 0) 1) 
                                    (else (+ (pascal col (- row 1)) (pascal (- col 1) (- row 1))))))
 ;
 ;
  
 ; Scala Version:
 
 ; def pascal(c: Int, r: Int): Int = if (r == c || c == 0) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)
  
 
 ;
 ;
 ; Exercise 1.13
 ;
 ; 
 ;
 ;
  
  ; Section 1.3.3

  ; Scala version of function to find fixed points of functions:
; val tolerance = 0.0001

; def isCloseEnough(x: Double, y: Double) =

; abs((x - y) / x) / x < tolerance

; def fixedPoint(f: Double => Double)(firstGuess: Double) = {

; def iterate(guess: Double): Double = {

; val next = f(guess)

; if (isCloseEnough(guess, next)) next

; else iterate(next)

; }

; iterate(firstGuess)

; }
  
  
  ; square-roots with average-damping and fixed points, in Scala:
  ;
  ; def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2
  ;
  ; def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1.0)
  ;
  ;
  ; Newton's Method for finding roots of functions, in Scala:
  ;
  ;
   ;   def is_close_enough(x: Double, y: Double) =
    ;     (abs_Double(x - y) < 0.001);
;
 ;     def search(f: Double=>Double, negative: Double, positive: Double): Double = {
  ;       val midpoint = average(negative, positive);
   ;      if (close_enough(negative, positive))
    ;        midpoint
     ;       else {
      ;         val test_value = f(midpoint);
       ;        if (test_value > 0)
        ;          search(f, negative, midpoint)
         ;         else if (test_value < 0) search(f, midpoint, pos_point)
          ;        else midpoint;
           ; }
      ;}

      ;def half_interval(f: Double=>Double, a: Double, b: Double) = {
       ;  val val_a = f(a);
        ; val val_b = f(b);
         ;if (val_a < 0 && val_b > 0)
          ;  search(f, a, b)
           ; else if (val_b < 0 && val_a > 0) search(f, b, a)
            ;else -1);
      ;}
  ;
  ;
  ;
  ; Newton's method, in Scheme:
  (define (search f negative positive)
       (let((midpoint(average negative positive)))
        (if close? negative positive)
        midpoint 
        (let ((test (f midpoint))))
        (cond ((positive? test) (search f negative midpoint))  
        ((negative? test)  (search f midpoint positive))
        (else midpoint))))

         (define (close-enough? x y) 
	 (< (abs (- x y)) 0.001  ))



         (define (poly-root f a b) (let (( a-value (f a))  
	                             (b-value (f b)))
	                       (cond ((and (negative? a-value) (positive? b-value)  

	                       	) (search f a b))
	                       ((and (negative b-value) (positive? a-value)) 
	                       	(search f a b))
		                        

	                       	)  

	)
 
	)
  ; Final section chapter 1 examples and exercises, in Scala:

  ; def sumInts(a: Int, b: Int): Int =
  ;  if (a > b) 0 else a + sumInts(a + 1, b)
   

   ; def cube(x: Int): Int = x * x * x
   
   ; def sumCubes(a: Int, b: Int): Int =
   ;if (a > b) 0 else cube(a) + sumCubes(a + 1, b) 

   ; And the generalization of the above functions:

   ; def sum(f: Int => Int)(a: Int, b: Int): Int =
   ; if (a > b) 0 else f(a) + sum(f)(a + 1, b)


   ; And the tail-recursive version:

   ; def sum(f: Int => Int)(a: Int, b: Int): Int = {
   ; def loop(a: Int, acc: Int): Int = {
   ; if (a > b) acc
   ; else loop(a + 1, f(a) + acc)
   ;      }
   ;loop(a, 0)
   ;     }


