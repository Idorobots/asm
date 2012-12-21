# Y combinator in ASM

(var Y (lambda (f)
         ((lambda (x) (x x))
          (lambda (g)
            (f (lambda (x)
                 ((g g) x)))))))

(var factorial (Y (lambda (fac)
                    (lambda (x)
                      (if (equal? x 0)
                          1
                          (* x (fac (- x 1))))))))

(var fib (Y (lambda (fib)
              (lambda (x)
                (if (< x 2)
                    x
                    (+ (fib (- x 1)) (fib (- x 2))))))))

(function fib2 (x)
  (if (< x 2)
      x
      (+ (fib2 (- x 1)) (fib2 (- x 2)))))

(write (factorial 1) " " (factorial 3) " " (factorial 5) " " factorial "\n")

(fib 20)