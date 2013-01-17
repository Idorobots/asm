# Y combinator in ASM

(var I (lambda (x) x))

(var M (lambda (x) (x x)))

(var B (lambda (x)
         (lambda (y)
           (lambda (z)
             (x (y z))))))

(var B2 (lambda (x y)
          (lambda (z)
            (x (y z)))))

(var B3 (lambda (x y z)
          (x (y z))))

(var L (lambda (x)
         (lambda (y)
           (x (M y)))))

(var L (lambda (x)
         ((B x) M)))

(var Me (lambda (x)
          (lambda (y)
            ((M x) y))))

(var Y (lambda (f)
         ((lambda (x) (f (x x)))
          (lambda (x) (f (x x))))))

(var Y ((B M) L))

(var Z (lambda (f)
         ((lambda (x) (x x))
          (lambda (g)
            (f (lambda (x)
                 ((g g) x)))))))

(var Zc (lambda (f)
         (M (B2 f Me))))

(macro recursive (f)
  `(M (lambda (g)
        ($f (lambda (x)
              ((g g) x))))))

(var factorial (Z (lambda (fac)
                    (lambda (x)
                      (if (equal? x 0)
                          1
                          (* x (fac (- x 1))))))))

# Native
(var fib (lambda (x)
           (if (< x 2)
               x
               (+ (fib (- x 1))
                  (fib (- x 2))))))

# Z-combined
(var fib0 (Z (lambda (fib)
               (lambda (x)
                 (if (< x 2)
                     x
                     (+ (fib (- x 1))
                        (fib (- x 2))))))))

# Zc-combined
(var fib1 (Zc (lambda (fib)
                (lambda (x)
                  (if (< x 2)
                      x
                      (+ (fib (- x 1))
                         (fib (- x 2))))))))

# Macroed
(var fib2 (recursive
           (lambda (fib)
             (lambda (x)
               (if (< x 2)
                   x
                   (+ (fib (- x 1))
                      (fib (- x 2))))))))

# Native+2
(var fib3 (((lambda ()
              (lambda ()
                (lambda (x)
                  (if (< x 2)
                      x
                      (+ (fib3 (- x 1))
                         (fib3 (- x 2))))))))))

(write (factorial 1) " " (factorial 3) " " (factorial 5) " " factorial "\n")

(fib3 23)