################################################################################
# ASM lazy stuff.
####################

(macro join~ [a b]
  `(lazy (join! $a $b)))

(function first~ [.lazy x]
  (first (x)))

(function rest~ [.lazy x]
  (second (x)))

(function fnord~ []
  (lazy fnord))

(function fnord?~ [.lazy x]
  (fnord? (x)))

(function make-integers [n]
  (join~ n (make-integers (+ 1 n))))

(var .lazy N (make-integers 1))

(function make-lazy (lst)
  (lazy (when lst
              (join! (first lst) (make-lazy (rest lst))))))

(function take (n .lazy lst)
  (unless (or (equal? n 0) (fnord?~ lst))
          (join! (first~ lst) (take (- n 1) (rest~ lst)))))

(function take-all (.lazy lst)
  (unless (fnord?~ lst)
          (join! (first~ lst) (take-all (rest~ lst)))))


(function map~ (f l)
  (lazy (unless (fnord?~ l)
                (join! (f (first~ l))
                      (map~ f (rest~ l))))))
