(import 'imports.lazy)

(function ++ [var]
  (+ 1 var))

(var all-integers ([n ->
                      (join~ n (self (++ n)))]
                   1))

(function squareduce [n]
  (reduce [a b ->
               (+ (* a a) (* b b))]
          (take n (map~ [x -> (* x x)]
                        all-integers))))