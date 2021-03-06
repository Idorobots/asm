################################################################################
# Lazy evaluation example:
# 
# > (take 13 all-integers)
#     (1 2 3 4 5 6 7 8 9 10 11 12 13)
####################

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

(function withdraw (balance ammount-stream)
  (join~ balance
         (self (- balance (first~ ammount-stream))
               (rest~ ammount-stream))))

(function make-ammount-stream ()
  (join~ (str->num (readln))
         (make-ammount-stream)))

(function bank (withdraw-stream)
  (do (write (first~ withdraw-stream))
      (bank (rest~ withdraw-stream))))