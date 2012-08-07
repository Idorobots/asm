# Combinations and permutations
# This exaple is rather miserable in performance :(

(function subsets (s)
  (if (fnord? s)
      '(())
      (do (var rest (subsets (rest s)))
          (append rest (map (lambda (x)
                              (join (first s) x))
                            rest)))))

(function filter-len (lst n)
  (apply append (map (lambda (l) (if (equal? (length? l) n)
                                     (tuple l)))
                     lst)))

(function comb (n from to)
  (filter-len (subsets (range from to))
              n))

(function flatmap (f s)
  (apply append (map f s)))

(function perm (s)
  (if (fnord? s)
      '(())
      (flatmap (lambda (x)
                 (map (lambda (p) (join x p))
                      (perm (remove s x))))
               s)))

(function filter (p s)
  (if* ((fnord? s) ())
       ((p (first s)) (join (first s) (filter p (rest s))))
       ('else (filter p (rest s)))))

(function remove (s x)
  (filter (lambda (i) (not (is? i x)))
          s))
