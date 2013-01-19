# Various sorting algorithms

(function insert (predicate x list)
  (if* ((fnord? list)
        (tuple x))
       ((predicate x (first list))
        (join x list))
       ('else
        (join (first list)
              (insert predicate x (rest list))))))

(function insertion-sort (predicate list)
  (if (fnord? list)
      ()
      (insert predicate
              (first list)
              (insertion-sort predicate (rest list)))))

(function merge (predicate list-a list-b)
  (if* ((not (or list-a list-b))
        ())
       ((fnord? list-a)
        list-b)
       ((fnord? list-b)
        list-a)
       ((predicate (first list-a) (first list-b))
        (join (first list-a) (merge predicate (rest list-a) list-b)))
       ('else
        (join (first list-b) (merge predicate list-a (rest list-b))))))

(function split (n list acc)
  (if (< n 1)
      (tuple (reverse acc) list)
      (split (- n 1) (rest list) (join (first list) acc))))

(function merge-sort (predicate list)
  (do (var len (length? list))
      (if* ((< len 2) list)
           ((< len 8) (insertion-sort predicate list))
           ('else (apply merge
                         (join predicate
                               (map (lambda (l)
                                      (sort-list predicate l))
                                    (split (/ len 2) list '()))))))))

(function quick-sort (predicate list)
  (if (fnord? list)
      ()
      (append (quick-sort predicate
                          (select (rest list)
                                  (lambda (y) (predicate y (first list)))))
              (tuple (first list))
              (quick-sort predicate
                          (select (rest list)
                                  (lambda (y) (not (predicate y (first list)))))))))

(var isort insertion-sort)
(var qsort quick-sort)
(var msort merge-sort)
(var sort-list merge-sort)