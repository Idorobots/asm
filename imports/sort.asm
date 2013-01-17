# A Merge sort

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

(function sort-list (predicate list)
  (do (var len (length? list))
      (if (= len 1)
          list
          (apply merge (join predicate
                             (map (lambda (l)
                                    (sort-list predicate l))
                                  (split (/ len 2) list '())))))))