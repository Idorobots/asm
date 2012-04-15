# Scheme style monads in ASM.

(function make-numbered-value (tag val)
  (tuple tag val))

(function nvalue-tag (tv)
  (first tv))

(function nvalue-val (tv)
  (second tv))

(function return (val)
  (lambda (counter)
    (make-numbered-value counter val)))

(function >>= (m f)
  (lambda (counter)
    (do (var (t v) (m counter))
        (var m1 (f v))
        (m1 t))))

(macro letm (binding expr)
  `(>>= $(second binding)
        (lambda ($(first binding)) $expr)))

(macro letm* (bindings expr)
  ((lambda (bindings)
     (if bindings
         `(letm ($(first (first bindings)) $(second (first bindings)))
                $(self (rest bindings)))
         expr))
   bindings))

(function incr (n)
  (make-numbered-value (+ 1 n) n))

(function run-monad (m counter)
  (m counter))

(function make-node (val kids)
  (letm (counter incr)
        (return (join (make-numbered-value counter val) kids))))

(function build-btree (depth)
  (if (equal? depth 0)
      (make-node depth '())
      (letm* ((left-branch (build-btree (- depth 1)))
              (right-branch (build-btree (- depth 1))))
             (make-node depth (tuple left-branch right-branch)))))

(write (run-monad (build-btree 3) 100))
