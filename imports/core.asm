################################################################################
# ASM core module.
####################

## Some basics:

# Generalized math operators:
# TODO

(var * mult)
(var / div)
(var modulo mod)
(var - sub)
(var + add)
(var ^ pow)

(var >= geq?)
(var <= leq?)
(var = equal?)
(var > (lambda (x y) (not (<= x y))))
(var < (lambda (x y) (not (>= x y))))

# Convinient if then else form.
(var if* if)

(macro if [@tuple args]
  `(if* ($(first args) $(second args))
        (else $(third args))))

(var else 'totally-not-fnord)

# Common escape sequences.
(var (\newline \tab \space \dollar \return \backslash \quote)
     (var (\n    \t   \s  \d   \r   \b   \q)
          '("\n" "\t" " " "\$" "\r" "\\" "\"")))

# Convinient Scope.call():
(macro __scopecall [arg]
  `(get '$arg))

# Convinient Set.call(): # FIXME: Doesn't work.
#(macro __setcall [s .tuple predicates]
  `(select $s $(reduce compose predicates)))

# Convinient Vector.call():
(macro __vectorcall [l @tuple indeces]
  (if (rest indeces)
      `(map (lambda [i] (nth $l i)) '$(vectorof indeces))
      `(nth $l $(first indeces))))

# Convinient Vector.eval()
(macro __vectoreval [@tuple elements]
  (do (var args '[])
      (var body '[])
      (function dispatch (had->? arg)
        (if (equal? arg '->)
            'yup
            (do (if had->?
                    (push! arg body)
                    (push! arg args))
                had->?)))
      (reduce dispatch (join () elements))
      (if (empty? body)
          (error "Function body cannot be empty.")
          `(lambda $(reverse args) $(append '(do fnord) (reverse body))))))

## Convinience macros:

# Function declarator.
(macro function [name args body]
  `(var $name (lambda $args $body)))

# Function template declarator.
(macro template [name targs args body]
  `(macro $name $targs
    (lambda $args $body)))

# Package declarator.
(macro package [name @tuple body]
  `(var $name $(join 'scope body)))

# Class declarator.
(macro class [name body]
  (do (var statics '{})
      (var locals '{})
      (function dispatch (arg)
        (when (tuple? arg)
              (if* ((equal? 'static (first arg))
                     (push! (rest arg) statics))
                  (else
                     (push! arg locals)))))
      (map dispatch body)
      `(var $name
            (scope $(append '(do fnord) (tupleof statics))
                   (function new []
                     (scope $(append '(do fnord) (tupleof locals))))))))

# Unittest macro:
(macro unittest [name @tuple body]
  `(catch $(append '(do ()) body)
          [e -> (write "Unittest `" $name "' failure: `" e "'.\n")
                (quit)]))
# Loops:

# Infinite loop.
(macro loop [@tuple body]
  (append '(do @while 1) body))

# Boolean operations and conditionals:
(macro not [a]
  `(if $a fnord 'yup))

(macro and [a b]
  (do (var __a a)
  `(if $__a (if $b $__a))))

(macro or [a b]
  (do (var __a a)
      (var __b b)
      `(if $__a $__a (if $__b $__b))))

(macro unless [condition @tuple body]
  `(if $condition
       fnord
       $(append '(do fnord) body)))

(macro when [condition @tuple body]
  `(if $condition
       $(append '(do fnord) body)))

# Other macros:

(macro scoped [body]
  `((lambda [] $body)))

## Convinience functions:

# Collection manipulation:
(function second [c]
  (nth c 1))

(function third [c]
  (nth c 2))

(function fourth [c]
  (nth c 3))

(function fifth [c]
  (nth c 4))

(function empty? [coll]
  (if coll
      (equal? (first coll) (rest coll) fnord)
      'yup))

(function length? [coll]
  ((lambda [coll len]
     (if (empty? coll)
         len
         (self (rest coll) (+ 1 len))))
   coll 0))

(function member? [el coll]
  (if coll
      (if (equal? el (first coll))
          coll
          (member? el (rest coll)))))

(function push! [what where]
  (set! where (join what where)))      # Either prepends element to a collection
                                       # or joins two elements.

(function pop! [where]
  (if (collection? where)
      (do (var tmp (first where))
          (set! where (rest where))
           tmp)))

(function reverse [l]
  (when l
        (if (rest l)
            (append (reverse (rest l))
                    (vector (first l)))
            l)))

(function assoc [key alist]
  (if (and alist (first alist))
      (if (equal? key (first (first alist)))
          (first alist)
          (assoc key (rest alist)))))

# Predicates:
(function fnord? [obj]
  (not obj))

# Type predicate template:
(template typePredicate [type] [object]
  (if (member? $type (typeof object)) object))

(macro __definePredicates [predicates]
  (do (function makeDecl [arg]
        `(var $(first arg) (typePredicate $(second arg))))
      (append '(do)
              (map makeDecl predicates))))

(__definePredicates
  ((immutable? immutable)
   (pure? pure)
   (builtin? builtin)
   (atom? atom)
   (callable? callable)
   (collection? collection)
   (number? number)
   (symbol? symbol)
   (string? string)
   (function? function)
   (keyword? keyword)
   (scope? scope)
   (set? set)
   (tuple? tuple)
   (vector? vector)))

(macro settable? [arg]
  `(if (member? 'settable (typeof $arg))
       $arg))

# Other functions:

(function swap [this that]
  (do (var __this this)
      (set! this that)
      (set! that __this)))

(function compose [f g]
  (lambda [x]
    (f (g x))))

(function repeat [f]
  (compose f f))

(function fact [n]
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))

(function abs [n]
  ((if (> n 0) + -) 0 n))

(function combine [f]
  (lambda [x y]
    (if (not (fnord? x))
        (join (f (first x) (first y))
              ((combine f) (rest x) (rest y))))))

(var zip (combine join))

(function zipWith [f a b]
  ((combine f) a b))

(function zipOne (element list)
  (if list
      (join element
            (join (first list)
                  (zipOne element
                          (rest list))))))

(function riff-shuffl [deck]
  (do (function take [n seq]
        (if (> n 0)
            (join (first seq)
                   (take (- n 1) (rest seq)))))
      (function drop [n seq]
        (if (<= n 0)
            seq
            (drop (- n 1) (rest seq))))
      (function mid [seq]
        (/ (length? seq) 2))
      ((combine append) (take (mid deck) deck)
                        (drop (mid deck) deck))))

(function str-join [strings]
  (reduce append strings))