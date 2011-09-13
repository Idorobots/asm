################################################################################
# ASM core module.
####################

## Some basics:

# Convinient Scope.call():
(macro __scopecall [arg]
  `(get '$arg))

# Convinient Set.call(): # FIXME: Doesn't work.
#(macro __setcall [s .tuple predicates]
  `(select $s $(reduce compose predicates)))

# Convinient Vector.call():
(macro __vectorcall [l .tuple indeces]
  (if (rest indeces)
      `(map (lambda [i] (nth $l i)) '$(vectorof indeces))
      `(nth $l $(first indeces))))

# Convinient Vector.eval()
(macro __vectoreval [.tuple elements]
  (do (var args '[])
      (var body '[])
      (function dispatch [had->? arg]
        (if (equal? arg '->)
            'yup
            (do (if had->?
                    (push! arg body)
                    (push! arg args))
                had->?)))
      (reduce dispatch (join () elements))
      `(lambda $(reverse args) $(append '(do fnord) (reverse body)))))

## Convinience macros:

# Function declarator.
(macro function [name args body]
  `(var $name (lambda $args $body)))

# Function template declarator.
(macro template [name targs args body]
  `(macro $name $targs
    (lambda $args $body)))

# Package declarator.
(macro package [name .tuple body]
  `(var $name $(join 'scope body)))

# Class declarator.
(macro class [name body]
  (do (var statics '{})
      (var locals '{})
      (function dispatch [arg]
        (if (tuple? arg)
            (if (equal? 'static (first arg))
                (push! (rest arg) statics)
                (push! arg locals))))
      (map dispatch body)
      `(var $name
            (scope $(append '(do fnord) (tupleof statics))
                   (function new []
                     (scope $(append '(do fnord) (tupleof locals))))))))

# Binds variables allowing for lazy evaluation:
(macro bind [sym obj]
  (if (member? 'lazy (keywordsof sym))
      `(var $sym (lambda [] $obj))
      `(var $sym $obj)))

# Multiple symbol binder:
(macro alias [object .tuple aliases]
  (if (rest aliases)
      (do (var 1st `(var $(first aliases) $object))
          (function makeAlias [t]
            `(var $t $(first aliases)))
          (append '(do fnord)
                 (join 1st (map makeAlias (rest aliases)))))
  #else `(var $(first aliases) $object)))

# Loops:

# Infinite loop.
(macro loop [.tuple body]
  (append '(do .while 1) body))

# While loop.
#(macro while [condition .tuple body]
  (append `(do $.while condition) body))

# Until loop.
#(macro until [condition .tuple body]
  (append `(do $.until condition) body))

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

(macro unless [condition .tuple body]
  `(if $condition
       fnord
       $(append '(do fnord) body)))

(macro when [condition .tuple body]
  `(if $condition
       $(append '(do fnord) body)))

# Other macros:

(macro scoped [body]
  `((lambda [] $body)))

## Convinience functions:

# Collection manipulation:
(function second [c]
  (first (rest c)))

(function third [c]
  (first (rest (rest c))))

(function fourth [c]
  (first (rest (rest (rest c)))))

(function fifth [c]
  (first (rest (rest (rest (rest c))))))

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
      (join 'do (map makeDecl predicates))))

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
        (f (f (first x) (first y))
           ((combine f) (rest x) (rest y))))))

(var zip (combine join))

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