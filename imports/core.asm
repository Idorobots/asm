################################################################################
# ASM core module.
####################

## Some basics:

# Convinient Scope.call.
(macro __scopecall [arg]
  `(get '$arg))

## Convinience macros:

# Function declarator.
(macro defun [name args body]
  `(var $name (function $args $body)))

# Function template declarator.
(macro template [name targs args body]
  `(macro $name $targs
    (function $args $body)))

# Package declarator.
(macro package [name @tuple]
  `(var $name $(join! 'scope @tuple)))

# Class declarator.
(macro class [name body]
  (do (var statics '{})
      (var locals '{})
      (defun dispatch [arg]
        (if (tuple? arg)
            (if (equal? 'static (first arg))
                (push (rest arg) statics)
                (push arg locals))))
      (map dispatch body)
      `(var $name
            (scope $(join! 'do (tupleof statics))
                   (defun new []
                     (scope $(join! 'do (tupleof locals))))))))

# Binds variables allowing for lazy evaluation:
(macro bind [sym obj]
  (if (in? 'lazy (keywordsof sym))
      `(var $sym '$obj)
      `(var $sym $obj)))

# Multiple symbol binder:
(macro alias [object @tuple]
  (if (rest @tuple)
      (do (var 1st `(var $(first @tuple) $object))
          (defun makeAlias [t] `(var $t $(first @tuple)))
          (join! 'do
                 (join! 1st (map makeAlias (rest @tuple)))))
  #else `(var $(first @tuple) $object)))


# Boolean operations:
(macro not [a]
  `(if $a fnord 'yup))

(macro and [a b]
  (do (var __a a)
      `(if $__a (if $b $__a))))

(macro or [a b]
  (do (var __a a)
      (var __b b)
      `(if $__a $__a (if $__b $__b))))

# Other macros:

# Lispy macros:

(macro let* (vars body)
  `((function []
    (do $(join! 'do
                (map (function [pair]
                       (join! 'var pair))
                     vars))
        $body))))

(var define var)
(var lambda function)
(var begin do)

## Convinience functions:

# Collection manipulation:
(defun second [c]
  (first (rest c)))

(defun third [c]
  (first (rest (rest c))))

(defun fourth [c]
  (first (rest (rest (rest c)))))

(defun fifth [c]
  (first (rest (rest (rest (rest c))))))

(defun in? [el coll]
  (reduce (function [result arg]
            (if (fnord? result)
                # In case of (in? fnord collection)
                (if (equal? arg el) 'yup)
                result))
          (join! fnord coll))) #TODO

(defun push [what where]
  (if (settable? where)
      (set! where (join! what where))))

(defun pop [where]
  (if (settable? (collection? where))
      (do (var tmp (first where))
          (set! where (rest where))
           tmp)))

(defun assoc [key alist]
  (if (not (fnord? alist))
      (if (equal? key (first (first alist)))
          (second (first alist))
          (assoc key (rest alist)))))

# Predicates:
(defun fnord? [obj]
  (not obj))

# Type predicate template:
(template typePredicate [type] [object]
  (if (in? $type (typeof object)) object))

(macro __definePredicates [predicates]
  (do (defun makeDecl [arg]
        `(var $(first arg) (typePredicate $(second arg))))
      (join! 'do (map makeDecl predicates))))

(__definePredicates
  ((immutable? immutable)
  (settable? settable)        # FIXME: This doesn't work as it should.
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
  (list? list)))

# Other functions:

(defun swap [this that]
  (do (var __this this)
      (set! this that)
      (set! that __this)))

(defun twice [x]
  (* 2 x))

(defun compose [f g]
  (function [x]
    (f (g x))))

(defun repeat [f]
  (compose f f))

(defun fact [n]
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))

(defun abs [n]
  ((if (> n 0) + -) 0 n))

(defun combine [f]
  (function [x y]
    (if (not (fnord? x))
        (f (f (first x) (first y))
           ((combine f) (rest x) (rest y))))))

(var zip (combine join!))

(defun riff-shuffl [deck]
  (do (defun take [n seq]
        (if (> n 0)
            (join! (first seq)
                   (take (- n 1) (rest seq)))))
      (defun drop [n seq]
        (if (<= n 0)
            seq
            (drop (- n 1) (rest seq))))
      (defun mid [seq]
        (/ (length seq) 2))
      ((combine append) (take (mid deck) deck)
                        (drop (mid deck) deck))))
