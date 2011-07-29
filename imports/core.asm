################################################################################
# ASM core module.
####################

## Convinience macros:

# Function declarator.
(macro defun [name args body]
    `(var $name (function $args $body)))

# Function template declarator.
(macro template [name targs args body]
    `(macro $name $targs
        (function $args $body)))

# Class declarator.
(macro class [name body]
    (begin (var statics '{})
           (var locals '{})
           (defun dispatch [arg]
               (if (tuple? arg)
                   (if (equal? 'static (first arg))
                       (push (rest arg) statics)
                       (push arg locals))))
           (map dispatch body)
           `(var $name
                 (scope $(join! 'begin (tupleof statics))
                        (defun new []
                            (scope $(join! 'begin (tupleof locals))))))))

# Binds variables allowing for lazy evaluation:
(macro bind [sym obj]
    (if (in? 'lazy (keywordsof sym))
        `(var $sym '$obj)
        `(var $sym $obj)))

# Multiple symbol binder:
(macro alias [object @tuple]
    (if (rest @tuple)
        (begin
            (var 1st `(var $(first @tuple) $object))
            (defun makeAlias [t] `(var $t $(first @tuple)))
            (join! 'begin
                   (join! 1st (map makeAlias (rest @tuple)))))
    #else `(var $(first @tuple) $object)))


# Boolean operations:
(macro not [a]
    `(if $a fnord 'yup))

(macro and [a b]
    (begin (var __a a)
           `(if $__a (if $b $__a))))

(macro or [a b]
    (begin (var __a a)
           (var __b b)
           `(if $__a $__a (if $__b $__b))))

# Other macros:
(macro set [@tuple]  # TODO: Turn it into a function.
    `'$(setof @tuple))
(macro list [@tulpe]
    `'$(listof @tuple))

# Lispy macros:

(macro let* (vars body)
    `((function []
        (begin $(join! 'begin
                       (map (function [pair] (join! 'var pair)) vars))
               $body))))

(var lambda function)

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

(defun push (what where)
    (if (settable? where)
        (set! where (join! what where))))

# Predicates:
(defun fnord? [obj]
    (not obj))

# Type predicate template:
(template typePredicate [type] [object]
    (if (in? $type (typeof object)) object))

(var immutable? (typePredicate immutable))
(var settable? (typePredicate settable))
(var pure? (typePredicate pure))
(var builtin? (typePredicate builtin))
(var atom? (typePredicate atom))
(var callable? (typePredicate callable))
(var collection? (typePredicate collection))
(var number? (typePredicate number))
(var symbol? (typePredicate symbol))
(var string? (typePredicate string))
(var function? (typePredicate function))
(var keyword? (typePredicate keyword))
(var scope? (typePredicate scope))
(var set? (typePredicate set))
(var tuple? (typePredicate tuple))
(var list? (typePredicate list))

# Other functions:
