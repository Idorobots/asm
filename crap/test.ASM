(macro and (a b)
    '(if $a (if $b $a)))

(macro not (a)
    '(if a ))

(macro makeGeneric (name primitive)
    '(var $name (function (...)         ## All arguments will be wrapped in a list.
        (if (and ... (rest ...))        ## If there actually are any arguments...
            (reduce $primitive ...))))) ## Reduce them using a primitive.

(makeGeneric * mult)
(makeGeneric + add)
(makeGeneric - sub)
(makeGeneric / div)
(makeGeneric % mod)

(macro defun (name args body)
    '(var $name (function $args $body)))

(write "Hello world! $*")
