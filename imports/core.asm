################################################################################
# ASM core module.
####################

## Convinience macros:

# Function declarator.
(macro defun (name args body)
    `(var $name (function $args $body)))

# Function template declarator.
(macro template (name targs args body)
    `(macro $name $targs
        (function $args $body)))

# Class declarator.
# TODO: (static name val) and one block.
(macro class (name static local)
    `(var $name (scope
        $static                 # Static part of the class.
        (defun new ()
            (scope $local)))))  # Local part of the class.

# Lispy macros:

(macro let* (vars body)
    `((function () {
        $(setof (mymap (function (pair)
                         (join 'var pair))
                      vars))
        $body
    })))

## Convinience functions:

(defun second (c)
    (first (rest c)))

(defun third (c)
    (first (rest (rest c))))

(defun fourth (c)
    (first (rest (rest (rest c)))))

(defun fifth (c)
    (first (rest (rest (rest (rest c))))))
