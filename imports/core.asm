## ASM core module.

# Declarator macros:
(macro def (name args body)
    `(var $name (function $args $body)))

(macro des (name body)
    `(var $name (scope $body)))

# Some convinience macros:
(def second (c)
    (first (rest c)))

(def third (c)
    (first (rest (rest c))))

(def fourth (c)
    (first (rest (rest (rest c)))))

(def fifth (c)
    (first (rest (rest (rest (rest c))))))
