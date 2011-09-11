################################################################################
# Lispy macros:
####################

(import 'imports.core) # TODO: Make this implicit.

(macro labels [funcs body]
  `(scoped
    (do $(join 'do
               (map (lambda [func]
                      (join 'function pair))
                    funcs))
        $body)))

(macro let* [vars body]
  `(scoped
    (do $(join 'do
               (map (lambda [pair]
                      (join 'var pair))
                    vars))
        $body)))

(var cons join)
(var car first)
(var cdr rest)
(var define var)
(var begin do)
(var defun function)
(var defmacro macro)
