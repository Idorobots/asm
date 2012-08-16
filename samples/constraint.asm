# Yet another SICP inspired example.

# Some helpers:

(macro i () '(import 'samples.constraint))

(var cond if*)
(var cons join)
(var car first)
(var cadr second)
(var cdar (lambda (x) (cdr (car x))))
(var cdr rest)
(var length length?)

# Code starts here:

(function celsius-fahrenheit-converter (c f)
  (do (var (u v w x y) (make-connectors 5))
      (multiplier c w u)
      (multiplier v x u)
      (adder v y f)
      (constant 9 w)
      (constant 5 x)
      (constant 32 y)
      'ok))

(function adder (a b s)
  (do (function process-new-value ()
        (cond ((and (has-value? a)
                    (has-value? b))
               (set-value! s
                           (+ (get-value a)
                              (get-value b))
                           me))
              ((and (has-value? a)
                    (has-value? s))
               (set-value! b
                           (- (get-value s)
                              (get-value a))
                           me))
              ((and (has-value? b)
                    (has-value? s))
               (set-value! a
                           (- (get-value s)
                              (get-value b))
                           me))))
      (function process-forget-value ()
        (do (forget-value! s me)
            (forget-value! a me)
            (forget-value! b me)
            (process-new-value)))
      (function me (request)
        (cond ((equal? request 'I-haz-a-value)  (process-new-value))
              ((equal? request 'I-haz-no-value) (process-forget-value))
              ('else                            (error "Invalid request!"))))
      (connect a me)
      (connect b me)
      (connect s me)
      me))

(function multiplier (m n p)
  (do (function process-new-value ()
        (cond ((or (and (has-value? m) (equal? (get-value m) 0))
                   (and (has-value? n) (equal? (get-value n) 0)))
               (set-value! p 0 me))
              ((and (has-value? m)
                    (has-value? n))
               (set-value! p
                           (* (get-value m)
                              (get-value n))
                           me))
              ((and (has-value? m)
                    (has-value? p))
               (set-value! n
                           (/ (get-value p)
                              (get-value m))
                           me))
              ((and (has-value? n)
                    (has-value? p))
               (set-value! m
                           (/ (get-value p)
                              (get-value n))
                           me))))
      (function process-forget-value ()
        (do (forget-value! p me)
            (forget-value! m me)
            (forget-value! n me)
            (process-new-value)))
      (function me (request)
        (cond ((equal? request 'I-haz-a-value)  (process-new-value))
              ((equal? request 'I-haz-no-value) (process-forget-value))
              ('else                            (error "Invalid request!"))))
      (connect m me)
      (connect n me)
      (connect p me)
      me))

(function constant (value connector)
  (do (function me (request)
        (error "Can't touch this!"))
      (connect connector me)
      (set-value! connector value me)
      me))

(function make-connectors (n)
  (times n make-connector))

(function make-connector ()
  (scope (var (value informant constraints))
         (function haz-value? ()
           (when informant
             'yup))
         (function set-value! (new-value setter)
           (cond ((not (haz-value?))
                  (do (set! value new-value)
                      (set! informant setter)
                      (for-each-except setter
                                       inform-about-value
                                       constraints)))
                 ((not (equal? value new-value))
                  (error "Contradiction!"))
                 ('else 'ignore)))
         (function forget-value! (retractor)
           (when (equal? retractor informant)
             (set! informant ())
             (for-each-except retractor
                              inform-about-no-value
                              constraints)))
         (function connect (new-constraint)
           (do (unless (member-is? new-constraint constraints)
                 (set! constraints (cons new-constraint constraints)))
               (when (haz-value?)
                 (inform-about-value new-constraint))))))

(function member-is? (el col)
  (when col
    (if (is? el (car col))
        col
        (member-is? el (cdr col)))))

(function for-each-except (exception proc lst)
  (do (function loop (items)
        (when items
          (if (is? (car items) exception)
              (loop (cdr items))
              (do (proc (car items))
                  (loop (cdr items))))))
      (loop lst)))

(function inform-about-value (constraint)
  (constraint 'I-haz-a-value))

(function inform-about-no-value (constraint)
  (constraint 'I-haz-no-value))

(function times (n thunk)
  (unless (equal? n 0)
    (cons (thunk) (times (- n 1) thunk))))

(function has-value? (connector)
  ((connector haz-value?)))

(function get-value (connector)
  (connector value))

(function set-value! (connector new-value informant)
  ((connector set-value!) new-value informant))

(function forget-value! (connector informant)
  ((connector forget-value!) informant))

(function reset-value! (connector value informant)
  (do (forget-value! connector informant)
      (set-value! connector value informant)))

(function connect (connector new-constraint)
  ((connector connect) new-constraint))

(function probe (name connector)
  (do (function print (value)
        (write name ": new value = " value "\n"))
      (function process-new-value ()
        (print (get-value connector)))
      (function process-forget-value ()
        (print "?"))
      (function me (request)
        (cond ((equal? request 'I-haz-a-value)  (process-new-value))
              ((equal? request 'I-haz-no-value) (process-forget-value))
              ('else                            (error "Shit makes no sense!"))))
      (connect connector me)
      me))

# Some tests:

(write "Celsius <-> Fahrenheit:\n")

(var (C F) (make-connectors 2))
(probe "Celsius" C)
(probe "Fahrenheit" F)

(celsius-fahrenheit-converter C F)

(set-value! C 32 'user)