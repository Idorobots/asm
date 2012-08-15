# Another SICP inspired code.

# Some helpers:

(macro i () '(import 'samples.sim))

(var cond if*)
(var cons join)
(var car first)
(var cadr second)
(var cdar (lambda (x) (cdr (car x))))
(var cdr rest)
(var length length?)

# Code starts here:

(function half-adder (a b s c)
  (do (var d (make-wire))
      (var e (make-wire))
      (or-gate a b d)
      (and-gate a b c)
      (inverter c e)
      (and-gate d e s)
      'ok))

(function full-adder (a b c-in sum c-out)
  (do (var s (make-wire))
      (var c1 (make-wire))
      (var c2 (make-wire))
      (half-adder b c-in s c1)
      (half-adder a s sum c2)
      (or-gate c1 c2 c-out)
      'ok))

(function inverter (input output)
  (do (function invert-input ()
        (do (var new-value (logical-not (get-signal input)))
            (after-delay *inverter-delay*
                         (lambda ()
                           (set-signal! output new-value)))))
      (add-action! input invert-input)
      'ok))

(function logical-not (s)
  (cond ((equal? s 0) 1)
        ((equal? s 1) 0)
        ('else (error "Invalid signal!"))))

(function and-gate (a b output)
  (do (function prod-input ()
        (do (var new-value (logical-and (get-signal a)
                                        (get-signal b)))
            (after-delay *and-gate-delay*
                         (lambda ()
                           (set-signal! output new-value)))))
      (add-action! a prod-input)
      (add-action! b prod-input)
      'ok))

# TODO Invalid signal checking.
(function logical-and (a b)
  (if (and (equal? a 1)
           (equal? b 1))
      1
      0))

(function or-gate (a b output)
  (do (function sum-input ()
        (do (var new-value (logical-or (get-signal a)
                                        (get-signal b)))
            (after-delay *or-gate-delay*
                         (lambda ()
                           (set-signal! output new-value)))))
      (add-action! a sum-input)
      (add-action! b sum-input)
      'ok))

# TODO Invalid signal checking.
(function logical-or (a b)
  (if (or (equal? a 1)
          (equal? b 1))
      1
      0))

(function ripple-carry-adder (A B S C)
  (do (function make-ripple-carry-adder (a b s last-c)
        (when a
          (var next-c (if (cdr a) (make-wire) C))
          (full-adder (car a) (car b) last-c (car s) next-c)
          (make-ripple-carry-adder (cdr a) (cdr b) (cdr s) next-c)))
      (make-ripple-carry-adder A B S (make-wire))
      'ok))

(function make-wire ()
  (scope (var signal-value 0)
         (var action-procedures '())

         (function set-signal! (new-value)
           (unless (equal? signal-value new-value)
             (set! signal-value new-value)
             (call-each action-procedures)))

         (function add-action! (action)
           (do (set! action-procedures (cons action action-procedures))
               (action)))))

(function call-each (procs)
  (unless (fnord? procs)
    ((car procs))
    (call-each (cdr procs))))

(function get-signal (wire)
  (wire signal-value))

(function set-signal! (wire new-value)
  ((wire set-signal!) new-value))

(function add-action! (wire thunk)
  ((wire add-action!) thunk))

(function after-delay (delay thunk)
  (add-to-agenda! *agenda*
                  (+ delay (current-time *agenda*))
                  thunk))

(function make-agenda ()
  (tuple (make-segment 0 (tuple (lambda ()
                                  (write "Simulation started!\n"))))))

(function first-segment (a)
  (car a))

(function rest-segments (a)
  (cdr a))

(function make-segment (time actions)
  (cons time actions))

(function segment-time (s)
  (car s))

(function segment-actions (s)
  (cdr s))

(function empty-agenda? (a)
  (or (fnord? a)
      (fnord? (segment-actions (first-segment a)))))

(function first-agenda-item (a)
  (unless (empty-agenda? a)
    (car (segment-actions (first-segment a)))))

(function remove-first-agenda-item! (a)
  (unless (empty-agenda? a)
    (var (time actions) (first-segment a))
    (var rest-actions (when (tuple? actions)
                        (cdr actions)))

    # Well, fuck macros, man!
    (var f (make-segment time rest-actions))
    (var r (rest-segments a))

    (set! a (cond (rest-actions (cons f r))
                  ((fnord? r)     (tuple (make-segment time ())))
                  ('else          r))))
    'ok)

(function add-to-agenda! (a time action)
  (do (function add-to-agenda!-iterate (segments)
        (if segments
            (do (var s (first-segment segments))
                (var s-time (segment-time s))
                (cond ((equal? s-time time) (cons (make-segment s-time (append (segment-actions s)
                                                                               (tuple action)))
                                                  (rest-segments segments)))
                      ((> s-time time)      (cons (make-segment time (tuple action))
                                                  segments))
                      ('else                (cons s (add-to-agenda!-iterate (rest-segments segments))))))
            (tuple (make-segment time (tuple action)))))
      (set! a (add-to-agenda!-iterate a))))

(function current-time (a)
  (car (first-segment a)))

(function reset-agenda (a)
  (set! a (make-agenda)))

(function step (a)
  (unless (empty-agenda? a)
    ((first-agenda-item a))
    (remove-first-agenda-item! a)))

(function propagate ()
  (unless (empty-agenda? *agenda*)
    (step *agenda*)
    (propagate)))

(function make-bus (len)
  (times len make-wire))

(function times (n thunk)
  (unless (equal? n 0)
    (cons (thunk)
          (times (- n 1) thunk))))

(function bus-value-str (bus)
  (stringof (bus-value bus)))

(function bus-value (bus)
  (when (tuple? bus)
    (map get-signal bus)))

(function set-bus-value! (bus vals)
  (if (equal? (length bus)
              (length vals))
      (when bus
        (set-signal! (car bus) (car vals))
        (set-bus-value! (cdr bus) (cdr vals)))
      (error "Bus size mismatch.")))

(function probe (name wire)
  (add-action! wire
               (lambda ()
                 (write (current-time *agenda*) " ns - " name
                        ": new value = " (get-signal wire) "\n"))))

(function probe-bus (base-name bus)
  (do (function probe-bus-iterate (n bus-rest)
        (when (bus-rest)
          (probe (append (stringof base-name) (stringof n))
                 (car bus-rest))
          (probe-bus-iterate (+ 1 n) (cdr bus-rest))))
      (probe-bus-iterate 0 bus)))

# Some tests:

(var (*inverter-delay*
      *and-gate-delay*
      *or-gate-delay*)
     '(2
       3
       3))

(var *agenda* (make-agenda))

(var (a b s c-in c-out) (make-bus 5))
(probe 'Sum s)
(probe 'Carry c-out)
(full-adder a b c-in s c-out)
(set-signal! a 1)
(set-signal! b 1)
(propagate)

(reset-agenda *agenda*)

(var bus-size 16)
(var A (make-bus bus-size))
(var B (make-bus bus-size))
(var c (make-wire))
(var S (make-bus bus-size))

(ripple-carry-adder A B S c)

(set-bus-value! A '(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(set-bus-value! B '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(write "A: " (bus-value A) "\n")
(write "B: " (bus-value B) "\n")
(write "S: " (bus-value S) "\n")

(propagate)

(write "S: " (bus-value S) "\n")
