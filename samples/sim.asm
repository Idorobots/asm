# Another SICP inspired code.

# Some helpers:

(macro i () '(import 'samples.sim))

(var cond if*)
(var cons join)
(var car first)
(var cadr second)
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
  (do (function make-ripple-carry-adder (a b s c)
        (unless (fnord? a)
           (var next-c (if (cdr a)
                           (make-wire)
                           C))
           (full-adder (car a) (car b) c (car s) next-c)
           (make-ripple-carry-adder (cdr a) (cdr b) next-c (cdr s))))
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

# TODO Needs a correct agenda.
(function make-agenda ()
  (tuple (tuple 0)))

#(function empty-agenda? (a)
  )

#(function first-agenda-item (a)
  )

(function remove-first-agenda-item! (a)
  )

#(function add-to-agenda! (a time action)
  )

(function current-time (a)
  (tuple (car a)))

(function reset-agenda (a)
  (set! a (make-agenda)))

(function propagate ()
  (unless (empty-agenda? *agenda*)
    ((first-agenda-item *agenda*))
    (remove-first-agenda-item! *agenda*)
    (propagate)))

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
      (error "Bus length mismatch.")))

(function probe (name wire)
  (add-action! wire
               (lambda ()
                 (write (current-time *agenda*) " ns - " name
                        ": new value = " (get-signal wire) "\n"))))

# Some tests:

(var (*inverter-delay*
      *and-gate-delay*
      *or-gate-delay*)
     '(2
       3
       3))

(var *agenda* (make-agenda))

(function times (n thunk)
  (unless (equal? n 0)
    (cons (thunk)
          (times (- n 1) thunk))))

(var (a b s c) (times 4 make-wire))
(probe 'Sum s)
(probe 'Carry c)
(half-adder a b s c)
(set-signal! a 1)
(set-signal! b 1)
(propagate)

(reset-agenda *agenda*)

(var A (times 16 make-wire))
(var B (times 16 make-wire))
(var c (make-wire))
(var S (times 16 make-wire))

(ripple-carry-adder A B S c)

(set-bus-value! A '(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(set-bus-value! B '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(write "A: " (bus-value A) "\n")
(write "B: " (bus-value B) "\n")
(write "S: " (bus-value S) "\n")

#(propagate)

#(write "S: " (bus-value S) "\n")