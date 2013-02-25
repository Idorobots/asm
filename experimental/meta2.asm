# Metacircular evaluator for ASM VM (and ASM in general).
# Just a concept.

# Some helpers:
(macro i () '(import 'experimental.meta))

# Some bindings:

(var car first)
(var caar (lambda (lst) (first (first lst))))
(var caadr (lambda (lst) (first (second lst))))
(var cadar (lambda (lst) (second (first lst))))
(var cadr second)
(var caddr third)
(var cadadr (lambda (lst) (second (second lst))))
(var cdr rest)
(var cddr (lambda (lst) (rest (rest lst))))
(var cons join)
(var length length?)
(var cond if*)
(var defvar var)
(var null? fnord?)
(var defmacro macro)

(defmacro defun (name args @tuple body)
  `(var $name (lambda $args $(cons 'do body))))

# Helper functions:

(defun mcons (a b)
  (tuple a b)) # There are no improper lists ATM in ASM, so we have to emulate...

(defvar mcar car)
(defvar mcdr cadr)

(defvar nil? fnord?)

(defmacro op (reg @tuple args)
  (append (cons 'tuple reg) args))

(defun op? (reg)
  (and (tuple? reg)
       (opcode? (car reg))))

(defvar opcode? function?)

(defun opargs (reg)
  (if (op? reg)
      (cdr reg)
      (error "Not an op.")))

(defun opcode (reg)
  (if (op? reg)
      (car reg)
      (error "Not an op.")))

(defun operative? (reg)
  (and (not (null? reg))
       (equal? (car reg) 'vau)))

(defun applicative? (reg)
  (and (not (null? reg))
       (equal? (car reg) 'lambda)))

(defun continuation? (reg)
  (and (not (null? reg))
       (equal? (car reg) 'cont)))

(defun vau (env code)
  (mcons 'vau (mcons env code)))

(defun wrap (vau)
  (if (operative? vau)
      (cons 'lambda vau)
      (error "Not an operative.")))

(defun unwrap (lam)
  (if (applicative? lam)
      (cdr lam)
      (error "Not an applicative.")))

(defun lam (env code)
  (wrap (vau env code)))

(defun cont (env code)
  (mcons 'cont (mcons env code)))

(defun env (reg)
  (if (or (operative? reg)
          (continuation? reg))
      (mcar (mcdr reg))
      (error (append "Cant access env part of " (stringof reg)))))

(defun code (reg)
  (if (or (operative? reg)
          (continuation? reg))
      (mcdr (mcdr reg))
      (error (append "Cant access code part of " (stringof reg)))))

(defun get (env index)
  (if (equal? index 0)
      (mcar env)
      (get (mcdr env (- index 1)))))

(defmacro defvm (regs @tuple body)
  (append `(do (defun dump $(cons 'name regs)
                 (write "Entering " name "...\n")
                 (map (lambda (reg)
                        (write (car reg) ": " (cadr reg) "\n"))
                       (zipWith tuple '$regs $(cons 'tuple regs)))
                 (if (equal? (readln) "q")
                     (error "Done")))

               (defmacro defop (name @tuple body)
                 (append (tuple 'defun name '$regs
                            (append (tuple 'dump (stringof name)) '$regs))
                         body))
               (defmacro go (name)
                 (cons name '$regs)))

          (map (lambda (op)
                 (append `(defop $(car op))
                         (cdr op)))
               body)))

################################
# VM take 2
# Registers: Expr, Code, Arg, Env, Cont, Meta, Dump

(defvm (Expr Code Arg Env Cont Meta Dump)
  # Evaluator:
  (.eval-in
    (set! Env  (mcdr Expr))
    (set! Expr (mcar Expr))
    (go .eval))
  (.eval
    (if (op? Expr)
        (go .eval-op)
        (do (set! Arg Expr)
            (if (nil? Code)
                (go .continue)
                (do (set! Expr (mcar Code))
                    (set! Code (mcdr Code))
                    (go .eval))))))
  (.eval-op
    (set! Dump (opcode Expr))
    (set! Expr (opargs Expr))
    (go Dump))
  (.meta-cont
    (set! Cont (mcar Meta))
    (set! Meta (mcdr Meta))
    (go .continue))
  (.continue
    (if (nil? Cont)
        (go .meta-cont)
        (do (set! Dump (mcar Cont))
            (set! Cont (mcdr Cont))
            (set! Env  (env  Dump))
            (set! Code (code Dump))
            (set! Expr (mcar Code))
            (set! Code (mcdr Code))
            (go .eval))))
  (.apply
    (set! Dump (mcdr Expr))
    (set! Dump (op .apply-comb Dump))
    (set! Dump (mcons Dump Code))
    (set! Dump (cont Env Dump))
    (set! Cont (mcons Dump Cont))
    (set! Expr (mcar Expr))
    (go .eval))
  (.apply-comb
    (if (operative? Arg)
        (go .rev-call)
        (do (set! Dump (unwrap Arg))
            (set! Dump (op .call Dump))
            (set! Dump (mcons Dump Code))
            (set! Dump (cont Ent Dump))
            (set! Cont (mcons Dump Cont))
            (go .eval))))
  (.rev-call
    (set! Dump Expr)
    (set! Expr Arg)
    (set! Arg Dump)
    (go .call))
  (.call
    (when (not (nil? Code))
      (set! Dump (cont Env Code))
      (set! Cont (mcons Dump Cont)))
    (set! Dump Env)
    (set! Env (env Expr))
    (set! Env (mcons Dump Env))
    (set! Env (mcons Arg Env))
    (set! Code (code Expr))
    (set! Expr (mcar Code))
    (set! Code (mcdr Code))
    (go .eval))

  # State management:
  (.get
   (set! Expr (get Env (mcar Expr))) # FIXME should use improper lists
   (go .eval))

  # Memory management:
  (.car
    (set! Expr (mcar Arg)) # FIXME Should be a regular argument
    (go .eval))

  # Combinators:
  (.vau
    (set! Expr (vau Env (mcar Expr))) # FIXME should use improper lists
    (go .eval))
  (.warap
   (set! Expr (wrap Expr)))

  # Arithmetic:
  # Threading:
  # Basic control structures:
  (.halt
    Arg))

# Stuff:

(defvar halt-cont (mcons (cont () (mcons (op .halt) ())) ()))
(defvar simple-env (mcons 23 (mcons 42 ())))
(defvar foo (op .apply (op .vau (mcons (op .get 0) (mcons (op .car) ()))) 23))