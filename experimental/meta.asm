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

(defun opcode? (reg)
  (and (tuple? reg)
       (function? (car reg))))

(defun args (reg)
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

(defun vau (env code)
  (tuple 'vau env code))

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

(defun get (index env)
  (nth env index))

################################
# The VM & its Opcodes
# Registers: Arg, Expr, Cont, Env, Dump

(defun dump (Op Arg Expr Env Cont Dump)
  (write "Entering " Op "...\n")
  (write "Arg: " Arg "\n")
  (write "Expr: " Expr "\n")
  (write "Env: " Env "\n")
  (write "Cont: " Cont "\n")
  (write "Dump: " Dump "\n")
  (readln))

(defmacro defvm (regs @tuple body)
  (append `(do (defmacro defop (name @tuple body)
                 (append (tuple 'defun name '$regs
                            (append (tuple 'dump (stringof name)) '$regs))
                         body))
               (defmacro go (name)
                 (cons name '$regs)))

          (map (lambda (op)
                 (append `(defop $(car op))
                         (cdr op)))
               body)))

(defvm (Arg Expr Env Cont Dump)

  # Evaluator:
  (.eval
    (if (opcode? Expr)
        (do (set! Dump (opcode Expr))
            (set! Expr (args Expr))
            (go Dump))
        (do (set! Arg Expr)
            (set! Expr (car Cont))
            (set! Env (cadr Cont))
            (set! Cont (cddr Cont))
            (go .eval))))
  (.apply
    (set! Arg (cadr Expr))
    (set! Expr (car Expr))
    (if (operative? Expr)
      (go .call)
      (do (set! Cont (cons Env Cont))
          (set! Cont (cons (cons .call (unwrap Expr)) Cont))
          (set! Expr Arg)
          (go .eval))))
  (.call
    (set! Dump Env)
    (set! Env (cadr Expr))
    (set! Env (cons Dump Env))
    (set! Env (cons Arg Env))
    (set! Expr (caddr Expr))
    (go .eval))

  # State management:
  (.def
    (set! Env (cons (car Expr) Env))
    (go .eval))
  (.get
    (set! Expr (get (car Expr) Env))
    (go .eval))
  (.set!
     (error "Unimplemented."))

  # Memory management:
  (.car
     (set! Expr (car Expr))
     (go .eval))
  (.cdr
     (set! Expr (cdar Expr))
     (go .eval))
  (.cons
     (set! Expr (apply #'cons Expr))
     (go .eval))

  # Combinators:
  (.wrap
    (set! Expr (wrap (car Expr)))
    (go .eval))
  (.vau
    (set! Expr (vau Env (car Expr)))
    (go .eval))

  # Arithmetic:
  # Threading:
  # Basic control structures:
  (.sel
    (set! Dump (car Expr))
    (set! Expr (cdr Expr))
    (if Dump
        (set! Expr (car Expr))
        (set! Expr (cadr Expr)))
    (go .eval))
  (.do
    (when Expr
      (set! Cont (cons Env Cont))
      (set! Cont (cons (cons .do (cdr Expr)) Cont))
      (set! Expr (car Expr)))
    (go .eval))
  (.halt
    Arg))
