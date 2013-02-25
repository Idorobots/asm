# Some helpers:
(macro i () '(import 'experimental.meta3))

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

(defun make-mtuple (args)
  (cond ((equal? (length args) 0) `())
        ((equal? (length args) 1) `(mcons $(car args) ()))
        ('else                    `(mcons $(car args) $(make-mtuple (cdr args))))))

(defmacro mtuple (@tuple args)
  (make-mtuple args))

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

(defun opcode? (name)
  (assoc name *opcode-alist*))

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

(defun cont (env stack code)
  # TODO Store and use stack
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
      (get (mcdr env) (- index 1))))

(defvar *opcode-alist* ())
(defvar *dump-cache* ())

(defmacro defvm (regs @tuple body)
  `(do (defun dump $(cons 'name regs)
              (write "Entering " name "...\n")
              (map (lambda (reg)
                         (write (car reg) ": " (cadr reg) "\n"))
                   (zipWith tuple '$regs $(cons 'tuple regs)))
              (if (equal? (readln) "q")
                  (error "Done")))

     (defmacro go (name)
       (append `((cadr (assoc $name *opcode-alist*)))
               '$regs))

     (defun run $regs
       (apply (cadr (assoc '.eval *opcode-alist*))
              $(cons 'tuple regs)))

     (set! *opcode-alist*
           $(cons 'tuple
                  (map (lambda (op)
                         (tuple 'tuple
                                (cons 'quote (car op))
                                `(lambda $regs
                                   $(append `(do $(append `(dump $(stringof (car op))) regs))
                                            (cdr op)))))
                       body)))))

################################
# VM take 2
# Registers: Expr, Code, Arg, Env, Cont, Meta, Dump

(defvm (Expr Code Args Env Cont Meta Dump)
  # Evaluator:
  (.eval
    (if (nil? Code)
        (go '.continue)
        (do (set! Expr (mcar Code))
            (set! Code (mcdr Code))
            (go '.try-eval))))
  (.try-eval
    (if (opcode? Expr)
        (go Expr)
        (do (set! Args (mcons Expr Args))
            (go '.eval))))
  (.meta-continue
    (set! Cont (mcar Meta))
    (set! Meta (mcdr Meta))
    (go '.continue))
  (.continue
    (if (nil? Cont)
        (go '.meta-continue)
        (do (set! Dump (mcar Cont))
            (set! Cont (mcdr Cont))
            (set! Env  (env Dump))
            #(set! Args (stack Dump))
            (set! Code (code Dump))
            (go '.eval))))
  (.apply
   (set! Dump (car Args))
   (if (operative? Dump)
       (go '.call)
       (do (set! Dump (cont Env Args Code))
           (set! Cont (mcons Dump Cont))
           # TODO
           (go '.halt))))
  (.call
    (when (not (nil? Code))
      (set! Dump (cont Env Args Code))
      (set! Cont (mcons Dump Cont)))
    (set! Expr Env)
    (set! Dump (mcar Args))
    (set! Args (mcdr Args))
    (set! Env  (env Dump))
    (set! Env  (mcons Expr Env)) # FIXME Envsplosion
    #(set! Env  (mcons -1 Env))
    (set! Expr (mcar Args))
    (set! Env  (mcons Expr Env)) # FIXME Argsplosion
    (set! Code (code Dump))
    (go '.eval))

  # State management:
  (.get
    (set! Dump (mcar Args))
    (set! Args (mcdr Args))
    (set! Dump (get Env Dump))
    (set! Args (mcons Dump Args))
    (go '.eval))
  (.defA
    (set! Env (mcons 'dummy Env)) # FIXME A huge kludge.
    (go '.eval))
  (.defB
    (set! Dump (mcar Args))
    (set-car! Env Dump)           # FIXME A huge kludge.
    (go '.eval))

  # Memory management:
  (.car
    (set! Dump (mcdr Args))
    (set! Args (mcar Args))
    (set! Args (mcar Args))
    (set! Args (mcons Args Dump))
    (go '.eval))
  (.cdr
    (set! Dump (mcdr Args))
    (set! Args (mcar Args))
    (set! Args (mcdr Args))
    (set! Args (mcons Args Dump))
    (go '.eval))
  (.cons
    (set! Dump (mcar Args))
    (set! Args (mcdr Args))
    (set! Expr (mcar Args))
    (set! Dump (mcons Expr Dump))
    (set! Args (mcdr Args))
    (set! Args (mcons Dump Args))
    (go '.eval))

  # Combinators:
  (.vau
    (set! Dump (mcar Args))
    (set! Args (mcdr Args))
    (set! Dump (vau Env Dump))
    (set! Args (mcons Dump Args))
    (go '.eval))
  (.wrap
    (set! Dump (mcar Args))
    (set! Dump (wrap Dump))
    (set! Args (mcdr Args))
    (set! Args (mcons Dump Args))
    (go '.eval))
  (.unwrap
    (set! Dump (mcar Args))
    (set! Dump (unwrap Dump))
    (set! Args (mcdr Args))
    (set! Args (mcons Dump Args))
    (go '.eval))

  # Arithmetic:
  # Threading:
  # Basic control structures:
  (.halt
    (mcar Args)))

# Stuff:

(defvar halt (mtuple '.halt))
(defvar halt-cont (mtuple (cont () () halt)))
(defvar finalize (mtuple halt-cont))
(defvar silly (mtuple 1 2 3 4 '.cons '.cons '.cons '.cdr '.car))
(defvar still-silly (mtuple (mtuple 1 2 3 4) '.cdr '.car))
(defvar env-1 (mtuple 42))
(defvar vau-1 (mtuple 23 (mtuple 0 '.get 1 '.get '.cons) '.vau '.call))
(defvar tail (mtuple 23 '.defA (mtuple 0 '.get 2 '.get '.call) '.vau '.defB '.call))
(defvar no-tail (mtuple (mcons 23 42) '.defA (mtuple 0 '.get 2 '.get '.call '.car) '.vau '.defB '.call))
