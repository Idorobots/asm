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
  (mcons 'cont (mcons stack (mcons env code))))

(defun stack (reg)
  (if (continuation? reg)
      (mcar (mcdr reg))
      (error (append "Cant access env part of " (stringof reg)))))

(defun env (reg)
  (cond ((operative? reg)    (mcar (mcdr reg)))
        ((continuation? reg) (mcar (mcdr (mcdr reg))))
        ('else               (error (append "Cant access env part of " (stringof reg))))))

(defun set-env! (reg env)
  (cond ((operative? reg)    (set-car! (mcdr reg) env))
        ((applicative? reg)  (error "Not implemented!"))
        ((continuation? reg) (set-car! (mcdr (mcdr reg)) env))
        ('else               (error (append "Cant set env part of " (stringof reg))))))

(defun code (reg)
  (cond ((operative? reg)    (mcdr (mcdr reg)))
        ((continuation? reg) (mcdr (mcdr (mcdr reg))))
        ('else               (error (append "Cant access code part of " (stringof reg))))))

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
# VM take 3
############
# Registers:
# 
# Code  - code to execute,
# Acc   - the accumulator,
# Stack - the stack,
# Env   - static environment,
# DEnv  - dynamic environment,
# Cont  - current continuation stack,
# Meta  - meta-continuation stack,
# Temp  - a helper register not to introduce temporary variables with defvar

(defvm (Code Acc Stack Env DEnv Cont Meta Temp)
  # Evaluator:
  (.eval
    (if (nil? Code)
        (go '.continue)
        (do (set! Temp (mcar Code))
            (set! Code (mcdr Code))
            (if (opcode? Temp)
                (go Temp)
                (do (set! Stack (mcons Acc Stack))
                    (set! Acc    Temp)
                    (go '.eval))))))
  (.meta-continue
    (set! Cont (mcar Meta))
    (set! Meta (mcdr Meta))
    (go '.continue))
  (.continue
    (if (nil? Cont)
        (go '.meta-continue)
        (do (set! Temp (mcar Cont))
            (set! Cont (mcdr Cont))
            (set! Env  (env Temp))
            (set! Stack (stack Temp))
            (set! Code (code Temp))
            (go '.eval))))
  (.apply
   (if (operative? Acc)
       (go '.call)
       (do (set! Temp (cont Env Stack Code))
           (set! Cont (mcons Temp Cont))

           # TODO .eval the arg, and .call unwrapped combinator

           (go '.halt))))

  (.call
    (when (not (nil? Code))
      (set! Temp (cont Env Stack Code))
      (set! Cont (mcons Temp Cont)))
    (set! DEnv Env)         # FIXME? Doesn't restore it after inner call.
    (set! Env   (env Acc))
    (set! Temp  Code)
    (set! Code  (code Acc))
    (set! Acc   (mcar Stack))
    (set! Stack (mcdr Stack))
    (set! Env   (mcons Acc Env))
    (when (nil? Temp)
      (set! Stack ()))
    (go '.eval))

  # State management:
  (.dyn-env
    (set! Stack (mcons Acc Stack))
    (set! Acc DEnv)
    (go '.eval))
  (.get
    (set! Acc  (get Env Acc))
    (go '.eval))

  # TODO Generalize these to support multiple mutually recursive expressions.
  (.def
    (set! Env (mcons Acc Env))
    (go '.eval))
  (.def-rec
    (set! Env (mcons Acc Env))
    (set-env! Acc Env)         # FIXME A little less of a kludge, but still...
    (go '.eval))

  # Memory management:
  (.car
    (set! Acc (mcar Acc))
    (go '.eval))
  (.cdr
    (set! Acc (mcdr Acc))
    (go '.eval))
  (.cons
    (set! Temp (mcar Stack))
    (set! Stack (mcdr Stack))
    (set! Acc (mcons Acc Temp))
    (go '.eval))

  # Combinators:
  (.vau
    (set! Acc (vau Env Acc))
    (go '.eval))
  (.wrap
    (set! Acc (wrap Acc))
    (go '.eval))
  (.unwrap
    (set! Acc (unwrap Acc))
    (go '.eval))

  # Arithmetic:
  # Threading:
  # Basic control structures:
  (.halt
    Acc))

# Stuff:

(defvar halt (mtuple '.halt))
(defvar halt-cont (mtuple (cont () () halt)))
(defvar finalize (mtuple halt-cont))
(defvar silly (mtuple 1 2 3 4 '.cons '.cons '.cons '.cdr '.car))
(defvar still-silly (mtuple (mtuple 1 2 3 4) '.cdr '.car))
(defvar env-1 (mtuple 42))
(defvar vau-1 (mtuple 23 (mtuple 0 '.get 1 '.get '.cons) '.vau '.call))
(defvar tail-1 (mtuple 23 (mtuple 0 '.get 1 '.get '.call) '.vau '.def-rec '.call))
(defvar no-tail-1 (mtuple (mcons 23 42) (mtuple 0 '.get 1 '.get '.call '.car) '.vau '.def-rec '.call))
(defvar vau-2 (vau () (mtuple 0 '.get '.car)))
(defvar tail-2 (mtuple 23 (mtuple (mcons 23 42) 1 '.get '.call) '.vau '.call))
(defvar no-tail-2 (mtuple 23 (mtuple (mcons 23 42) 1 '.get '.call 0 '.get '.cons) '.vau '.call))
(defvar env-2 (mtuple vau-2))
