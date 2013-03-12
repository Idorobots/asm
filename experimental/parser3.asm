# Parser take three.

(import 'imports.lispy)

# For convinient debugging:
(macro i () '(import 'experimental.parser3))

#######################################################3
# Some utils:

(var car first)
(var caar (lambda (lst) (first (first lst))))
(var caadr (lambda (lst) (first (second lst))))
(var cadr second)
(var caddr third)
(var cadadr (lambda (lst) (second (second lst))))
(var cdr rest)
(var cddr (lambda (lst) (rest (rest lst))))
(var cons join)
(var length length?)
(var cond if*)
(var defvar var)
(var defun function)
(var null? fnord?)

###############################################
# Pattern Matcher generator:

(defun pack (match rest)
  (tuple rest match))

(defun pack-none (rest)
  (tuple rest))

(defun get-rest (packed)
  (car packed))

(defun get-match (packed)
  (cadr packed))

(defun has-match? (packed)
  (equal? (length packed) 2))

(defun has-rest? (packed)
  (get-rest packed))

(defun matches? (pattern value)
  (not (get-rest (match pattern value))))

(defun match (pattern value)
  ((matcher pattern) value))

(defun matcher (pattern)
  (cond ((string? pattern)   (match-string pattern))
        ((variable? pattern) (match-variable pattern))
        ((symbol? pattern)   (match-symbol pattern))
        ((number? pattern)   (match-number pattern))
        ((tuple? pattern)    (match-tuple pattern))
        ((vector? pattern)   (match-vector pattern))
        ((function? pattern) pattern)
        ('else               ())))

# String

(defun match-string (pattern)
  (lambda (value)
    (when (and (string? value)
               (prefix? pattern value))
      (pack pattern (drop (length pattern) value)))))

(defun prefix? (a b)
  (cond ((null? a)        't)
        ((null? b)        ())
        ((equal? (car a)
                 (car b)) (prefix? (cdr a) (cdr b)))
        ('else            ())))

(defun drop (n from)
  (if (<= n 0)
      from
      (drop (- n 1) (cdr from))))

# Special Symbol

(defun variable? (s)
  (and (symbol? s)
       (or (equal? (stringof s) "_")
           (equal? (car (stringof s)) "?"))))

(defun match-variable (pattern)
  (lambda (value)
    (pack value ())))

# Symbol

(defun match-symbol (pattern)
  (lambda (value)
    (cond ((symbol? value)
           (when (equal? pattern value)
             (pack value ())))
          ((tuple? value)
           (when (equal? pattern (car value))
             (pack (car value) (cdr value)))))))

# Number

(defun match-number (pattern)
  (lambda (value)
    (cond ((number? value)
           (when (equal? pattern value)
             (pack value ())))
          ((tuple? value)
           (when (equal? pattern (car value))
             (pack (car value) (cdr value)))))))

# Tuple

(defun match-tuple (pattern)
  (lambda (value)
    (when (tuple? value)
      (defun loop (pats value acc)
        (if (null? pats)
            (when (null? value)
              acc)
            (let* ((packed  (match (car pats) (car value)))
                   (rest    (get-rest packed))
                   (match   (get-match packed)))
              (when (and packed (null? rest))
                (loop (cdr pats) (cdr value) (match-cat acc packed))))))
      (loop pattern value ()))))

(defun match-cat (m1 m2)
  (cond ((null? m1)
         m2)
        ((null? m2)
         m1)
        ('else
         (let* ((match1 (get-match m1))
                (match2 (get-match m2))
                (rest   (get-rest m2)))
           (pack (cond ((and (has-match? m1)
                             (has-match? m2))
                        ((if (tuple? match1)
                             append
                             cons)
                         match1
                         (tuple match2)))
                       ((has-match? m1)
                        match1)
                       ((has-match? m2)
                        match2))
                 rest)))))

# Vector

(defun match-vector (pattern)
  (lambda (value)
    (when (equal? pattern value)
      (pack value ()))))

# (... ...)

(defun match-... pats
  (lambda (value)
    (do (defun loop (pats value acc)
          (if (null? pats)
              acc
              (let* ((packed  (match (car pats) value))
                     (rest    (get-rest packed))
                     (match   (get-match packed)))
                (when packed
                  (loop (cdr pats) rest (match-cat acc packed))))))
        (loop pats value ()))))

# (/ ...)

(defun match-/ pats
  (lambda (value)
    (do (defun loop (pats)
          (unless (null? pats)
            (let* ((packed (match (car pats) value))
                   (rest   (get-rest packed))
                   (match  (get-match packed)))
              (or packed
                  (loop (cdr pats))))))
        (loop pats))))

# (! ...)

(defun match-! pats
  (let* ((m (apply match-/ pats)))
    (lambda (value)
      (unless (m value)
        (pack-none value)))))

# (& ...)

(defun match-& pats
  (let* ((m (apply match-/ pats)))
    (lambda (value)
      (when (m value)
        (pack-none value)))))

# (? ...)

(defun match-? pats
  (let* ((m (apply match-/ pats)))
    (lambda (value)
      (or (m value)
          (pack-none value)))))

# (* ...)

(defun match-* pats
  (let* ((m (apply match-/ pats)))
    (lambda (value)
      (do (defun loop (value acc)
            (let* ((packed (m value))
                   (rest   (get-rest packed))
                   (match  (get-match packed)))
              (if packed
                  (loop rest (match-cat acc packed))
                  acc)))
          (loop value (pack-none value))))))

# (+ ...)

(defun match-+ pats
  (let* ((m (apply match-* pats)))
    (lambda (value)
      (let* ((packed (m value))
             (rest   (get-rest packed))
             (match  (get-match packed)))
        (when (has-match? packed)
          packed)))))

# (: ...)

(defun match-: pats
  (let* ((m (apply match-/ pats)))
    (lambda (value)
      (let* ((packed (m value))
             (rest   (get-rest packed)))
        (when packed
          (pack-none rest))))))

# (~ ...)

(defun match-~ pats
  (let* ((m (apply match-/ pats)))
    (lambda (value)
      (let* ((packed (m value))
             (rest   (get-rest packed))
             (match  (get-match packed)))
        (when packed
          (pack (apply concat match) rest))))))

(defun concat stuff
  (reduce (lambda (a b)
            (if (and (string? a)
                     (string? b))
                (append a b)
                (error "Can't cat non-strings!")))
          stuff
          ""))

################################
# Examples:

(match "pattern" "value")
# ()

# FIXME ?
(match (tuple "pa" (match-+ "tt") "ern") "pattttttern")
# ()

(match (tuple "pa" (match-+ "tt") "ern") '("pa" "tttttt" "ern"))
# (() ("pa" ("tt" "tt" "tt") "ern"))

(match (match-... "pa" (match-+ "tt") "ern") "pattttttern")
# (() ("pa" ("tt" "tt" "tt") "ern"))

# FIXME
(match (match-* (match-/ "a" "b")) "")
# ("")

(match (match-* (match-/ "a" "b")) "babbbab")
# (() ("b" "a" "b" "b" "b" "a" "b"))

(match (match-* "a" "b") "babbbab")
# (() ("b" "a" "b" "b" "b" "a" "b"))

(match (match-~ (match-* "a" "b")) "babbbab")
# (() "babbbab")

(match (tuple 1 '(a b) (match-? 3)) '(1 (a b)))
# (() (1 (a b)))

(match (tuple 1 '(a b) (match-? 3)) '(1 (a b) 3))
# (() (1 (a b) 3))

(match (tuple 1 '(a b) (match-? 3)) '(1 (a b) 4))
# ()

# FIXME
(match (tuple 1 '(a b) (match-? 3)) '(1 (a b) ()))
# (() (1 (a b)))

(match (tuple 'a '_ (tuple 1 (match-+ "x") '?b)) '(a 23 (1 "xxxxx" 42)))
# (() (a 23 (1 ("x" "x" "x" "x" "x") 42)))

(match (tuple 'a '_ (tuple 1 (match-+ "x") '?b)) '(a 5 (1 "xxxxx" 13)))
# (() (a 5 (1 ("x" "x" "x" "x" "x") 13)))

(match (match-... 1 (match-+ 2) 3) '(1 2 3))
# (() (1 2 3))

(match (match-... 1 (match-+ 2) 3) '(1 2 2 2 2 2 2 2 3))
# (() (1 (2 2 2 2 2 2 2) 3))

(match 1 1)
# (() 1)

# FIXME
(match 1 '(1))
# (() 1)

# FIXME
(match '(a b c) '((a) (b) (c)))
# (() (a b c))

##################################
# A little more complex example

(defun make-matcher (chars)
  (lambda (value)
    (when (member? (car value) chars)
        (pack (car value) (cdr value)))))

(defvar S       (matcher (match-: (match-* (make-matcher " \t\n\v")))))
(defvar Letter  (make-matcher "abcdefghijklmnopqrstuvwxyz"))
(defvar Digit   (make-matcher "0123456789"))
(defvar Special (make-matcher "_-!?*+~"))
(defvar Integer (matcher (match-~ (match-+ Digit))))
(defvar Symbol  (matcher (match-~ (match-... (match-/ Letter Special)
                                             (match-~ (match-* Letter Digit Special))))))
(defvar List    (lambda (value) (match (match-... (match-: "(")
                                                  S
                                                  (match-* (match-... Expr S))
                                                  S
                                                  (match-: ")"))
                                       value)))
(defvar Expr    (matcher (match-/ List Symbol Integer)))

(match Expr "(define (fact n) (if (equal? n 0) 1 (* n (fact (- n 1)))))")
# (() ("define" ("fact" "n") ("if" ("equal?" "n" "0") "1" ("*" "n" ("fact" ("-" "n" "1"))))))
