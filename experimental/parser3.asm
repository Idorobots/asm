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

(defun get-rest (packed)
  (car packed))

(defun get-match (packed)
  (cadr packed))

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
    (when (equal? pattern value)
      (pack value ()))))

# Number

(defun match-number (pattern)
  (lambda (value)
    (when (equal? pattern value)
      (pack value ()))))

# Tuple

(defun match-tuple (pattern)
  (lambda (value)
    (when (tuple? value)
      (defun loop (pats value acc)
        (if (null? pats)
            (when (null? value)
              (pack acc ()))
            # TODO Figure out how to precompile this match.
            (let* ((packed  (match (car pats) (car value)))
                   (rest    (get-rest packed))
                   (match   (get-match packed)))
              (when (and packed (null? rest))
                (loop (cdr pats) (cdr value) (append acc (tuple match)))))))
      (loop pattern value ()))))

# Vector

(defun match-vector (pattern)
  (lambda (value)
    (when (equal? pattern value)
      (pack value ()))))

# (... ...)

(defun match-... pats
  (lambda (value)
    (do (defun loop (pats value acc)
          (if (car pats)
              # TODO Figure out how to precompile this match.
              (let* ((packed  (match (car pats) value))
                     (rest    (get-rest packed))
                     (match   (get-match packed)))
                (when (and packed (null? rest))
                      (loop (cdr pats) rest (append acc (tuple match)))))
              (pack acc value)))
        (loop pats value ()))))

# (/ ...)

(defun match-/ pats
  (lambda (value)
    (do (defun loop (pats value)
          (when (car pats)
              # TODO Figure out how to precompile this.
              (let* ((packed (match (car pats) value))
                     (rest   (get-rest packed))
                     (match  (get-match packed)))
                (or packed
                    (loop (cdr pats) value)))))
        (loop pats value))))

# (! ...)

(defun match-! pats
  (let* ((m (apply match-/ pats)))
    (lambda (value)
      (unless (m value)
        (pack () value)))))

# (& ...)

(defun match-& pats
  (let* ((m (apply match-/ pats)))
    (lambda (value)
      (when (m value)
          (pack () value)))))

# (? ...)

(defun match-? pats
  (let* ((m (apply match-/ pats)))
    (lambda (value)
      (or (m value)
          (pack () value)))))

# (* ...)

(defun match-* pats
  (let* ((m (apply match-/ pats)))
    (lambda (value)
      (do (defun loop (value acc)
            (let* ((packed (m value))
                   (rest   (get-rest packed))
                   (match  (get-match packed)))
              (if packed
                  (loop rest (append acc (tuple match)))
                  (pack acc value))))
          (loop value ())))))

# (+ ...)

(defun match-+ pats
  (let* ((m (apply match-* pats)))
    (lambda (value)
      (let* ((packed (m value))
             (rest   (get-rest packed))
             (match  (get-match packed)))
        (when (> (length match) 0)
          packed)))))

# (: ...)

(defun match-: pats
  (let* ((m (apply match-/ pats)))
    (lambda (value)
      (let* ((packed (m value))
             (rest   (get-rest packed)))
        (when packed
          (pack () rest))))))

# (~ ...)

(defun match-~ pats
  (let* ((m (apply match-... pats)))
    (lambda (value)
      (let* ((packed (m value))
             (rest   (get-rest packed))
             (match  (get-match packed)))
        (when packed
          (pack (tuple (apply concat match)) rest))))))

(defun concat strings
  (reduce (lambda (a b)
            (if (and (string? a)
                     (string? b))
                (append a b)
                (error "Tried appending strings!")))
          strings
          ""))