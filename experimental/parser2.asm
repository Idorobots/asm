# Parser take two.

(var car first)
(var caar (lambda (lst) (first (first lst))))
(var cadr second)
(var cadadr (lambda (lst) (second (second lst))))
(var caadr (lambda (lst) (first (second lst))))
(var cdr rest)
(var cddr (lambda (lst) (rest (rest lst))))
(var cons join)
(var length length?)
(var cond if*)

(var terminal? string?)

(var *gensym-counter* 0)
(function gensym (base)
  (do (set! *gensym-counter* (+ 1 *gensym-counter*))
      (str->symbol (append "__" base "_" (stringof *gensym-counter*)))))

###############################################
# TODO: Make (tuple string (return)) implicit.
# TODO: Instead of (Arg <- Rules) use (Rule <- Args) ---> more hygenic.

###############################################

(macro syntax (rule transform)
  (do (var name (car rule))
      (var compiled (compile-rule (cddr rule)))
      `(function $name (string)
         $(if transform
              `((lambda ($name) (when $name $transform))
                ($compiled string))
              `($compiled string)))))

(function compile-rule (rule)
  (cond ((terminal? rule) (make-matcher rule))
        ((tuple? rule) (if (equal? (length rule) 1)
                           (compile-rule (car rule))
                           (compile-complex-rule rule)))
        ('else rule)))

(function make-matcher (term)
  `(lambda (string)
     (do (var stripped (strip string))
         (var match (car (regex-match $(append "^" term)
                                      stripped)))
         (when match
           (tuple (advance stripped (length match))
                  (tuple match))))))

(function strip (string)
  (advance string (length (car (regex-match "^[ \\n\\t\\r]*"
                                            string)))))

(function advance (collection n)
  (if (equal? n 0)
      collection
      (advance (cdr collection) (- n 1))))

(function compile-complex-rule (rule)
  (do (var fst (car rule))
      (cond ((equal? fst '/) (compile-or rule))
            ((equal? fst '*) (compile-zero-or-more rule))
            ((equal? fst '+) (compile-one-or-more rule))
            ((equal? fst '?) (compile-optional rule))
            ((equal? fst '!) (compile-not rule))
            ((equal? fst '&) (compile-and rule))
            ((equal? fst ':) (compile-drop rule))
            ('else (compile-sequence rule)))))

# (...)
(function compile-sequence (rules)
  `(lambda (string)
     $(compile-sequence-each rules 'string ())))

(function compile-sequence-each (rules last-str names)
  (if rules
      (do (var str (gensym "str"))
          (var match (gensym "match"))
          `(when (var ($str $match) ($(compile-rule (car rules)) $last-str))
             $(compile-sequence-each (cdr rules) str (append names (tuple match)))))
      (tuple 'tuple last-str (cons 'append names))))

# (/ ...)
(function compile-or (rules)
  `(lambda (string)
     $(compile-or-each (cdr rules))))

(function compile-or-each (rules)
  (when rules
    (var match (gensym "match"))
    `(if (var $match ($(compile-rule (car rules)) string))
         $match
         $(compile-or-each (cdr rules)))))

# (* ...)
(function compile-zero-or-more (rule)
  `(lambda (str)
     (do (function zero-or-more (string matches)
           (if (var (munched match) ($(compile-rule (cdr rule)) string))
               (zero-or-more munched (append matches match))
               (if matches
                   (tuple string matches)
                   (tuple string)))) # A null is fine too.
         (zero-or-more str ()))))

# (+ ...)
(function compile-one-or-more (rule)
  `(lambda (str)
     (do (function one-or-more (string matches)
           (if (var (munched match) ($(compile-rule (cdr rule)) string))
               (one-or-more munched (append matches match))
               (when matches
                 (tuple string matches))))
         (one-or-more str ()))))

# (? ...)
(function compile-optional (rule)
  (do (var foo (gensym "foo"))
      `(lambda (string)
         (if (var $foo ($(compile-rule (cdr rule)) string))
             $foo
             (tuple string)))))

# (! ...)
(function compile-not (rule)
  `(lambda (string)
     (unless ($(compile-rule (cdr rule)) string)
       (tuple string))))

# (& ...)
(function compile-and (rule)
  `(lambda (string)
     (when ($(compile-rule (cdr rule)) string)
       (tuple string))))

# (: ...)
(function compile-drop (rule)
   (do (var str (gensym "str"))
       (var match (gensym "match"))
       `(lambda (string)
          (when (var ($str $match) ($(compile-rule (cdr rule)) string))
            (tuple $str)))))

#### Simple Lisp:
(syntax (Expression <- (/ Atom List)) ())

(syntax (Atom <- (/ Number Symbol)) ())

(syntax (Number <- "[0-9]+")
  `($(car Number)
    ($(str->num (caadr Number)))))

(syntax (Symbol <- "[a-zA-Z0-9_\\*\\?!&%\\+]+")
  `($(car Symbol)
    ($(str->symbol (caadr Symbol)))))

(syntax (List <- (: "\\(") (* Expression) (: "\\)"))
  `($(car List)
    $(cdr List)))

(function parse (string)
  (caadr (Expression string)))

(function repl ()
  (do @while 1
      (write "\n# ")
      (var input (readln))
      (write "\t " (eval (parse input)))))

#########################################
#### Extended Lisp:

(syntax (Lambda <- List (: "->") Expression)
  `($(car Lambda)
    ((lambda $(caadr Lambda) $(cadadr Lambda)))))

(syntax (Apply <- (/ Lambda Symbol) (: "=>") Expression)
  `($(car Apply)
    ((apply $(caadr Apply) $(cadadr Apply)))))

(syntax (Let <- (: "let") (/ Symbol List) (: ":") Expression)
  `($(car Let)
    ((var $(caadr Let) $(cadadr Let)))))

(syntax (Quote <- (: "'") Expression)
  `($(car Quote)
    ((quote $(caadr Quote)))))

(syntax (Expression <- (/ Quote Let Apply Lambda Atom List)) ())

