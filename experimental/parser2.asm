# Parser take two.

# For convinient debugging:

(macro i () '(import 'experimental.parser2))

# Some bindings:

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

(var terminal? string?)

(var *gensym-counter* 0)
(function gensym (base)
  (do (set! *gensym-counter* (+ 1 *gensym-counter*))
      (str->symbol (append "__" base "_" (stringof *gensym-counter*)))))

(function identity (x) x)

###############################################
# TODO: Make (tuple string (return)) implicit.
# TODO: Instead of (Arg <- Rules) use (Rule <- Args) ---> more hygenic.
# FIXME: Strip strings only once!
# FIXME: Pass stripping? instead of rule type.

###############################################
## PEG arrow kinds:

(var arrow '<-)
(var strip-arrow '<)
(var append-arrow '<~) # NOTE Currently not implemented.

## Syntax macro:

(macro syntax (rule transform)
  (do (var name (car rule))
      (var type (cadr rule))

      (unless (member? type (tuple arrow strip-arrow append-arrow))
        (error (append "Unrecognized rule type: " (stringof type))))

      (var compiled (compile-rule (cddr rule) type))
      `(function $name (string)
         $(if transform
              `((lambda ($name) (when $name $transform))
                ($compiled string))
              `($compiled string)))))

(function compile-rule (rule type)
  (cond ((terminal? rule) (make-matcher rule (equal? type strip-arrow)))
        ((tuple? rule) (if (equal? (length rule) 1)
                           (compile-rule (car rule) type)
                           (compile-complex-rule rule type)))
        ('else rule)))

(function make-matcher (term stripping?)
  `(lambda (string)
     (do (var str $(if stripping?
                       '(strip string)
                       'string))
         (var match (car (regex-match $(append "^" term)
                                      str)))
         (when match
           (tuple (advance str (length match))
                  (tuple match))))))

(function strip (string)
  (if (var (stripped skip) (Space string))
      stripped
      string))

(function advance (collection n)
  (if (equal? n 0)
      collection
      (advance (cdr collection) (- n 1))))

(function compile-complex-rule (rule type)
  (do (var fst (car rule))
      (cond ((equal? fst '/) (compile-or rule type))
            ((equal? fst '*) (compile-zero-or-more rule type))
            ((equal? fst '+) (compile-one-or-more rule type))
            ((equal? fst '?) (compile-optional rule type))
            ((equal? fst '!) (compile-not rule type))
            ((equal? fst '&) (compile-and rule type))
            ((equal? fst ':) (compile-drop rule type))
            ('else (compile-sequence rule type)))))

# (...)
(function compile-sequence (rules type)
  `(lambda (string)
     $(compile-sequence-each rules type 'string ())))

(function compile-sequence-each (rules type last-str names)
  (if rules
      (do (var stripping? (equal? type strip-arrow))
          (var str (gensym "str"))
          (var match (gensym "match"))
          `(when (var ($str $match) ($(compile-rule (car rules) type) $(if stripping?
                                                                           (tuple 'strip last-str)
                                                                           last-str)))
             $(compile-sequence-each (cdr rules) type str (append names (tuple match)))))
      (tuple 'tuple last-str (cons 'append names))))

# (/ ...)
(function compile-or (rules type)
  (do (var stripping? (equal? type strip-arrow))
      `(lambda (string)
         (do (var str $(if stripping?
                           '(strip string)
                           'string))
             $(compile-or-each (cdr rules) type)))))

(function compile-or-each (rules type)
  (when rules
    (var match (gensym "match"))
    `(if (var $match ($(compile-rule (car rules) type) str))
         $match
         $(compile-or-each (cdr rules) type))))

# (* ...)
(function compile-zero-or-more (rule type)
  (do (var stripping? (equal? type strip-arrow))
      `(lambda (str)
         (do (function zero-or-more (string matches)
               (if (var (munched match) ($(compile-rule (cdr rule) type) $(if stripping?
                                                                              '(strip string)
                                                                              'string)))
                   (zero-or-more munched (append matches match))
                   (if matches
                       (tuple string matches)
                       (tuple string)))) # A null is fine too.
             (zero-or-more str ())))))

# (+ ...)
(function compile-one-or-more (rule type)
  (do (var stripping? (equal? type strip-arrow))
      `(lambda (str)
         (do (function one-or-more (string matches)
               (if (var (munched match) ($(compile-rule (cdr rule) type) $(if stripping?
                                                                              '(strip string)
                                                                              'string)))
                   (one-or-more munched (append matches match))
                   (when matches
                     (tuple string matches))))
             (one-or-more str ())))))

# (? ...)
(function compile-optional (rule type)
  (do (var foo (gensym "foo"))
      `(lambda (string)
         (if (var $foo ($(compile-rule (cdr rule) type) string))
             $foo
             (tuple string)))))

# (! ...)
(function compile-not (rule type)
  `(lambda (string)
     (unless ($(compile-rule (cdr rule) type) string)
       (tuple string))))

# (& ...)
(function compile-and (rule type)
  `(lambda (string)
     (when ($(compile-rule (cdr rule) type) string)
       (tuple string))))

# (: ...)
(function compile-drop (rule type)
   (do (var str (gensym "str"))
       (var match (gensym "match"))
       `(lambda (string)
          (when (var ($str $match) ($(compile-rule (cdr rule) type) string))
            (tuple $str)))))

#### Simple Lisp:
(syntax (Space <- (/ Comment "[\\s]+")) ())

(syntax (Comment <- (: "#" "[^\\n]*\\n")) ())

(syntax (Expression < (/ String Atom List)) ())

(syntax (String <- (:"\"") "[^\"]*" (:"\"")) ())

(syntax (Atom <- (/ Number Symbol)) ())

(syntax (Number <- "[+\\-]?[0-9]+(\\.[0-9]*)?")
  `($(car Number)
    ($(str->num (caadr Number)))))

(syntax (Symbol <- (! Number) "[^\\(\\)\\[\\]\\{\\}'`\\$#\\s]+")
  `($(car Symbol)
    ($(str->symbol (caadr Symbol)))))

(syntax (List < (: "\\(") (* Expression) (: "\\)"))
  `($(car List)
    $(cdr List)))

(function parse (string)
  (do (var (rest parse) (Expression string))
      (if* (rest (error (append "Parsing error: Unable to parse `" string "', near: `" rest "'.")))
           ((not parse) (error (append "Parsing error: Shit doesn't parse: `" string "'.")))
           ('else (car parse)))))

##############################################
## The REPL:

(var abort? "abort")
(var current-scope (vau (env) env))
(var *global-scope* (current-scope))

(function read-eval-print-loop (counter env)
  (do @until (equal? counter -1)
      (if (equal? counter 0)
          (write "\n# ")
          (write "\n" counter "# "))
      (if (equal? (var input (readln)) abort?)
          (set! counter -1) # Abort! Abort!
          (catch (write "\t " (eval (parse input) env))
                 (lambda (e)
                   (do (write "Cought an exception in " (if (equal? counter 0)
                                                            "main"
                                                            "the")
                              " loop:\n" e "\n")
                       (write "\nYou can abort the execution by typing `" abort? "'.\n")
                       (read-eval-print-loop (+ counter 1) (scope "New scope."))))))))

(function repl ()
  (do (write "# Experimental REPL. You can exit at any time by typing `" abort? "'.")
      (read-eval-print-loop 0 *global-scope*)
      (write "# Have a nice day!")))

#########################################
#### Some macros:

(syntax (Lambda < List (: "->") Expression)
  `($(car Lambda)
    ((lambda $(caadr Lambda) $(cadadr Lambda)))))

(syntax (Apply < (/ Lambda Symbol) (: "=>") Expression)
  `($(car Apply)
    ((apply $(caadr Apply) $(cadadr Apply)))))

(syntax (Let < (: "let") (/ Symbol List) (: ":") Expression)
  `($(car Let)
    ((var $(caadr Let) $(cadadr Let)))))

(syntax (Quote < (: "'") Expression)
  `($(car Quote)
    ((quote $(caadr Quote)))))

(syntax (Ternary < Expression (: "\\?") Expression (: ":") Expression)
  `($(car Ternary)
    ($(ternary-to-if (cadr Ternary)))))

(function ternary-to-if (t)
  `(if $(car t)
       $(cadr t)
       $(caddr t)))

# JSON anybody?
(syntax (JValue < (/ String Number JObject JArray JTrue JFalse JNull)) ())

(syntax (JObject < (: "\\{") (? JPair (* (:",") JPair)) (:"\\}"))
  `($(car JObject)
    ($(cons 'scope (cadr JObject)))))

(syntax (JPair < String (: ":") JValue)
  `($(car JPair)
    ((var $(str->symbol (caadr JPair)) $(cadadr JPair)))))

(syntax (JArray < (:"\\[") (? JValue (* (:",") JValue)) (:"\\]"))
  `($(car JArray)
    ($(vectorof (cadr JArray)))))

(syntax (JTrue <- "true") ())
(syntax (JFalse <- "false") ())
(syntax (JNull <- "null") ())

## Add it to the Lispy grammar:
(syntax (Expression < (/ Quote JObject String List Atom)) ())

# Some tests:
(JObject "{
  \"Number\": 42,
  \"Decimal\": 123.456,
  \"String\": \"abc\",
  \"Empty\" : {},
  \"Array\" : [0,1,2],
  \"Array2\": [0, [0,1,2], \"abc\"],
  \"Obj\"   : { \"Member\":0, \"Member\":[0,1,2] },
  \"True\"  : true,
  \"False\" : false,
  \"Null\"  : null
}")

# Semantic comments:
(syntax (Comment <- (/ InfoComment MetaComment ExprComment LineComment)) ())

(syntax (InfoComment <- (: "#\\?") String)
  (do (warn (caadr InfoComment))
      (tuple (car InfoComment))))

(syntax (LineComment <- (: "#" "[^\\n]*\\n")) ())

(syntax (ExprComment <- (:"#\\+" Expression)) ())

(syntax (MetaComment <- (: "#!") Expression)
  (do (eval (caadr MetaComment))
      (tuple (car MetaComment))))
