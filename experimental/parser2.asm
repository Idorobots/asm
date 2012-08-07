# Parser take two.

# For convinient debugging:
(macro i () '(import 'experimental.parser2))

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

(var *gensym-counter* 0)
(function gensym (base)
  (do (set! *gensym-counter* (+ 1 *gensym-counter*))
      (str->symbol (append "__" base "_" (stringof *gensym-counter*)))))

(function identity (x) x)

###############################################
# TODO: Make (tuple string (return)) implicit.
# TODO: Instead of (Arg <- Rules) use (Rule <- Args) ---> more hygenic.
# TODO: compile-complex-rule should pass (cdr rule) not rule.
# FIXME: Strip strings only once!

###############################################
# PEG parser generator:

(var left-arrow '<-)
(var strip-arrow '<)
(var concat-arrow '<~)

(macro grammar (@tuple rules)
  (cons 'do
        (map (lambda (rule)
               (make-syntax (car rule) (cdr rule)))
             rules)))

(macro syntax (rule @tuple transform)
  (make-syntax rule transform))

(function make-syntax (rule transform)
  (do (var name (car rule))
      (var type (cadr rule))

      (unless (member? type (tuple left-arrow strip-arrow concat-arrow))
        (error (append "Unrecognized rule type: " (stringof type))))

      (var stripping? (equal? type strip-arrow))
      (var catting? (equal? type concat-arrow))

      (var compiled (if catting?
                        (compile-rule (tuple '~ (cddr rule)) stripping?)
                        (compile-rule (cddr rule) stripping?)))

      `(function $name (string)
         $(if transform
              `((lambda ($name) (when $name $(append '(do) transform)))
                ($compiled string))
              `($compiled string)))))

(function compile-rule (rule stripping?)
  (cond ((terminal? rule)         (make-matcher rule stripping?))
        ((nonterminal? rule)      rule)
        ((equal? (length rule) 1) (compile-rule (car rule) stripping?))
        ('else                    (compile-complex-rule rule stripping?))))

(var terminal? string?)
(var nonterminal? symbol?) # FIXME Accepts strings.

(function make-matcher (term stripping?)
  (do (var str (if stripping? 'str 'string))
      (var match (gensym "match"))
      `(lambda (string)
         (do $(when stripping?
                '(var str (strip string)))
             (var $match (car (regex-match $(append "^" term) $str)))
             (when $match
               (tuple (advance $str (length $match))
                      (tuple $match)))))))

(function strip (string)
  (if (var (stripped skip) (Spacing string))
      stripped
      string))

(function advance (collection n)
  (if (equal? n 0)
      (unless (equal? (length collection) 0)
        collection)
      (advance (cdr collection) (- n 1))))

(function compile-complex-rule (rule stripping?)
  (do (var fst (car rule))
      (cond ((equal? fst '/) (compile-or rule stripping?))
            ((equal? fst '*) (compile-zero-or-more rule stripping?))
            ((equal? fst '+) (compile-one-or-more rule stripping?))
            ((equal? fst ':) (compile-drop rule stripping?))
            ((equal? fst '?) (compile-optional rule stripping?))
            ((equal? fst '!) (compile-not rule stripping?))
            ((equal? fst '&) (compile-and rule stripping?))
            ((equal? fst '~) (compile-concat rule stripping?))
            ('else (compile-sequence rule stripping?)))))

# (...)
(function compile-sequence (rules stripping?)
  `(lambda (string)
     $(compile-sequence-each rules stripping? 'string ())))

(function compile-sequence-each (rules stripping? last-str names)
  (if rules
      (do (var str (gensym "str"))
          (var match (gensym "match"))
          `(when (var ($str $match) ($(compile-rule (car rules) stripping?)
                                     $(if stripping?
                                          (tuple 'strip last-str)
                                          last-str)))
             $(compile-sequence-each (cdr rules) stripping? str (append names (tuple match)))))
      (tuple 'tuple last-str (cons 'append names))))

# (/ ...)
(function compile-or (rules stripping?)
  `(lambda (string)
     (do (var str $(if stripping?
                       '(strip string)
                       'string))
         $(compile-or-each (cdr rules) stripping?))))

(function compile-or-each (rules stripping?)
  (when rules
    (var match (gensym "match"))
    `(if (var $match ($(compile-rule (car rules) stripping?) str))
         $match
         $(compile-or-each (cdr rules) stripping?))))

# (* ...)
(function compile-zero-or-more (rule stripping?)
  `(lambda (str)
     (do (function zero-or-more (string matches)
           (if (var (munched match) ($(compile-rule (cdr rule) stripping?)
                                     $(if stripping?
                                          '(strip string)
                                          'string)))
               (zero-or-more munched (append matches match))
               (if matches
                   (tuple string matches)
                   (tuple string)))) # A null is fine too.
         (zero-or-more str ()))))

# (+ ...)
(function compile-one-or-more (rule stripping?)
  `(lambda (str)
     (do (function one-or-more (string matches)
           (if (var (munched match) ($(compile-rule (cdr rule) stripping?)
                                     $(if stripping?
                                          '(strip string)
                                          'string)))
               (one-or-more munched (append matches match))
               (when matches
                 (tuple string matches))))
         (one-or-more str ()))))

# (? ...)
(function compile-optional (rule stripping?)
  (do (var foo (gensym "foo"))
      `(lambda (string)
         (if (var $foo ($(compile-rule (cdr rule) stripping?) string))
             $foo
             (tuple string)))))

# (! ...)
(function compile-not (rule stripping?)
  `(lambda (string)
     (unless ($(compile-rule (cdr rule) stripping?) string)
       (tuple string))))

# (& ...)
(function compile-and (rule stripping?)
  `(lambda (string)
     (when ($(compile-rule (cdr rule) stripping?) string)
       (tuple string))))

# (: ...)
(function compile-drop (rule stripping?)
   (do (var str (gensym "str"))
       (var match (gensym "match"))
       `(lambda (string)
          (when (var ($str $match) ($(compile-rule (cdr rule) stripping?) string))
            (tuple $str)))))

# (~ ...)
(function compile-concat (rule stripping?)
  (do (var str (gensym "str"))
      (var match (gensym "match"))
      `(lambda (string)
         (when (var ($str $match) ($(compile-rule (cdr rule) stripping?) string))
           (tuple $str
                  (tuple (reduce append $match "")))))))

##############################################
# REPL:

(var abort? "abort")
(var current-scope (vau (env) env))
(var *global-scope* (current-scope))

(function repl ()
  (do (write "# Experimental REPL. You can exit at any time by typing `" abort? "'.")
      (read-eval-print-loop 0 *global-scope*)
      (write "# Have a nice day!")))

(function read-eval-print-loop (counter env)
  (unless (equal? counter -1)
    (write "\n#" (if (equal? counter 0) "" counter) " ")
    (if (equal? (var input (readln)) abort?)
        (set! counter -1) # Abort! Abort!
        (catch (write "  ⇒  " (eval (parse input) env))
               (lambda (e)
                 (do (write "Cought an exception in " (if (equal? counter 0)
                                                          "main"
                                                          "the")
                            " loop:\n" e "\n")
                     (write "\nYou can abort the execution by typing `" abort? "'.\n")
                     (read-eval-print-loop (+ counter 1) (scope "New scope."))))))
    (read-eval-print-loop counter env)))

(function parse (string)
  (do (var (rest parse) (Expression string))
      (cond (rest (error (append "Parsing error: Unable to parse `" string "', near: `" rest "'.")))
            ((not parse) (error (append "Parsing error: Shit doesn't parse: `" string "'.")))
            ('else (car parse)))))

#########################################################
# Predefined parsers:

(grammar ((Alpha       <- "[a-zA-Z_]"))
         ((Digit       <- "[0-9]"))
         ((Alphanum    <- (/ Alpha Digit)))
         ((Identifier  <~ Alpha (* Alphanum)))
         ((Space       <- " "))
         ((Blank       <- (/ Space "\t" "\v")))
         ((LF          <- "\n"))
         ((CR          <- "\r"))
         ((CRLF        <- "\r\n"))
         ((EOL         <- (/ CRLF LF CR)))
         ((EOI         <- "^$"))
         ((Spacing     <- (* (/ Blank EOL))))
         ((Line        <~ (+ (! EOL) ".") (/ EOL EOI)))
         ((Lines       <~ (+ Line)))
         ((Slash       <- "/"))
         ((BackSlash   <- "\\"))
         ((Quote       <- "'"))
         ((BackQuote   <- "`"))
         ((DoubleQuote <- "\"")))

"\"" # FIXME Old parser bug, lol.

##########################################################
# Simple Lisp:

(grammar ((Expression < (/ String List Atom)))
         ((String     <- (:"\"") "[^\"]*" (: "\"")))
         ((List       < (: "\\(") (* Expression) (: "\\)"))
                        `($(car List)
                          $(cdr List)))
         ((Atom       <- (/ Number Symbol)))
         ((Number     <- "[+\\-]?[0-9]+(\\.[0-9]*)?")
                         `($(car Number)
                           ($(str->num (caadr Number)))))
         ((Symbol     <- (! Number) "[^\\(\\)\\[\\]\\{\\}\"'`\\$#\\s]+")
                         `($(car Symbol)
                           ($(str->symbol (caadr Symbol)))))
         ((Spacing    <- (* (/ Comment "[\\s]+"))))
         ((Comment    <- (: "#" "[^\n]*\n"))))

#########################################
# Some macros:

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

#############################################
# JSON anybody?

(grammar ((JValue  < (/ String Number JObject JArray JTrue JFalse JNull)))
         ((JObject < (: "\\{") (? JPair (* (:",") JPair)) (:"\\}"))
                     `($(car JObject)
                       ($(cons 'scope (cadr JObject)))))
         ((JPair   < String (: ":") JValue)
                     `($(car JPair)
                       ((var $(str->symbol (caadr JPair)) $(cadadr JPair)))))
         ((JArray  < (:"\\[") (? JValue (* (:",") JValue)) (:"\\]"))
                     `($(car JArray)
                        ($(vectorof (cadr JArray)))))
         ((JTrue   <- (: "true"))
                      `($(car JTrue)
                         ('t)))
         ((JFalse  <- (: "false")))
         ((JNull   <- (: "null"))))

# Add it to the Lispy grammar:
(syntax (Expression < (/ Quote JObject String List Atom)))

##############################################
# Semantic comments:

(grammar ((Comment     <- (/ InfoComment MetaComment ExprComment LineComment)))
         ((InfoComment <- (: "#\\?") String)
                          (do (warning (caadr InfoComment))
                              (tuple (car InfoComment))))
         ((ExprComment <- (:"#\\+" Expression)))
         ((MetaComment <- (: "#!") Expression)
                          (do (eval (caadr MetaComment))
                              (tuple (car MetaComment))))
         ((LineComment <- (: "#" "[^\\n]*\\n"))))

############################################
# Some tests:

#(write (JObject "{
  \"Number\" : 42,
  \"Decimal\": 123.456,
  \"String\" : \"abc\", #?\"This is a string.\"
  \"Empty\"  : {},
  \"Array\"  : [0,1,2],
  \"Array2\" : [0, [0,1,2], \"abc\"],
  \"Obj\"    : { \"Member\":0, \"Member\":[0,1,2] },
  \"True\"   : true,
  \"False\"  : false,
  \"Null\"   : null
}"))