## The syntax table:

(var syntax-table '["\(" "\)" "\'" "," "\-\>" "macro"])
(var macro-table '[(test 2)])

## Syntax dispatchers:

(function "\(" [ostream istream]
  (do (var list (list))
      (read-delimited-list "\)" list istream)
      (if (empty? list)
          (push! fnord ostream)
          (push! (tupleof (first list)) ostream))))

(function "\)" [o i]
  (error "Missmatched `)'."))

(function "\'" [ostream istream]
  (do (var tmp (list))
      (read-expression tmp istream)
      (push! (qquote (quote (embed (first tmp))))
             ostream)))

(function "," [ostream istream]
  (do (var left (pop! ostream))
      (var tmp (list))
      (read-expression tmp istream)
      (var right (first tmp))
      (if (tuple? left)
          (push! (append! left (tuple right)) ostream)
          (push! (tuple left right) ostream))))

(function "\-\>" [ostream istream]
  (do (var args (pop! ostream))
      (var body (list))
      (read-expression body istream)
      (push! (qquote (lambda (embed (if (collection? args)
                                        (listof args)
                                        (list args)))
                             (embed (first body))))
             ostream)))

(function "macro" [ostream istream]
  (do (var name (pop! istream))
      (var tmp (list))
      (read-expression tmp istream)
      (var args (first tmp))
      (read-expression tmp istream)
      (var body (first tmp))
      (push! 'do ostream)
      (push! (qquote (push! (tuple (quote (embed name)) (embed (length? args)))
                            macro-table))
             ostream)
      (push! (qquote (function (embed name)
                             (embed args)
                             (embed body)))
             ostream)))

## Macros:

(function test [foo bar]
  (qquote (list (embed bar)
               (embed foo))))

## Reader functions:

(function parser-macro-call [token ostream istream]
  (when (callable? (get token))
        (var argnum (second (assoc token macro-table)))
        (var args (list))
        (var i 0)
        (do .until (equal? i argnum)
             (read-expression args istream)
             (set! i (+ i 1)))
       (push! (apply (get token) args) ostream)))

(function parser-try-call [token ostream istream]
  (if (callable? (get token))
      ((get token) ostream istream)
      (push! (get token) ostream)))

(function read-expression [ostream istream]
  (do (var token (pop! istream))
      (if (member? token syntax-table)
          (parser-try-call token ostream istream)
      (#else if (assoc token macro-table)
          (parser-macro-call token ostream istream)
      #else (push! token ostream)))))

(function read-delimited-list [delimiter ostream istream]
  (do (var list (list))
      (do .until (equal? (first istream) delimiter)
          (unless istream (error "Missmatched `('."))
          (read-expression list istream))
      (pop! istream)
      (push! (reverse list) ostream)))

(function parse [input]
  (do (var istream (lex input syntax-table))
      (var ostream (list))
      (do .until (fnord? istream)
          (read-expression ostream istream))
      (reverse ostream)))

(function repl [__depth]
  (do (var __prompt " > ")
      (write __depth)
      (loop (catch (do (var __input (readln __prompt))
                       (var __parsed (eval (first (parse __input))))
                       (write \tab (if __parsed __parsed " ") \newline))
                   (lambda [error]
                     (do (write \tab error \newline)
                         (repl (+ 1 __depth)))))
            (write __depth))))