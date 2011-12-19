## The syntax table:

(var syntax-table '["\\(" "\\)" "'" "," "\\-\\>" "macro" ":" ";"])
(var macro-table '[(test 2 (lambda (foo bar) '(* foo bar)))])

## Syntax dispatchers:

(function "\\(" [ostream istream]
  (do (var list (vector))
      (read-delimited-list "\\)" list istream)
      (if (empty? list)
          (push! fnord ostream)
          (push! (tupleof (first list)) ostream))))

(function ";" [o i]
  (error "Missmatched `;'."))

(function "\\)" [o i]
  (error "Missmatched `)'."))

(function "'" [ostream istream]
  (do (var tmp (vector))
      (read-expression tmp istream)
      (push! (qquote (quote (embed (first tmp))))
             ostream)))

(function "," [ostream istream]
  (do (var left (pop! ostream))
      (var tmp (vector))
      (read-expression tmp istream)
      (var right (first tmp))
      (if (tuple? left)
          (push! (append! left (tuple right)) ostream)
          (push! (tuple left right) ostream))))

(function ":" [ostream istream]
  (do (var right (vector))
      (read-delimited-list ";" right istream)
      (push! (qquote (var (embed (pop! ostream))
                          (embed (if (equal? (length? (first right)) 1)
                                     (first (first right))
                                     (tupleof (first right))))))
             ostream)))

(function "\\-\\>" [ostream istream]
  (do (var args (pop! ostream))
      (var body (vector))
      (read-expression body istream)
      (push! (qquote (lambda (embed (if (collection? args)
                                        (vectorof args)
                                        (vector args)))
                             (embed (first body))))
             ostream)))

(function "macro" [ostream istream]
  (do (var name (pop! istream))
      (var tmp (vector))
      (read-expression tmp istream)
      (var args (first tmp))
      (read-expression tmp istream)
      (var body (first tmp))
      (push! 'do ostream)
      (push! (qquote (push! (tuple (quote (embed name))
                                   (embed (length? args))
                                   (lambda (embed args) (embed body)))
                            macro-table))
             ostream)
      (push! (qquote (function (embed name)
                               (embed args)
                               (embed body)))
             ostream)))

## Reader functions:
(function parser-macro-call [token ostream istream]
  (do (var macro (third (assoc token macro-table)))
      (var argnum (second (assoc token macro-table)))
      (var args (vector))
      (var i 0)
      (do @until (equal? i argnum)
          (read-expression args istream)
          (set! i (+ i 1)))
      (push! (apply macro args) ostream)))

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
  (do (var list (vector))
      (do @until (equal? (first istream) delimiter)
          (unless istream (error "Missmatched `('."))
          (read-expression list istream))
      (pop! istream)
      (push! (reverse list) ostream)))

(function parse [input]
  (do (var istream (lex input syntax-table))
      (var ostream (vector))
      (do @until (empty? istream)
          (read-expression ostream istream))
      (reverse ostream)))

(function repl [__depth]
  (do (var __prompt " > ")
      (write __depth)
      (loop (catch (do (write __prompt)
                       (var __input (readln))
                       (var __parsed (eval (first (parse __input))))
                       (write \tab (if __parsed __parsed " ") \newline))
                   (lambda [error]
                     (do (write \tab error \newline)
                         (repl (+ 1 __depth)))))
            (write __depth))))
