## The syntax table:

(var syntax-table '["\(" "\)" "\'" "," "\-\>" "/[^/]*/"])

## Syntax dispatchers:

(function "\(" [ostream istream]
  (do (var list (list))
      (read-delimited-list "\)" list istream)
      (push! (tupleof (first list)) ostream)))

(var "\)" "\)")

(function "\'" [ostream istream]
  (do (var tmp (list))
      (read-expression tmp istream)
      (push! (qquote (quote (embed (first tmp))))
             ostream)))

(function "," [ostream istream]
  (push! (tuple (pop! ostream) (pop! istream)) ostream))

(function "\-\>" [ostream istream]
  (do (var args (pop! ostream))
      (var body (list))
      (read-expression body istream)
      (push! (qquote (lambda (embed (if (collection? args)
                                        (listof args)
                                        (list args)))
                             (embed (first body))))
             ostream)))

(function "/[^/]*/" [ostream istream]
  (push! 'perkele ostream))

## Reader functions:

(function try-call [token ostream istream]
  (if (callable? (get token))
      ((get token) ostream istream)
      (push! (get token) ostream)))

(function read-expression [ostream istream]
  (if (member? (var token (pop! istream))
               syntax-table)
      (try-call token ostream istream)
      (push! token ostream)))

(function read-delimited-list [delimiter ostream istream]
  (do (var list (list))
      (do .until (or (fnord? istream)
                     (equal? (first list) delimiter))
          (read-expression list istream))
      (push! (reverse (rest list)) ostream)))

(function parse [input]
  (do (var istream (lex input syntax-table))
      (var ostream (list))
      (do .until (fnord? istream)
          (read-expression ostream istream))
      (reverse ostream)))

(function repl []
  ((lambda [__depth]
    (do (var __prompt " > ")
        (write __depth)
        (loop (catch (var __input (readln __prompt))
                     (var __parsed (eval (first (parse __input))))
                     (write \tab (if __parsed __parsed " ") \newline)
                     (do (write \tab error \newline)
                         (self (+ 1 __depth))))
              (write __depth))))
   0))