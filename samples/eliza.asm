################################################################################
# Elizaesque example:
# 
# > (eliza)
# Eliza > Hello Eliza
# How do you do. Please state your problem.
# Eliza > quit
####################

(var +pattern-character+ "?")

(function variable? (x)
  (and (symbol? x)
       (equal? (first (stringof x))
               +pattern-character+)))

(function matches? (pattern list)
  (if (and pattern list)
      (and (if (variable? (first pattern))
               'yup
               (equal? (first pattern)
                       (first list)))
           (matches? (rest pattern)
                     (rest list)))
      (and (not pattern)
           (not list))))

(function match (pattern list)
  (when pattern
        (if (variable? (first pattern))
            (join (tuple (first pattern)
                         (first list))
                  (match (rest pattern)
                         (rest list)))
            (match (rest pattern)
                   (rest list)))))

(function reply (input rules)
  (when (var rule (first rules))
        (if (matches? (first rule) input)
            (sublist (match (first rule) input)
                     (random (rest rule)))
            (reply input (rest rules)))))

(function sublist (alist list)
  (when (var atom (first list))
    (if (var assoced (assoc atom alist))
        (join (second assoced)
              (sublist alist (rest list)))
        (join atom
              (sublist alist (rest list))))))

(function pretty-read-from-string (str)
  (read-from-string (append "(" str ")")))

(function pretty-read ()
  (pretty-read-from-string (readln)))

(function pretty-string (list)
  (when list
    (append (stringof (first list)) \s
    (pretty-string (rest list)))))

(function pretty-write (list)
  (write (pretty-string list) "\n"))

(function eliza ()
  (do (write "Eliza > ")
      (var input (pretty-read))
      (unless (equal? input '(quit))
              (if (var rep (reply input *rules*))
                  (pretty-write rep)
                  (write "I don't understand.\n"))
              (eliza))))

# The rules

(var *rules*
 '(((Would you like ?x)
   (Do you want me to eat ?x))
  ((Hello ?x)
   (How do you do. Please state your problem.))
  ((I want ?x)
   (What would it mean if you got ?x))
  ((Why do you want ?x)
   (Suppose you got ?X soon))))
