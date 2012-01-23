################################################################################
# Size comparison of different ASM datatypes:
####################

(write "-- Sizes (just references):" \n)
(write "Number: " (sizeof 23.5) \n)
(write "Symbol: " (sizeof 'sizeof) \n)
(write "Tuple: " (sizeof '(1 2 3 4)) \n)
(write "Vector: " (sizeof '[1 2 3 4]) \n)
(write "Set: " (sizeof '{1 2 3 4}) \n)
(write "String: " (sizeof "Hello world!") \n)
(write "Function: " (sizeof (lambda (x) x)) \n)
(write "Builtin: " (sizeof sizeof) \n)
(write "Fnord!: " (sizeof ()) \n)
