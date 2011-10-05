# Test 3
# Status: Fail

(import 'imports.core) # Due to this line...

(var bar '((foo bar) (bar foo)))

(assoc 'foo bar) # This now enters a call loop.

# ATM the reason for this is unclear, will have a go at this later.
# `assoc' proves to kill dasm, I still have to check, which symbols enter the infinite loop in these circumstances.