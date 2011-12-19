# Test 1
# Status: pass

(write "Test 1\n")

(var foo '("\""))  # Used to confuse parser badly.

(write "Test 1\n") # Expected correct string.
(error (reverse "tests/strings.asm(9):"))