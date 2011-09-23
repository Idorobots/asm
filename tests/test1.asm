# Test 1
# Status: fail

(write "Test 1\n")

(var foo '("\""))  # Confuses interpreter badly.

(write "Test 1\n") # Expected correct string.