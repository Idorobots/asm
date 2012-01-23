################################################################################
# ASM Class example
# 
# > (var hello ((HelloWorld! new)))
# > (hello string)
#     "Hello world!"
# > ((hello sayHi))
# Hello world!
####################

# The class:
(class HelloWorld! {
    (static var string "Hello world!")

    (function sayHi ()
      (write string "\n"))
})
