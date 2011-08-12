## ASM Class example

# The class:
(class HelloWorld! {
    (static var world " world!")
    (static function sayWorld []
        world)

    (function hello []
        `("Hello" $sayWorld))
})


# The instance:
(var say ((HelloWorld! new)))

# The use:
((say hello))          # Can access class variables.

((say sayWorld))       # And static variables.
(say world)

(say say)              # And even globals.
