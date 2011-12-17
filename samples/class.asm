## ASM Class example

# The class:
(class HelloWorld! {
    (static var world " world!")
    (static function getWorld ()
        world)

    (function hello (guy)
        (reduce join
                (tuple "Hello, "
                       guy
                       ". It's a beautiful"
                       (getWorld))))
})


# The instance:
(var say ((HelloWorld! new)))

# The use:
((say hello) "Bob")    # Can access class variables.

((say getWorld))       # And static variables.
(say world)

(say say)              # And even globals. :(
