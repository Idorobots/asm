################################################################################
# A short game in ASM.
####################

# Places to go.
(var *nodes* '{(living-room (You are in the living-room. A wizard is snoring loudly on the couch.))
               (garden (You are in a beautiful garden. There is a well in front of you.))
               (attic (You are in the attic. There is a giant welding a torch in the corner.))})

# Paths to take.
(var *edges* '{(living-room (garden west door)
                            (attic upstairs ladder))
               (garden (living-room east door))
               (attic (living-room downstairs ladder))})

# Items to steal.
(var *objects* '{whiskey bucket frog chain})

# Where to steal them.
(var *object-locations* '[(whiskey living-room)
                          (bucket living-room)
                          (frog garden)
                          (chain garden)])

# Where we at, yo.
(var *location* 'living-room)

(function describe-location [location nodes]
  (second (assoc location nodes)))

(function describe-path [edge]
  `(There is a $(third edge) going $(second edge) from here.))

(function describe-paths [location edges]
  (apply append (map describe-path (rest (assoc location edges)))))

(function objects-at [loc objs obj-locs]
  (objs [obj -> (equal? (second (assoc obj obj-locs))
                       loc)]))

(function describe-objects [loc objs obj-locs]
  (apply append (map [obj -> `(You see a $obj on the floor.)]
                     (objects-at loc objs obj-locs))))

(function look []
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(function walk [direction] {
  (var next (first (select (rest (assoc *location* *edges*))
                           [edge -> (equal? (second edge) direction)])))
  (if next {
      (set! *location* (first next))
      (look)
   }
   #else '(You can't go there.))
})

(function pickup [object]
  (if (member? object
               (objects-at *location* *objects* *object-locations*))
      (do (push! (tuple object 'inventory) *object-locations*)
          `(You are now carrying the $object))
      '(You can't get that.)))

(function inventory []
  (join 'inventory:
         (objects-at 'inventory *objects* *object-locations*)))

## Game repl:
(function game-read []
  (if (collection? (var command (read)))
      (tuple (first command) `(quote $(second command)))
      (tuple command)))

(function game-print [what] {
  (map [arg -> (write arg \space)]
       what)
  (write \newline)
})

(function game-repl [] {
  (var accepted-commands '{look walk pickup inventory quit})
  (if (equal? (first (var command (game-read)))
              'quit)
      (game-print '(Bye, bye.))
  #else {
      (if (member? (first command) accepted-commands)
          (game-print (eval command))
      #else (game-print '(I don't know this command.)))
      (game-repl)
  })
})

(function new-game [] {
  (set! *location* 'living-room)
  (game-print (look))
  (game-repl)
})