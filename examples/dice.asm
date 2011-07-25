## Dice example:

(import 'imports.core)

(def roll (dice)                                # Dice rolling function.
    (if (tuple? dice)
        (random dice)))

(def rollRandom ()
    (roll (roll dRANDOM)))

(def d (dice)                                   # An additional dice rolling function.
    (roll (range 1 (dice + 1))))

(var d4 (range 1 5))
(var d6 (range 1 7))
(var d8 (range 1 9))
(var d10 (range 1 11))
(var d100 (range 1 101))
(var d% d100)                                   # An alias for the d100
(var d20 (range 1 21))
(var d30 (range 1 31))
(var dHIT '("head" "upper body" "lower body"
            "left arm" "right arm" "left leg"
            "right leg"))
(var dRANDOM (tuple d4 d6 d8 d10 d% d20 d30))

## Roll some dice:

(roll 2)                                        # Fnord!
#(rollRandom)
(roll d4)
(* 2 (roll d20))
(2 * (roll d20))
(d 4)
(+ (* 2 (d 4)) 3)
((2 * (d 4)) + 3)

(def hitTheOrc ()                               # Ouch!
    `(You hit the Orc in the $(roll dHIT) for $((d 8) + 4) damage!))