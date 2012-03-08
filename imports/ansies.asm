# ANSI Escape Sequences

(function CSI (args type)
  (append "\x1B[" args type))

(macro color (color @tuple args)
  (append '(append)
           (zipOne (CSI color "m") args)
           (CSI "0" "m")))

(macro __defineColors (colors)
  (do (function makeColor (arg)
        `(macro $(first arg) (@tuple args)
                (append '(color $(second arg))
                        args)))
      (append '(do)
              (map makeColor colors))))

(__defineColors
   # Foreground colors
  ((black       "30")
   (red         "31")
   (green       "32")
   (yellow      "33")
   (blue        "34")
   (magenta     "35")
   (cyan        "36")
   (white       "37")
   # Background colors
   (black-bg    "40")
   (red-bg      "41")
   (green-bg    "42")
   (yellow-bg   "43")
   (blue-bg     "44")
   (magenta-bg  "45")
   (cyan-bg     "46")
   (white-bg    "47")
   # Brightness
   (bright      "1")
   (faint       "2")
   # Attributes
   (itallic     "3")
   (underline   "4")
   (blink       "5")
   (blinkf      "6")
   (inverse     "7")
   (conceal     "8")
   (strike      "9")
   ))