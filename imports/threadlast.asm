# ASM implementation of the thread last macro.

(function append* (a b)
  (if (fnord? a)
      (tuple b)
      (join (first a) (append* (rest a) b))))

(function ->>-prepend (a b)
  (if (tuple? b)
      (append* b a)
      (tuple b a)))

(macro ->> (@tuple exprs)
  (reduce prepend exprs))

# Example usage:

#(import 'imports.sort)
#(->> (map square (range 0 100))
      (msort >=)
      (map stringof)
      vectorof)