## ASM LaTeX module.

(function LaTeX (arg)
  (append "$$" (stringof arg) "$$"))

(macro LaTeX-block (name @tuple args)
  `(lambda (arg)
     (append $(append "\\begin{" (stringof name) "}\n")
             (stringof arg)
             $(append "\\end{" (stringof name) "}"))))

(var \equation (LaTeX-block equation))
(var \equation* (LaTeX-block equation*))
(var \eqn \equation)
(var \eqn* \equation*)
(var \eqnarray (LaTeX-block eqnarray))
(var \eqnarray* (LaTeX-block eqnarray*))

(var \LaTeX "\\LaTeX")