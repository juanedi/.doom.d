; use eval-buffer to run tests!
(defun jedi/ivy-regex--check (query candidate)
  (let ((regex (car (car (jedi/ivy-regex query)))))
    (cl-assert (string-match
                regex
                candidate)
               nil
               "%s did not match candidate: %s"
               query
               candidate
               )))

;; matching literals
(jedi/ivy-regex--check "foo" "foo")

;; splits query into segments that filter independently and disregarding order
(jedi/ivy-regex--check "foo bar" "barfoo")

;; sequences of uppercase letters are interpreted as as "initials"
(jedi/ivy-regex--check "PLH" "Page/Learn/Home/Main.elm")

;; sequences of uppercase chars also match as a literal
(jedi/ivy-regex--check "PLH" "PLH.txt")
