;;; ivy-regex.el -*- lexical-binding: t; -*-

(defun jedi/ivy-regex--split-string-keeping-separators (string regex)
  "Helper function to split an input string into segments using a regex for
separators. Unlike `split-string', the separators are returned as part of the
resulting list."
  (let ((case-fold-search nil)
        (i 0)
        (len (length string))
        (list nil))
    (while (and (< i len)
                (string-match regex string i))
      ; add anything between the current point and the beginning of the match
      (when (< i (match-beginning 0))
          (push (substring string i (match-beginning 0)) list))
      ; add the match itself
      (push (substring string (match-beginning 0) (match-end 0)) list)
      (setq i (match-end 0)))

    ;; add any remaining text after the last match
    (if (< i len)
        (push (substring string i len) list))
    (nreverse list)))

(defun jedi/ivy-regex--or (regex1 regex2)
  "Builds a regular expression that matches an input if and only
if either one of the two sub-regexes matches."
  (concat "\\(?:" regex1 "\\)\\|\\(?:" regex2 "\\)"))

(defun jedi/ivy-regex--part-to-regex (part)
  "function that turns an uppercase string into a pattern that
interprets each char as an initial by intercalating a \".*\" to
match any intermediate characters followed by \"\\b\" (word
boundary). e.g.: \"ABC\" -> \"A.*\\bB.*\\b\""
  (if (string-equal (upcase part) part)
      (jedi/ivy-regex--uppercase-sequence-to-regex part)
    part))

(defun jedi/ivy-regex--uppercase-sequence-to-regex (part)
  "builds the regex used for a sequence of uppercase characters.
these are interpreted as either:

- a literal
- a sequence of initials
"
  (let*
      ((chars (split-string part "" t "[[:blank:]]*"))
       (joiners (make-list (length chars) ".*"))
       (initials-regex (apply 'concat (butlast (-interleave chars joiners)))))
    (jedi/ivy-regex--or part initials-regex)))

(defun jedi/ivy-regex (input)
  "Function to turn a query supplied by ivy into the regex that
we'll use to filter results. Works pretty much like
`ivy--regex-ignore-order', except that it does a bit of
pre-processing to allow using sequences of uppercase characters
as initials.

As an example, this means that \"PLHMain\" will match \"Page/Learn/Home/Main.elm\""
  (let* (; tokenize the query using sequences of uppercase characters as separators
         ; e.g.: "PLHMain" -> ("PLHM" "ain")
         (parts (jedi/ivy-regex--split-string-keeping-separators input "[[:upper:]][[:upper:]]+"))
         ;; turns each part into a regex
         ; e.g.: ("PLHM" "ain") -> ("(P.*L.*H.*M|PLHM)" "ain")
         (transformed-parts (seq-map #'jedi/ivy-regex--part-to-regex parts))
         ; joins the transformed segments to build a new query
         ; e.g.: ("(P.*L.*H.*M|PLHM)" "ain") -> "(P.*L.*H.*|PLH)Main"
         (query (string-join transformed-parts "")))
    ; after pre-processing the query, call `ivy--regex-ignore-order' which
    ; basically:
    ;   - splits the input at whitespaces to get a list of subqueries
    ;   - runs each of those as a separate filter
    (ivy--regex-ignore-order query)))

; ------- TESTS

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

;; sequences of uppercase letters are interpreted as as "initials" with no separator needed between them
(jedi/ivy-regex--check "FB" "FooBar.txt")

;; sequences of uppercase chars also match as a literal
(jedi/ivy-regex--check "PLH" "PLH.txt")
