;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nova)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; ----------------------------------------------------------------------------
;; Elm
;; ----------------------------------------------------------------------------

(setq elm-format-on-save t)

(add-to-list 'safe-local-variable-values
             '(flycheck-elm-executable . "elm-test"))

(defun elm/import (&optional input)
  "Prompts for an import statement to add to the current file"
  (interactive)
  (let ((statement (read-string "Import statement: " (concat "import " input))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^import " nil t)
          (beginning-of-line)
        (forward-line 1)
        (insert "\n"))
      (insert (concat statement "\n")))
    (elm-sort-imports)))

(defun elm/import-from-file ()
  "Selects an elm file interactively and adds an import for the
corresponding module"
  (interactive)
  (let*
      ((all-files (projectile-current-project-files))
       (elm-files (seq-filter (lambda (f) (s-ends-with-p ".elm" f)) all-files))
       (file-name (projectile-completing-read "Module to import: " elm-files)))
    (when file-name
      (let*
          ((full-file-name (expand-file-name file-name (projectile-project-root)))
           (module-name (with-current-buffer (find-file-noselect full-file-name)
                          (elm--get-module-name)))
           (aliased-module-name (elm/aliased-module-name module-name)))
        (elm/import aliased-module-name)))))

(defun elm/aliased-module-name (module-name)
  (let ((components (s-split "\\." module-name)))
    (if (< 1 (length components))
        (concat module-name " as " (car (last components)))
        (concat module-name))))

(map! :localleader
      :map elm-mode-map
      (:prefix ("i" . "import")
       :desc "Add import"              "i"   #'elm/import
       :desc "Add import from file"    "f"   #'elm/import-from-file)
      (:prefix ("t" . "tests")
       :desc "Run tests in buffer"                    "v"   #'elm-test-runner-run
       :desc "Re-run last test"                       "r"   #'elm-test-runner-rerun
       :desc "Toggle between test and implementation" "T" #'elm-test-runner-toggle-test-and-target
       ))

;; ----------------------------------------------------------------------------
;; Ruby
;; ----------------------------------------------------------------------------

(defun ruby/rspec-outline ()
  "Display an outline of specs in the current buffer without actually running them."
  (interactive)
  (rspec--autosave-buffer-maybe)
  (rspec-run-single-file (rspec-spec-file-for (buffer-file-name))
                         "--colour --dry-run --format doc --order defined"))

(defun rubocop-autocorrect-all-current-file ()
  "Run autocorrect on current file."
  (interactive)
  (rubocop--file-command "rubocop -A --format emacs"))

(map! :localleader
      :map ruby-mode-map
      "F" #'rubocop-autocorrect-all-current-file
      )

(map! :localleader
      :map rspec-mode-map
      :desc "Display an outline of the speec" "t o"   #'ruby/rspec-outline)

;; ----------------------------------------------------------------------------
;; Misc
;; ----------------------------------------------------------------------------

(setq
  mac-frame-tabbing nil

  enable-local-variables t

  indicate-empty-lines nil

  ruby-insert-encoding-magic-comment nil

  ;; tune initial frame size
  ;; initial-frame-alist '((width . 80) (height . 30))

  git-link-use-commit t
  git-link-use-single-line-number t

  doom-font-increment 1
  )

(setq-default line-spacing 4)

(setq projectile-switch-project-action
      (lambda ()
        (if (file-exists-p (expand-file-name ".git" (projectile-project-root)))
            (magit-status)
          (projectile-find-file))))

(global-centered-cursor-mode)

;; ----------------------------------------------------------------------------
;; Global Keybindings
;;
;; All keybindings should go here, except those defined that target a specific
;; mode (which are OK to keep with the rest of the mode's config).
;; ----------------------------------------------------------------------------

(map!
  :i "s-s" (lambda () (interactive) (evil-escape) (save-buffer))
  :n "C-H" #'evil-first-non-blank
  :n "C-L" #'evil-end-of-line
  :n "C-=" #'er/expand-region
  :n "C-c C-t" (lambda () (interactive) (find-file "~/Dropbox/Apps/org-notes/things.org"))
  )

(map! :leader
      (:prefix ("e" . "compilation errors")
       :desc "Go to first error"     "f" #'flycheck-first-error
       :desc "Go to next error"      "n" #'flycheck-next-error
       :desc "Go to previous error"  "p" #'flycheck-previous-error
       :desc "Recompile buffer"      "r" #'flycheck-buffer)

      (:prefix ("g l" . "list/link")
       :desc "Copy Github link"              "l" #'git-link
       :desc "Open Github link in browser"   "o" (lambda () (interactive) (let ((git-link-open-in-browser t)) (call-interactively 'git-link))))

      (:prefix ("s" . "search/symbol")
       :desc "Reset highlight"               "c" #'evil-ex-nohighlight
       :desc "Highlight symbol under point"  "h" (lambda () (interactive) (evil-ex-start-word-search nil 'forward 0 t)))

      (:prefix ("i l" . "lorem ipsum")
       :desc "Insert paragraph" "l" #'lorem-ipsum-insert-list
       :desc "Insert paragraph" "p" #'lorem-ipsum-insert-paragraphs
       :desc "Insert paragraph" "s" #'lorem-ipsum-insert-sentences)

      (:prefix ("T" . "themes")
        "T" #'jedi/cycle-theme
        "l" #'load-theme)

      ; same as default, but also switch focus to the sidebar
      :desc "Find file in project sidebar" "o P" (lambda () (interactive) (treemacs-find-file) (treemacs-select-window))

      "SPC" #'execute-extended-command

      "TAB" #'evil-switch-to-windows-last-buffer

      ;; "c q" #'misc/close-compilation-window
      "c y" #'evilnc-copy-and-comment-lines

      "i k" #'+evil/insert-newline-above
      "i j" #'+evil/insert-newline-below

      "j j" #'avy-goto-char-2

      ; skip doom's wrapper (+ivy/projectile-find-file) which uses the incorrect
      ; cwd to build the target file when opening target in another window via
      ; C-o j (will use the current directory instead of the project root)
      ;; "p f" #'counsel-projectile-find-file

      ;; "w +" #'misc/window-layout-toggle
      "w v" #'+evil/window-vsplit-and-follow
      "w s" #'+evil/window-split-and-follow
      "w x" #'kill-buffer-and-window

      ;; "0" #'treemacs-select-window
      ;; "1" #'winum-select-window-1
      ;; "2" #'winum-select-window-2
      ;; "3" #'winum-select-window-3
      ;; "4" #'winum-select-window-4
      ;; "5" #'winum-select-window-5
      ;; "6" #'winum-select-window-6
      ;; "7" #'winum-select-window-7
      ;; "8" #'winum-select-window-8
      ;; "9" #'winum-select-window-9
      )

