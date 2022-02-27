;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 14 :weight 'semi-light)
      ;; doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Menlo" :size 18 :weight 'semi-light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-ayu-mirage)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; -----------------------------------------------
;; Elm
;; -----------------------------------------------

;; NOTE: I use dir-locals to set tell flycheck to use elm-test for spec files
;; that is done by calling elm-test-runner--buffer-is-test-p, which is not
;; autoloaded. I should change the package to autoload that function, but in the
;; meantime this will do.
(add-hook! elm-mode (require 'elm-test-runner))

(setq elm-format-on-save t)

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
  "Selects an elm file interactively and adds an import for the corresponding module"
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

(map! :map elm-mode-map
      (:leader
       (:prefix ("m i" . "import")
        :desc "Add import"              "i"   #'elm/import
        :desc "Add import from file"    "f"   #'elm/import-from-file)
       (:prefix ("m t" . "tests")
        :desc "Run tests in buffer"                    "b"   #'elm-test-runner-run
        :desc "Re-run last test"                       "r"   #'elm-test-runner-rerun
        :desc "Toggle between test and implementation" "TAB" #'elm-test-runner-toggle-test-and-target
        )))

;; -----------------------------------------------
;; Idris
;; -----------------------------------------------

(setq idris-interpreter-path "idris2"
      idris-stay-in-current-window-on-compiler-error t
      idris-show-help-text nil)

(defun load-idris-file-hook ()
  (when (and buffer-file-name
             (eq major-mode 'idris-mode))
    (idris-load-file)))

(add-hook 'after-save-hook #'load-idris-file-hook)
(add-hook 'idris-mode-hook #'load-idris-file-hook)

(add-hook! idris-info-mode (text-scale-adjust -1))
(add-hook! idris-hole-list-mode (text-scale-adjust -1))

(use-package! idris-settings
 :config
 (require 'tree-sitter)
 (put 'idris-semantic-type-face 'face-alias 'tree-sitter-hl-face:type)
 (put 'idris-semantic-data-face 'face-alias 'tree-sitter-hl-face:constructor)
 (put 'idris-semantic-function-face 'face-alias 'tree-sitter-hl-face:function)
 (put 'idris-semantic-bound-face 'face-alias 'tree-sitter-hl-face:variable.parameter))

(set-popup-rule!
  "^\\**idris-.*\\*"
  :side 'bottom
  :slot 1
  :quit t
  :select nil
  :size #'+popup-shrink-to-fit)


(map! :map idris-mode-map
      (:leader
       "m C" #'idris-make-cases-from-hole
       "m T" #'idris-type-at-point))

;; -----------------------------------------------
;; Flycheck
;; -----------------------------------------------
(flycheck-posframe-configure-pretty-defaults)
(setq
  flycheck-highlighting-mode 'lines
  flycheck-navigation-minimum-level 'error
  flycheck-posframe-border-width 10
  flycheck-posframe-prefix "\u24be "
  flycheck-posframe-info-prefix "\u24be "
  flycheck-posframe-position 'frame-bottom-right-corner
  )

(flycheck-posframe-configure-pretty-defaults)
(set-face-attribute 'flycheck-posframe-face         nil :height 0.9)
(set-face-attribute 'flycheck-posframe-info-face    nil :height 0.9)
(set-face-attribute 'flycheck-posframe-warning-face nil :height 0.9)
(set-face-attribute 'flycheck-posframe-error-face   nil :height 0.9)

(global-centered-cursor-mode)

(map! :leader
      (:prefix ("e" . "compilation errors")
       :desc "Go to first error"     "f" #'flycheck-first-error
       :desc "Go to next error"      "n" #'flycheck-next-error
       :desc "Go to previous error"  "p" #'flycheck-previous-error
       :desc "Recompile buffer"      "r" #'flycheck-buffer
       ))

;; -----------------------------------------------
;; Formatters
;; -----------------------------------------------

(use-package! reformatter
  :config
  (reformatter-define ormolu
    :program "ormolu")

  (reformatter-define fourmolu
    :program "fourmolu")

  (reformatter-define prettier-format
    :program "prettier"
    :args
    (cond
     (buffer-file-name (list "--stdin-filepath" buffer-file-name))
     ((eq major-mode 'js2-mode) (list "--parser" "babel")))))

;; -----------------------------------------------
;; Theme Customizations
;; -----------------------------------------------

(custom-theme-set-faces! 'doom-monokai-pro
  '(evil-ex-lazy-highlight :background "#6b696b" :foreground "#FCFCFA" :distant-foreground "#19181A"))

;; -----------------------------------------------
;; Misc
;; -----------------------------------------------

(setq
  enable-local-variables t

  indicate-empty-lines nil

  ;; disable tooltip with docs on hover
  lsp-ui-doc-enable nil

  ;; use regular posframe errors for lsp instead of the custom sideline
  lsp-ui-sideline-enable nil

  ruby-insert-encoding-magic-comment nil

  ;; tune initial frame size
  initial-frame-alist '((width . 80) (height . 30))

  git-link-use-single-line-number nil

  ivy-count-format ""

  doom-font-increment 1)

(use-package! treemacs
  :config
  (setq treemacs-width 30
        doom-themes-treemacs-enable-variable-pitch nil)
  (add-hook! treemacs-mode (text-scale-adjust -1)))

(use-package! which-key
  :config
  (setq which-key-idle-delay 0.5))

(use-package! counsel-projectile
  :config
  (counsel-projectile-modify-action
    'counsel-projectile-switch-project-action
    '((default counsel-projectile-switch-project-action-vc))))

; the javascript module adds node_modules/.bin to execpath by default. i don't like that.
(remove-hook '+javascript-npm-mode-hook 'add-node-modules-path)

; stolen from spacemacs :-)
(defun misc/close-compilation-window ()
  "Close the window containing the '*compilation*' buffer."
  (interactive)
  (when compilation-last-buffer
    (delete-windows-on compilation-last-buffer)))

; stolen from spacemacs :-)
(defun misc/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((window-tree (car (window-tree)))
             (current-split-vertical-p (car window-tree))
             (first-window (nth 2 window-tree))
             (second-window (nth 3 window-tree))
             (second-window-state (window-state-get second-window))
             (splitter (if current-split-vertical-p
                           #'split-window-horizontally
                         #'split-window-vertically)))
        (delete-other-windows first-window)
        ;; `window-state-put' also re-selects the window if needed, so we don't
        ;; need to call `select-window'
        (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))


(map!
  :i "s-s" (lambda () (interactive) (evil-escape) (save-buffer))
  :n "C-H" #'evil-first-non-blank
  :n "C-L" #'evil-end-of-line
  :n "C-SPC" #'ivy-switch-buffer)

(map! :leader
      (:prefix ("g l" . "list/link")
       :desc "Copy Github link"              "l" #'git-link
       :desc "Open Github link in browser"   "o" (lambda () (interactive) (let ((git-link-open-in-browser t)) (call-interactively 'git-link)))
       )

      (:prefix ("s" . "search/symbol")
       :desc "Reset highlight"               "c" #'evil-ex-nohighlight
       :desc "Highlight symbol under point"  "h" (lambda () (interactive) (evil-ex-start-word-search nil 'forward 0 t))
       )

      (:prefix ("i l" . "lorem ipsum")
       :desc "Insert paragraph" "l" #'lorem-ipsum-insert-list
       :desc "Insert paragraph" "p" #'lorem-ipsum-insert-paragraphs
       :desc "Insert paragraph" "s" #'lorem-ipsum-insert-sentences
       )

      ; same as default, but also switch focus to the sidebar
      :desc "Find file in project sidebar" "o P" (lambda () (interactive) (treemacs-find-file) (treemacs-select-window))

      "SPC" #'counsel-M-x

      "TAB" #'evil-switch-to-windows-last-buffer

      "c q" #'misc/close-compilation-window
      "c y" #'evilnc-copy-and-comment-lines

      "i k" #'+evil/insert-newline-above
      "i j" #'+evil/insert-newline-below

      ; skip doom's wrapper (+ivy/projectile-find-file) which uses the incorrect
      ; cwd to build the target file when opening target in another window via
      ; C-o j (will use the current directory instead of the project root)
      "p f" #'counsel-projectile-find-file

      "w +" #'misc/window-layout-toggle
      "w v" #'+evil/window-vsplit-and-follow
      "w s" #'+evil/window-split-and-follow

      "w x" #'kill-buffer-and-window
      )

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(remove-hook! 'doom-modeline-mode-hook #'size-indication-mode)

(use-package! evil-matchit
  :config
  (add-hook! 'ruby-mode-hook #'evil-matchit-mode))

(use-package! dhall-mode
  :ensure t
  :mode "\\.dhall\\'")
