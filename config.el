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
(setq doom-font (font-spec :family "Monaco" :size 18 :weight 'semi-light))

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

;; make sure envrc is loaded before detecting available checkers
(add-hook 'flycheck-before-syntax-check-hook #'+direnv-init-h)

;; -----------------------------------------------
;; Misc
;; -----------------------------------------------

(setq
  indicate-empty-lines nil

  ;; use regular posframe errors for lsp instead of the custom sideline
  lsp-ui-sideline-enable nil

  ruby-insert-encoding-magic-comment nil

  ;; tune initial frame size
  initial-frame-alist '((width . 80) (height . 30))
  )

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

; formatters!
(use-package! reformatter
  :config
  (reformatter-define prettier-format
    :program "prettier"
    :args
    (cond
     (buffer-file-name (list "--stdin-filepath" buffer-file-name))
     ((eq major-mode 'js2-mode) (list "--parser" "babel")))))

; the javascript module adds node_modules/.bin to execpath by default. i don't like that.
(remove-hook '+javascript-npm-mode-hook 'add-node-modules-path)

(map!
  :i "s-s" (lambda () (interactive) (evil-escape) (save-buffer))
  :n "C-H" #'evil-first-non-blank
  :n "C-L" #'evil-end-of-line
  :n "C-SPC" #'ivy-switch-buffer
  )

(map! :leader
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

      "c y" #'evilnc-copy-and-comment-lines

      "i k" #'+evil/insert-newline-above
      "i j" #'+evil/insert-newline-below

      "w v" #'+evil/window-vsplit-and-follow
      "w s" #'+evil/window-split-and-follow

      "w x" #'kill-buffer-and-window
      )
