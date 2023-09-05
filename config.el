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
;; - `doom-unicode-font' -- for unicode glyphs
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
(setq doom-font (font-spec :family "Menlo" :size 15))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

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
;; Themes
;; ----------------------------------------------------------------------------

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq jedi/themes '(timu-spacegrey
                    doom-ayu-mirage
                    doom-city-lights
                    doom-nord
                    doom-molokai
                    doom-nord-light
                    doom-solarized-light
                    )

      randomize-theme nil

      doom-theme (if randomize-theme (nth (random (length jedi/themes)) jedi/themes) (car jedi/themes)))

(defun jedi/cycle-theme ()
  (interactive)
  (let ((next-theme
         (or (car (cdr (member doom-theme jedi/themes)))
             (car jedi/themes))))
    (progn
        (message "%s" next-theme)
        (load-theme next-theme t)
        (setq doom-theme next-theme))))

(custom-theme-set-faces! 'doom-ayu-mirage
  '(evil-ex-lazy-highlight :background "#6b696b" :foreground "#FCFCFA" :distant-foreground "#19181A")

  ;; yellow bar in the active buffer; invisible in inactive ones
  '(doom-modeline-bar :background "#ffcc66")
  '(doom-modeline-bar-inactive :background "#141820")

  ;; make the current line number stand out more
  '(line-number              :foreground "#484f5b")
  '(line-number-current-line :foreground "#f5f7fd")

  '(copilot-overlay-face :slant italic :inherit 'shadow))

(custom-theme-set-faces! 'doom-ayu-dark
  '(line-number               :slant normal :foreground "#373b42")
  '(line-number-current-line  :slant normal)
  '(ivy-current-match  :background "#131721"))

(custom-theme-set-faces! 'doom-solarized-light
  '(line-number               :slant normal :foreground "#96A7A9")
  '(line-number-current-line  :slant normal :foreground "#556b72" :background "#d6d0bf")
  '(line-number-current-line  :slant normal :foreground "#556b72" :background "#d6d0bf")
  '(font-lock-type-face       :slant normal :foreground "#b58900")
  )

(custom-theme-set-faces! 'timu-spacegray
  '(ivy-posframe-border       :foreground "#232830" :background "#232830"))

;; ----------------------------------------------------------------------------
;; Elm
;; ----------------------------------------------------------------------------

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
;; Haskell
;; ----------------------------------------------------------------------------

(map! :localleader
      :map haskell-mode-map
      ;; HACK! Using elm import function because it just works :-)
      (:prefix ("i" . "import")
       :desc "Add import"              "i"   #'elm/import))

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

(use-package! evil-matchit
  :config
  (add-hook! 'ruby-mode-hook #'evil-matchit-mode))

; yard mode hook is throwing an error and messing up later hooks. don't really
; feel like debugging it yet.
(remove-hook! 'ruby-mode-hook 'yard-mode)

;; ----------------------------------------------------------------------------
;; Flycheck
;; ----------------------------------------------------------------------------
(defvar-local misc/flycheck-local-cache nil)

;; allow mode-dependand checker chains
;; see https://github.com/flycheck/flycheck/issues/1762
(defun misc/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker misc/flycheck-local-cache))
      (funcall fn checker property)))

(advice-add 'flycheck-checker-get :around 'misc/flycheck-checker-get)

;; configure rubocop to run right after lsp in ruby-mode
;; (add-hook 'lsp-managed-mode-hook
;;           (lambda ()
;;             (when (derived-mode-p 'ruby-mode)
;;               (setq misc/flycheck-local-cache '((lsp . ((next-checkers . (ruby-rubocop)))))))
;;             ))

(flycheck-posframe-configure-pretty-defaults)
(setq
  flycheck-highlighting-mode 'lines
  flycheck-navigation-minimum-level 'error
  flycheck-posframe-border-width 10
  flycheck-posframe-prefix "\u24be "
  flycheck-posframe-info-prefix "\u24be "
  flycheck-posframe-position 'frame-bottom-right-corner
  )

(set-face-attribute 'flycheck-posframe-face         nil :height 0.8)
(set-face-attribute 'flycheck-posframe-info-face    nil :height 0.8)
(set-face-attribute 'flycheck-posframe-warning-face nil :height 0.8)
(set-face-attribute 'flycheck-posframe-error-face   nil :height 0.8)

(global-centered-cursor-mode)

;; ----------------------------------------------------------------------------
;; Formatters
;; ----------------------------------------------------------------------------
(use-package! reformatter
  :config
  (reformatter-define ormolu
    :program "ormolu")

  (reformatter-define fourmolu
    :program "fourmolu")

  (reformatter-define nixfmt
    :program "nixfmt")

  (reformatter-define prettier-format
    :program "prettier"
    :args
    (cond
     (buffer-file-name (list "--stdin-filepath" buffer-file-name))
     ((eq major-mode 'js2-mode) (list "--parser" "babel")))))

;; ----------------------------------------------------------------------------
;; LSP
;; ----------------------------------------------------------------------------

(use-package! lsp-mode
  :config

  ;; Doom has some custom code for logging the project root after initializing
  ;; LSP that works well in general, but is problematic when the project root
  ;; can't be guessed automatically and requires prompting the user to select
  ;; it.
  ;; Specifically:
  ;;   - lsp mode calls `lsp--calculate-root' (indirectly)
  ;;   - `lsp--calculate-root' asks the user to interactively select the root
  ;;   - doom's hook calls `lsp--calculate-root' again to know which root to log (assuming it's a pure function)
  ;;   - ... which means the user gets asked twice!
  ;;
  ;;  Workaround is to remove doom's hook and reinstate the logging using advice
  ;;  instead.
  (remove-hook! 'lsp-mode-hook '+lsp-display-guessed-project-root-h)
  (defadvice! +lsp-display-guessed-project-root (root)
    "Log what LSP things is the root of the current project."
    ;; Makes it easier to detect root resolution issues.
    :filter-return #'lsp--calculate-root
    (if root
        (lsp--info "Guessed project root is %s" (abbreviate-file-name root))
      (lsp--info "Could not guess project root."))
    root))

(setq
  ;; disable tooltip with docs on hover
  lsp-ui-doc-enable nil

  lsp-lens-enable nil

  ;; use regular posframe errors for lsp instead of the custom sideline
  lsp-ui-sideline-enable nil)

;; ----------------------------------------------------------------------------
;; Ivy
;; ----------------------------------------------------------------------------

(setq ivy-count-format "")

(defun jedi/ivy-posframe-display (str)
  (ivy-posframe--display
   str
   (lambda (info) ; NOTE: The structure of INFO can be found in docstring of `posframe-show'.
     (cons (/ (- (plist-get info :parent-frame-width)
                 (plist-get info :posframe-width))
              2)
           (/ (plist-get info :parent-frame-height) 4)))))

(use-package! ivy-posframe
  :config
  (setf (alist-get t ivy-posframe-display-functions-alist) #'jedi/ivy-posframe-display)
  (setq
    ivy-posframe-border-width 3
    ivy-posframe-parameters
        '((left-fringe . 10)
          (right-fringe . 10))))

(load-file (concat doom-user-dir "ivy-regex.el"))
(ivy--alist-set 'ivy-re-builders-alist t 'jedi/ivy-regex)

;; ----------------------------------------------------------------------------
;; Copilot
;;
;; Heavily inspired by
;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
;; ----------------------------------------------------------------------------

(defvar jedi/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered,
e.g. via M-C-<return>.")

(use-package! copilot
  :after company
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word))
  :config
  (add-to-list 'copilot-disable-predicates (lambda () jedi/copilot-manual-mode))
  (setq-default company-idle-delay nil)
  (map! :i "C-SPC" 'jedi/copilot-complete-or-accept)
  (define-key! company-active-map "C-SPC" 'jedi/copilot-complete-or-accept)
  )

(defun jedi/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion)
    (progn
      (company-abort)
      (copilot-complete))))

(defun jedi/copilot-change-activation ()
  "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode jedi/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq jedi/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq jedi/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))


;; ----------------------------------------------------------------------------
;; Misc
;; ----------------------------------------------------------------------------

(setq
  mac-frame-tabbing nil

  enable-local-variables t

  indicate-empty-lines nil

  ruby-insert-encoding-magic-comment nil

  ;; tune initial frame size
  initial-frame-alist '((width . 80) (height . 30))

  git-link-use-commit t
  git-link-use-single-line-number t

  doom-font-increment 1

  ;; use encrypted token store for forge
  auth-sources '("~/.authinfo.gpg")
  )

(global-visual-line-mode 1)

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

; restore keybindings that magit overrides in blob mode
(map! :map magit-blob-mode-map
      :n "n" #'evil-ex-search-next
      :n "N" #'evil-ex-search-previous)

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

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook
            (lambda () (unless (member major-mode '(ruby-mode haskell-mode)) (tree-sitter-hl-mode)))))

(remove-hook! 'doom-modeline-mode-hook #'size-indication-mode)

;; (use-package! dhall-mode
;;   :ensure t
;;   :mode "\\.dhall\\'")

(let ((local-config-file "~/config.local.el"))
  (when (file-exists-p local-config-file)
    (load-file local-config-file)))

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
  :n "C-SPC" #'ivy-switch-buffer
  :n "C-=" #'er/expand-region)

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

      "SPC" #'counsel-M-x

      "TAB" #'evil-switch-to-windows-last-buffer

      "c q" #'misc/close-compilation-window
      "c y" #'evilnc-copy-and-comment-lines

      "i k" #'+evil/insert-newline-above
      "i j" #'+evil/insert-newline-below

      "j j" #'avy-goto-char-2

      ; skip doom's wrapper (+ivy/projectile-find-file) which uses the incorrect
      ; cwd to build the target file when opening target in another window via
      ; C-o j (will use the current directory instead of the project root)
      "p f" #'counsel-projectile-find-file

      "w +" #'misc/window-layout-toggle
      "w v" #'+evil/window-vsplit-and-follow
      "w s" #'+evil/window-split-and-follow
      "w x" #'kill-buffer-and-window

      "0" #'winum-select-window-0-or-10
      "1" #'winum-select-window-1
      "2" #'winum-select-window-2
      "3" #'winum-select-window-3
      "4" #'winum-select-window-4
      "5" #'winum-select-window-5
      "6" #'winum-select-window-6
      "7" #'winum-select-window-7
      "8" #'winum-select-window-8
      "9" #'winum-select-window-9)
