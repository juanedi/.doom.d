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


(use-package! which-key
  :config
  (setq which-key-idle-delay 0.5))

(map!
  :i "s-s" (lambda () (interactive) (evil-escape) (save-buffer))
  :n "C-H" #'evil-first-non-blank
  :n "C-L" #'evil-end-of-line
  )

(map! :leader
      (:prefix ("e" . "Compilation errors")
       :desc "Go to first error"     "f" #'flycheck-first-error
       :desc "Go to next error"      "n" #'flycheck-next-error
       :desc "Go to previous error"  "p" #'flycheck-previous-error
       :desc "Recompile buffer"      "r" #'flycheck-buffer
       )
      (:prefix ("s" . "search/symbol")
       ;; :desc "Highlight symbol" "c" #'evil-ex-highligh
       :desc "Reset highlight"  "c" #'evil-ex-nohighlight
       )

      ;; override: follow after splitting by default
      "w v" #'+evil/window-vsplit-and-follow
      "w s" #'+evil/window-split-and-follow
      "w x" #'kill-buffer-and-window
      )

;; NOTE: I use dir-locals to set tell flycheck to use elm-test for spec files
;; that is done by calling elm-test-runner--buffer-is-test-p, which is not
;; autoloaded. I should change the package to autoload that function, but in the
;; meantime this will do.
(add-hook! elm-mode (require 'elm-test-runner))

(setq elm-format-on-save t)

(map! :map elm-mode-map
      (:leader
       (:prefix ("m t" . "Tests")
        :desc "Run tests in buffer"                    "b"   #'elm-test-runner-run
        :desc "Re-run last test"                       "r"   #'elm-test-runner-rerun
        :desc "Toggle between test and implementation" "TAB" #'elm-test-runner-toggle-test-and-target
        )))

(global-centered-cursor-mode)
