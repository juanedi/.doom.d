(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(elm-test-runner))
 '(safe-local-variable-values
   '((eval progn
      (magit-disable-section-inserter 'magit-insert-tags-header)
      (magit-disable-section-inserter 'magithub-maybe-report-offline-mode)
      (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
      (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream))
     (flycheck-elm-executable . "elm-test")
     (rspec-use-bundler-when-possible)
     (lsp-enabled-clients sorbet-ls)
     (elm-test-runner-preferred-test-suffix . "Spec")
     (dante-methods bare-cabal)
     (magit-revision-insert-related-refs)
     (eval remove-hook 'server-switch-hook 'magit-commit-diff))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
