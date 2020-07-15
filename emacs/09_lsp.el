(use-package lsp-mode
  :defer t
  :ensure t
  :init
  ;; Some of these don't play well with clojure-refactor, so we I disable them
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-completion-at-point nil)
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp)
  :custom
  (lsp-inhibit-message t)
  (lsp-enable-completion-at-point nil)
  (lsp-file-watch-threshold 3000)
  :config
  (require 'lsp-clojure)
  (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure"))
  (add-to-list 'lsp-language-id-configuration '(clojurec-mode . "clojure"))
  (add-to-list 'lsp-language-id-configuration '(clojurescript-mode . "clojurescript")))

(use-package lsp-ui
  :defer t
  :ensure t
  :config
  (setq lsp-ui-peek-list-width 60
        lsp-ui-peek-fontify 'always))

  :init
  (setq lsp-ui-doc-enable t
                 lsp-ui-doc-use-webkit nil
                 lsp-ui-doc-delay 0.2
                 lsp-ui-doc-include-signature t
                 lsp-ui-doc-position 'at-point
                 lsp-ui-doc-border (face-foreground 'default)

                 lsp-ui-sideline-enable t
                 lsp-ui-sideline-show-hover nil
                 lsp-ui-sideline-show-diagnostics nil
                 lsp-ui-sideline-show-code-actions t
                 lsp-ui-sideline-ignore-duplicate t

                 lsp-ui-imenu-enable t
                 lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                       ,(face-foreground 'font-lock-string-face)
                                       ,(face-foreground 'font-lock-constant-face)
                                       ,(face-foreground 'font-lock-variable-name-face)))

(use-package lsp-ivy
   :commands lsp-ivy-workspace-symbol)

(use-package company-lsp
  :defer t
  :ensure t
  :custom
  (company-lsp-cache-candidates 'auto)
  (company-lsp-match-candidate-predicate #'company-lsp-match-candidate-prefix)
  :config
  (setq company-lsp-match-candidate-predicate #'company-lsp-match-candidate-prefix)
  :after (lsp-mode))

(use-package dap-mode
  :defer t
  :ensure t
  :after (lsp-mode))
