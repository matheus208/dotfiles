(use-package magit
  :ensure t
  :defer t
  :commands
  (magit-status magit-dispatch-popup magit-list-repositories)
  :custom
  (magit-repository-directories '(("~/repos/" . 3))))

(use-package smartparens
  :ensure t
  :defer t
  :delight
  :commands
  (smartparens-mode smartparens-global-mode show-smartparens-mode)
  :hook
  ((prog-mode markdown-mode) . smartparens-mode)
  :config
  (use-package smartparens-config)
  (sp-use-smartparens-bindings)
  (sp-pair "'" nil)
  (sp-pair "(" ")" :wrap "s-(")
  (sp-pair "[" "]" :wrap "s-["))

(use-package expand-region
  :ensure t
  :defer t
  :bind
  (("C-@" . er/expand-region)
   ("C-M-@" . er/contract-region))
  :commands
  (er/expand-region er/contract-region))

(use-package which-key
  :defer t
  :ensure t
  :delight
  :hook (after-init . which-key-mode))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure)
