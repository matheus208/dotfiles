;; init.el --- Initialization file for Emacs


;;; Commentary:
;; Forked from https://gist.github.com/matheusemm/d3b216a0369c3af6f4d83549276bacb1/5706bf75f52e3d37d521d3b6eaddeafbf091bd2c

(require 'package)

;;; Code:

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)
(package-refresh-contents)

;; Add dependencies here first, they will be downloaded in case your machine don't have them yet
(let ((dependencies-list (list 'undo-tree
                               'ivy
                               'smex
                               'projectile
                               'flycheck
                               'magit
                               'exec-path-from-shell
                               'company
                               'company-box
                               'cider
                               'clj-refactor
                               'flycheck-clj-kondo
                               'use-package
                               'clojure-mode
                               'clojure-mode-extra-font-locking
                               'smartparens
                               'rainbow-delimiters
                               'lsp-mode
                               'lsp-ui
                               'lsp-ivy
                               'which-key
                               'multiple-cursors
                               'doom-themes
                               'all-the-icons
                               'doom-modeline
                               'solaire-mode
                               ;'emidje
                               )))
  (mapc (lambda (pkg)
          (unless (package-installed-p pkg)
            (package-install pkg)))
        dependencies-list))

;; use-package
(eval-when-compile
  (require 'use-package))

;; emidje
;;(eval-after-load 'cider #'emidje-setup)

;; font

(set-face-attribute 'default nil :family "Fira Code")
(set-face-attribute 'default nil :height 155)


;; undo-tree

(global-undo-tree-mode)

;; ivy
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d "))

;; smex
(use-package smex
  :config
  (smex-initialize)
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)
   ("C-c C-c M-x" . execute-extended-command)))

;; projectile
(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  :bind-keymap
  (("s-p". projectile-command-map)
   ("C-c p" . projectile-command-map)))

;; magit
(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)))

;; exec-path-from-shell
(exec-path-from-shell-initialize)

;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; company-mode
(global-company-mode)
(use-package company-box
  :hook
  (company-mode . company-box-mode))

;; cider
(use-package cider
  :ensure t
  :defer t
  :hook
  ((cider-repl-mode . subword-mode)
   (cider-repl-mode . smartparens-strict-mode)
   (cider-repl-mode . rainbow-delimiters-mode)
   (cider-repl-mode . company-mode)
   (cider-mode . company-mode)
   (cider-mode . clj-refactor-mode))

  ;; :config
  ;; (setq nrepl-log-messages t
  ;;       cider-repl-display-in-current-window t
  ;;       cider-repl-use-clojure-font-lock t
  ;;       cider-prompt-save-file-on-load 'always-save
  ;;       cider-font-lock-dynamically '(macro core function var)
  ;;       nrepl-hide-special-buffers t
  ;;       cider-overlays-use-font-lock t
  ;;       cider-repl-wrap-history t
  ;;       cider-repl-history-file "~/.emacs.d/cider-history"
  ;;       cider-save-file-on-load t)
  )

;; flycheck-clj-kondo

(require 'flycheck-clj-kondo)

;; editing

(show-paren-mode 1)
(global-hl-line-mode 1)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(setq-default indent-tabs-mode nil)

;; smartparens-mode
(use-package smartparens
  :defer t
  :ensure t
  :config
  (use-package smartparens-config)
  (sp-use-smartparens-bindings)
  (sp-pair "'" nil)
  (sp-pair "(" ")" :wrap "s-(")
  (sp-pair "[" "]" :wrap "s-[")
  :commands (smartparens-mode show-smartparens-mode))

;; clojure-mode
(use-package clojure-mode
  :hook
  ((clojure-mode . rainbow-delimiters-mode)
   (clojure-mode . subword-mode)
   (clojure-mode . smartparens-strict-mode))

  :config
  (require 'clojure-mode-extra-font-locking)
  (setq clojure-indent-style 'align-arguments
        clojure-align-forms-automatically t
        clojure-thread-all-but-last t
        cider-show-error-buffer 'only-in-repl
        cider-font-lock-dynamically '(macro core function var deprecated)
        yas-minor-mode 1)
  ;; (define-clojure-indent
  ;;   (fact 1)
  ;;   (facts 1)
  ;;   (flow 1)
  ;;   (fnk 1)
  ;;   (provided 1)
  ;;   (clojure.test.check/quick-check 2)
  ;;   (clojure.test.check.properties/for-all 2)
  ;;   (common-datomic.test-helpers/let-entities 2))
  )

;; clj-refactor
(use-package clj-refactor
  :defer t
  :ensure t
  :config
  (cljr-add-keybindings-with-prefix "C-c C-r")
  (setq cljr-clojure-test-declaration "[midje.sweet :refer :all]"
        clj-refactor-mode 1
        cljr-magic-require-namespaces
        '(("s"   . "schema.core")
          ("th"  . "common-core.test-helpers")
          ("gen" . "common-test.generators")
          ("d-pro" . "common-datomic.protocols.datomic")
          ("ex" . "common-core.exceptions")
          ("dth" . "common-datomic.test-helpers")
          ("t-money" . "common-core.types.money")
          ("t-time" . "common-core.types.time")
          ("d" . "datomic.api")
          ("m" . "matcher-combinators.matchers")
          ("pp" . "clojure.pprint")
          ("init" . "postman-aux.init"))))

;; lsp-mode
(use-package lsp-mode
  :hook
  ((clojure-mode . lsp))
  :config
  (dolist (m '(clojure-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))


;; (use-package lsp-mode
;;   :commands lsp
;;   :ensure t
;;   :hook ((clojure-mode . lsp)
;;          (clojurec-mode . lsp)
;;          (clojurescript-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :config
;;   ;; add paths to your local installation of project mgmt tools, like lein
;;   (setenv "PATH" (concat
;;                    "/usr/local/bin" path-separator
;;                    (getenv "PATH")))
;;   (dolist (m '(clojure-mode
;;                clojurec-mode
;;                clojurescript-mode
;;                clojurex-mode))
;;      (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
;;   (setq lsp-clojure-server-command '("bash" "-c" "clojure-lsp")
;;         lsp-signature-auto-activate nil)
;;   (advice-add #'lsp-rename :after (lambda (&rest _) (projectile-save-project-buffers))))

;; (use-package lsp-ui
;;   :after lsp-mode
;;   :commands lsp-ui-mode
;;   :config
;;   (setq lsp-ui-peek-list-width 60
;;         lsp-ui-peek-fontify 'always))

;; (use-package lsp-ivy
;;   :commands lsp-ivy-workspace-symbol)

(use-package which-key
  :config (which-key-mode))

(use-package multiple-cursors
  :bind ("C-c m c" . mc/edit-lines))


;; NuBank
;; (let ((nudev-emacs-path "~/dev/nu/nudev/ides/emacs/"))
;;   (when (file-directory-p nudev-emacs-path)
;;     (add-to-list 'load-path nudev-emacs-path)
;;     (require 'nu)))

;; misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq create-lockfiles nil)
;; brew install pandoc ; to generate/compile/preview  markdown files
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(ivy-mode t)
 '(markdown-command "/usr/local/bin/pandoc")
 '(package-selected-packages
   (quote
    (lsp-mode aggressive-indent-mode company-box company company-mode undo-tree clojure-mode-extra-font-locking clojure-mode exec-path-from-shell magit projectile smex ivy smartparens rainbow-delimiters zenburn-theme))))

;; Themes/Visual!

(menu-bar-mode -1)
(global-linum-mode)
(scroll-bar-mode -1)
(setq-default frame-title-format "%b (%f)")
(setq ring-bell-function 'ignore)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config)
  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;;(doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package all-the-icons)


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))


;; Bugfixes

;; do not touch


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

;;; init.el ends here
