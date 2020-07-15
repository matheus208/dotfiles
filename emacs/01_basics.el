(require 'color)
(global-display-line-numbers-mode)
(column-number-mode 1)
(show-paren-mode t)
(setq vc-follow-symlinks t)
(setq debug-on-error t)
(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 10000)
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq create-lockfiles nil)

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Move Custom-Set-Variables to Different File
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;; Smooth Scrolling
;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)


;; MacOS
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; Documentation popup
(use-package eldoc
  :ensure
  :delight)

;; Project manager
(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  :bind-keymap
  (("s-p". projectile-command-map)
   ("C-c p" . projectile-command-map)))
