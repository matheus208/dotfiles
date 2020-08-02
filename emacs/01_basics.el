(require 'color)
(global-display-line-numbers-mode)
(column-number-mode 1)
(show-paren-mode t)
(setq vc-follow-symlinks t)
;;(setq debug-on-error t)
(setq debug-on-error nil)
(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 10000)
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq create-lockfiles nil)

;; Exits safely server mode
(defalias 'exit 'save-buffers-kill-emacs)

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

;; Windmove (s-<arrow keys> will move focus to window)
(windmove-default-keybindings)


;; set shortcut to kill whole emacs session
(global-set-key (kbd "C-x c") 'save-buffers-kill-emacs)
(global-set-key (kbd "M-o") 'other-window)

;; Kill region, if region is set. Otherwise, kill backward
(defun obar/kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(global-set-key (kbd "C-w") 'obar/kill-region-or-backward-word)


;; backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; MacOS
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; Documentation popup
(use-package eldoc
  :ensure
  :delight
  :hook (prog-mode . eldoc-mode))

;; Project manager
(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "out")
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  :bind-keymap
  (("s-p". projectile-command-map)
   ("C-c p" . projectile-command-map)))

;; WinnerPac
(use-package winner
  :ensure t
  :custom
  (winner-boring-buffers
   '("*Completions*"
     "*Compile-Log*"
     "*inferior-lisp*"
     "*Fuzzy Completions*"
     "*Apropos*"
     "*Help*"
     "*cvs*"
     "*Buffer List*"
     "*Ibuffer*"
     "*esh command on file*"))
  :config
  (winner-mode 1))
(provide 'init-winner)
