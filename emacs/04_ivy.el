;; (use-package ivy
;;   :ensure t
;;   :defer t
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-count-format "(%d/%d)"))

;; (use-package amx
;;   :defer t
;;   :ensure t
;;   :after ivy)

(use-package ivy
  :diminish
  :init
  (use-package amx :ensure t :defer t)
  (use-package counsel :ensure t
    :diminish
    :config (counsel-mode 1))
  (use-package swiper :ensure t :defer t)
  (ivy-mode 1)
  :bind
  (("C-s" . swiper-isearch)
   ;("C-z s" . counsel-rg)
   ;("C-z b" . counsel-buffer-or-recentf)
   ;("C-z C-b" . counsel-ibuffer)
   (:map ivy-minibuffer-map
         ("C-r" . ivy-previous-line-or-history)
         ("M-RET" . ivy-immediate-done))
   (:map counsel-find-file-map
         ("C-~" . counsel-goto-local-home)))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "【%d/%d】")
  (ivy-wrap t)
  :config
  (defun counsel-goto-local-home ()
      "Go to the $HOME of the local machine."
      (interactive)
    (ivy--cd "~/")))
