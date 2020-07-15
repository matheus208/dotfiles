
(use-package flycheck
  :ensure t
  :defer t
  :bind
  (("M-p" . flycheck-previous-error)
   ("M-n" . flycheck-next-error))
  :commands
  (flycheck-mode flycheck-previous-error flycheck-next-error))

(use-package flymake-diagnostic-at-point
  :defer t
  :ensure t
  :after flymake
  :custom
  (flymake-diagnostic-at-point-timer-delay 0.1)
  (flymake-diagnostic-at-point-error-prefix "☠ ")
  (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup)
  :hook
  (flymake-mode . flymake-diagnostic-at-point-mode))
