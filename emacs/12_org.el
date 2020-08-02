(use-package org
  :ensure t
  :bind
  (:map org-mode-map
	("C-c a" . org-agenda))
  :config
  (setq org-support-shift-select +1))
