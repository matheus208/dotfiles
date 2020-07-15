(package-install 'use-package)
(package-install 'delight)
(require 'use-package)
(setq explicit-shell-file-name "/usr/bin/bash")

(use-package auto-package-update
  :ensure t
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package diminish
  :ensure t)
