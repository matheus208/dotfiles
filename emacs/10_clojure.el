(use-package clojure-mode
  :ensure t
  :defer t
  :mode (("\\.clj$" . clojure-mode)
         ("\\.cljs$" . clojure-mode)
	 ("\\.edn$" . clojure-mode))
  :custom
  (clojure-indent-style 'align-arguments)
  (clojure-align-forms-automatically t)
  (clojure-thread-all-but-last t)
  (cider-show-error-buffer 'only-in-repl)
  (cider-font-lock-dynamically '(macro core function var deprecated))
  :init
  (add-hook 'clojure-mode-hook
            '(lambda ()
               (flycheck-mode t)
               (subword-mode t)
               (smartparens-strict-mode t)
               (rainbow-delimiters-mode t)
               (clj-refactor-mode t)
               (yas-minor-mode t)
               (add-hook 'before-save-hook 'delete-trailing-whitespace)))
  :commands
  (clojure-mode)
  :config
  (use-package clj-refactor
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
	    ("init" . "postman-aux.init")))))

(use-package cider
  :ensure t
  :defer t
  :init
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))
