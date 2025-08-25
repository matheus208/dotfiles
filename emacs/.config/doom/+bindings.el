;;; +bindings.el -*- lexical-binding: t; -*-

(setq ns-command-modifier 'super
      ns-option-modifier 'meta
      ns-alternate-modifier 'meta
      ns-control-modifier 'control
      ns-function-modifier 'hyper

      ns-right-command-modifier 'left
      ns-right-alternate-modifier 'left
      ns-right-option-modifier 'left
      ns-right-control-modifier 'left)

(map! :after simple
      :map smartparens-mode-map
      [C-<up>] #'matheus208/sp-reverse-transpose-sexp
      [C-<down>] #'sp-transpose-sexp)

(use-package! smartparens)

(map! :after smartparens
      :map sp-keymap
      "C-M-u" nil)

(map! :after smartparens
      :map smartparens-mode-map
      "C-M-f" #'sp-forward-sexp
      "C-M-b" #'sp-backward-sexp
      "C-M-u" #'sp-backward-up-sexp
      "C-M-e" #'sp-end-of-sexp
      "C-M-d" #'sp-down-sexp
      "C-M-p" #'sp-backward-down-sexp
      "C-M-n" #'sp-next-sexp
      "C-M-s" #'sp-splice-sexp
      "C-M-<right>" #'sp-forward-slurp-sexp
      "C-M-<left>" #'sp-forward-barf-sexp
      "C-S-M-<left>" #'sp-backward-slurp-sexp
      "C-S-M-<right>" #'sp-backward-barf-sexp
      "M-<backspace>" #'sp-raise-sexp)

(global-set-key (kbd "M-<up>") 'matheus208/sp-reverse-transpose-sexp)
(global-set-key (kbd "M-<down>") 'sp-transpose-sexp)
