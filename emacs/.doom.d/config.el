;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Matheus S N"
      user-mail-address "m.serpellone@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(setq +lsp-auto-install-servers t
      +format-on-save-enabled-modes '(dart-mode)

      company-idle-delay 0.5
      company-minimum-prefix-length 1

      confirm-kill-emacs nil

      doom-font (font-spec :family "Hack" :size 18)
      doom-theme 'doom-solarized-light

      history-length 300
      indent-tabs-mode nil
      hover-command-path "~/go/bin/hover"

      gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      treemacs-no-png-images t)

(defun matheus208/sp-reverse-transpose-sexp (arg)
  (interactive "*p")
  (transpose-sexps (- arg)))

(defun matheus208/kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(global-set-key (kbd "C-w") 'matheus208/kill-region-or-backward-word)

(use-package! gptel
 :config
 (gptel-make-gh-copilot "Copilot")
 (setq gptel-model 'claude-3.7-sonnet
      gptel-backend (gptel-make-gh-copilot "Copilot")))

(defun matheus208/project-open-magit-or-default-doom-project-find-file (dir)
  "Jump to a file in DIR (searched recursively)."
  (magit-status dir))

(use-package! magit
  :after workspace
  :init (setq +workspaces-switch-project-function #'matheus208/project-open-magit-or-default-doom-project-find-file))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package! lsp-mode
  :commands lsp
  :config
  (setq lsp-semantic-tokens-enable t)
  (add-hook 'lsp-after-apply-edits-hook (lambda (&rest _) (save-buffer)))) ;; save buffers after renaming

(use-package! lsp-dart
  :hook (dart-mode . lsp-mode))

(use-package! projectile
  :config
  (setq projectile-project-search-path '("~/dev/"
                                         "~/dev/nu/"
                                         "~/dev/nu/mini-meta-repo/packages")
        projectile-enable-caching nil
        projectile-project-root-functions '(projectile-root-local
                                          projectile-root-top-down
                                          projectile-root-top-down-recurring
                                          projectile-root-bottom-up))

  (projectile-register-project-type 'flutter '("pubspec.yaml" ".metadata")
                                    :project-file "pubspec.yaml"
                                    :src-dir "lib/"
                                    :test-dir "test/"
                                    :test-suffix "_test"
                                    :test "flutter test"
                                    :configure "flutter pub get"
                                    :compile "flutter pub run build_runner build --delete-conflicting-outputs"))

(after! projectile
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(use-package! hover
  :after dart-mode
  :bind (:map hover-minor-mode-map
              ("C-M-z" . #'hover-run-or-hot-reload)
              ("C-M-x" . #'hover-run-or-hot-restart)
              ("C-M-p" . #'hover-take-screenshot))
  :init
  (setq hover-flutter-sdk-path (concat (getenv "HOME") "/flutter") ; remove if `flutter` is already in $PATH
        hover-command-path (concat (getenv "GOPATH") "/bin/hover") ; remove if `hover` is already in $PATH
        hover-hot-reload-on-save t
        hover-screenshot-path (concat (getenv "HOME") "/Pictures")
        hover-screenshot-prefix "my-prefix-"
        hover-observatory-uri "http://my-custom-host:50300"
        hover-clear-buffer-on-hot-restart t)
  (hover-minor-mode 1))

(let ((nudev-emacs-path "~/dev/nu/nudev/ides/emacs/"))
  (when (file-directory-p nudev-emacs-path)
    (add-to-list 'load-path nudev-emacs-path)
    (require 'nu nil t)))

(load! "+bindings")

(use-package lsp-metals
  :ensure t
  :custom
  ;; You might set metals server options via -J arguments. This might not always work, for instance when
  ;; metals is installed using nix. In this case you can use JAVA_TOOL_OPTIONS environment variable.
  (lsp-metals-server-args '(;; Metals claims to support range formatting by default but it supports range
                            ;; formatting of multiline strings only. You might want to disable it so that
                            ;; emacs can use indentation provided by scala-mode.
                            "-J-Dmetals.allow-multiline-string-formatting=off"
                            ;; Enable unicode icons. But be warned that emacs might not render unicode
                            ;; correctly in all cases.
                            "-J-Dmetals.icons=unicode"))
  ;; In case you want semantic highlighting. This also has to be enabled in lsp-mode using
  ;; `lsp-semantic-tokens-enable' variable. Also you might want to disable highlighting of modifiers
  ;; setting `lsp-semantic-tokens-apply-modifiers' to `nil' because metals sends `abstract' modifier
  ;; which is mapped to `keyword' face.
  (lsp-metals-enable-semantic-highlighting t)
  :hook (scala-mode . lsp))

;;"Fixes a bug in which ivy would print bogus messages if the find function didn't return anything"
(defadvice! +ivy--always-return-zero-exit-code-a (fn &rest args)
  :around #'counsel-rg
  (letf! (defun process-exit-status (proc)
           (let ((code (funcall process-exit-status proc)))
             (if (= code 2) 0 code)))
    (apply fn args)))

;;Runs projectile-discover-projects after cloning a new repo with nu-proj-clone
(defadvice! +projectile-discover-after-nu-proj-clone (fn &rest args)
  :around #'nu-project-clone
  (interactive)
  (when-let ((project (read-string "Project Name: ")))
    (shell-command (format "nu proj clone %s" project))
    (projectile-discover-projects-in-search-path)))
