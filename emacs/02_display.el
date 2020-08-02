;(set-face-attribute 'default nil :family "Fira Code")
;(set-face-attribute 'default nil :height 155)

;; Input Mono, Monaco Style, Line Height 1.3 download from http://input.fontbureau.com/
;; Love LetterTW download from https://www.dafont.com/love-letter-tw.font
(defvar font-list '(("Input" . 11) ("SF Mono" . 12) ("Consolas" . 12) ("Fira Code" . 12) ("Love LetterTW" . 12.5))
  "List of fonts and sizes.  The first one available will be used.")
(defun change-font ()
  "Documentation."
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font font-list (setq available-fonts (nreverse available-fonts)))
      (when (member (car font) (font-family-list))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t) available-fonts)))
            (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting)))))

(change-font)

(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package all-the-icons
  :ensure t)

;; (use-package doom-themes
;;   :ensure t
;;   :custom
;;   (doom-themes-enable-italic t)
;;   (doom-themes-enable-bold t)
;;   (doom-challenger-deep-brighter-comments t)
;;   (doom-molokai-brighter-comments t)
;;   (doom-dracula-brighter-comments t)
;;   :custom-face
;;   ;; (default ((t (:background "#22232e"))))
;;   ;; (doom-modeline-bar ((t (:background "#6272a4"))))
;;   ;; (font-lock-comment-face ((t (:foreground "#e0e0e0"))))
;;   :config
;;   (doom-themes-neotree-config)
;;   (doom-themes-org-config))

(add-to-list 'load-path "~/.emacs.d/elegant-emacs")
(require 'elegance)
(require 'sanity)

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :delight
  :commands (highlight-indent-guides-mode)
  :hook ((yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character))

(use-package git-gutter
  :ensure t
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
  :custom-face
  (git-gutter:modified ((t (:background "#f1fa8c"))))
  (git-gutter:added    ((t (:background "#50fa7b"))))
  (git-gutter:deleted  ((t (:background "#ff79c6"))))
  :config
  (global-git-gutter-mode +1))

(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  ;; (doom-modeline-minor-modes t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (line-number-mode 1)
  (column-number-mode 1))

(use-package beacon
  :ensure t
  :defer t
  :delight
  :hook (prog-mode . beacon-mode)
  :commands (beacon-mode)
  ;; :bind (("C-b" . beacon-blink))
  :custom
  (beacon-color "#77fffa")
  (beacon-blink-duration 0.7)
  (beacon-size 100))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :delight
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :commands
  (rainbow-delimiters-mode))

(use-package ace-window
  :ensure
  :bind ("C-x C-o" . ace-window))

(use-package dashboard
  :ensure t
  :demand
  :diminish (dashboard-mode page-break-lines-mode)
  :init
  (global-set-key (kbd "C-z") nil)
  :bind
  (("C-z d" . open-dashboard)
   :map dashboard-mode-map
   (("n" . dashboard-next-line)
    ("p" . dashboard-previous-line)
    ("N" . dashboard-next-section)
    ("F" . dashboard-previous-section)))
  :custom
  (dashboard-banner-logo-title "emacs")
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents  . 7)
		     (projects . 5)))
  (initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  (dashboard-set-heading-icons t)
  (dashboard-set-navigator t)
  :custom-face
  (dashboard-banner-logo-title ((t (:family "Love LetterTW" :height 123))))
  :config
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  (dashboard-setup-startup-hook)
  ;; Open Dashboard function
  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (if (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)
    (goto-char (point-min))
    (delete-other-windows)))

(use-package whitespace
  :ensure t
  :delight
  :config
  (setq whitespace-style '(space-mark tab-mark face spaces trailing))
  (setq whitespace-display-mappings
        '(
          ;; (space-mark   ?\     [?\u00B7]     [?.]) ; space - centered dot
          (space-mark   ?\xA0  [?\u00A4]     [?_]) ; hard space - currency
          (space-mark   ?\x8A0 [?\x8A4]      [?_]) ; hard space - currency
          (space-mark   ?\x920 [?\x924]      [?_]) ; hard space - currency
          (space-mark   ?\xE20 [?\xE24]      [?_]) ; hard space - currency
          (space-mark   ?\xF20 [?\xF24]      [?_]) ; hard space - currency
          (space-mark ?\u3000 [?\u25a1] [?_ ?_]) ; full-width-space - square
          (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t]) ; tab - left quote mark
          ))
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (set-face-foreground 'whitespace-space "cyan")
  (set-face-background 'whitespace-space 'nil)
  (set-face-underline  'whitespace-trailing t)
  (set-face-foreground 'whitespace-trailing "cyan")
  (set-face-background 'whitespace-trailing 'nil))
(global-whitespace-mode)

;;awesome-tab
;;fill-column-indicator
;;presentation
;;volatile-highlights

