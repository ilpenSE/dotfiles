;;; melpa
(require 'package)

; disable package sign logic
(setq package-check-signature nil)

; gnu packages
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))

(package-initialize)

;;; =========== PACKAGES ==========

; loading other lisp files
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "langs")
(load "my-dired")
(load "keybindings")
(load "mc")

;;; theme package
(use-package gruber-darker-theme
  :ensure t
  :config
  (load-theme 'gruber-darker t))

;;; visuals
(set-face-attribute 'default nil :height 200)
(cua-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-subword-mode 1)
(global-font-lock-mode 1)
(electric-indent-mode -1)

(setq ring-bell-function 'ignore)

;;; startup
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      initial-buffer-choice "~/programming")

;;; colored compilation
(require 'ansi-color)
(defun my-compilation-colorize ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook #'my-compilation-colorize)

;;; line numbers
(line-number-mode t)
(column-number-mode t)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;;; change backup and autosaves dir
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/autosaves/" t)))

;;; abbrev config
(setq-default abbrev-mode nil)
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (abbrev-mode -1)))

;; whitespace mode, doesnt show newlines
(global-whitespace-mode 1)
(setq whitespace-style
      '(face tabs spaces tab-mark space-mark))

;; company
(use-package company
  :ensure t
  :init
  (global-company-mode 1)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-frontends '(company-preview-if-just-one-frontend))  
  :bind
  ("M-/" . company-complete))

;; for minibuffer things
(use-package vertico ;; vertical minibuffer
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia ;; minibuffer search suggestions
  :ensure t
  :init
  (marginalia-mode))

(use-package orderless ;; smart search
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "PATH"))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; yas config
(require 'yasnippet)
(yas-global-mode 1)
;; for adding a new snippet: M-x yas-new-snippet or C-c C-n
;; and enter a name and a keyboard then after the comments type the code
;; Then press C-c C-c, if it asks the mode, you just say c-mode or smth that in which mode do you want to use
;; that snippet and then save the file

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd" default))
 '(package-selected-packages
   '(company move-text expand-region gruber-darker-theme eglot exec-path-from-shell orderless marginalia vertico lorem-ipsum flycheck yasnippet-snippets yasnippet multiple-cursors intel-hex-mode rust-mode haskell-mode markdown-mode cmake-mode magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
