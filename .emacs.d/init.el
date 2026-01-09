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

;;; theme package
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'catppuccin-theme)
(load-theme 'catppuccin t)

;;; visuals
(set-face-attribute 'default nil :height 250)

(cua-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-subword-mode 1)

;; Word movement’ı symbol bazlı yap
(global-set-key (kbd "M-f") #'forward-symbol)
(global-set-key (kbd "M-b") #'backward-symbol)

(setq ring-bell-function 'ignore)

;;; startup
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      initial-buffer-choice "~/programming")

;;; syntax highlighting
(global-font-lock-mode 1)

;;; line numbers
(line-number-mode t)
(column-number-mode t)
(global-display-line-numbers-mode 1)

;;; change backup and autosaves dir
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/autosaves/" t)))

;;; T or S-t now creates a file
(defun dired-touch (file)
  (interactive "FTouch file name: ")
  (write-region "" nil file t))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "T") #'dired-touch))

;;; dired
(setq dired-listing-switches "-lah"
      dired-kill-when-opening-new-dired-buffer t
      dired-by-name-regexp nil
      dired-use-ls-dired nil
      )

(put 'dired-find-alternate-file 'disabled nil)

;; for is dired shows ls hidden files?
(defvar-local my-dired-show-hidden nil)
;; toggler func
(defun my-dired-toggle-hidden ()
  (interactive)
  (setq my-dired-show-hidden (not my-dired-show-hidden))
  (setq-local dired-actual-switches
              (if my-dired-show-hidden
                  "-lah"
                "-lh"))
  (revert-buffer nil t))
(add-hook 'dired-mode-hook
          (lambda ()
            (setq my-dired-show-hidden nil)
            (setq-local dired-actual-switches dired-listing-switches)))


(defun my-dired ()
  (interactive)
  (dired default-directory))

(global-set-key (kbd "C-x d") 'my-dired)

;;; compile keybinds
;;; M-x compile: F5, M-x recompile: F6
(global-set-key (kbd "<f5>") #'compile)
(global-set-key (kbd "<f6>") #'recompile)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd ".") 'my-dired-toggle-hidden))
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

;;; C-x C-s and C-x C-j at the same time
(defun save-and-dired-jump ()
  (interactive)
  (when (buffer-file-name)
    (save-buffer)
    (dired-jump)))

(global-set-key (kbd "C-c j") #'save-and-dired-jump)

;;; c
(setq c-basic-offset 2)
(setq c-default-style "linux")

;;; indent / tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq sentence-end-double-space nil)

(electric-indent-mode 1)
(electric-pair-mode -1)

;;; move text-up/down
(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column col)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
