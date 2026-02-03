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
(set-face-attribute 'default nil :height 200)

(cua-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-subword-mode 1)

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

;;; dired
(setq dired-listing-switches "-lah"
      dired-kill-when-opening-new-dired-buffer t
      dired-by-name-regexp nil
      dired-use-ls-dired nil
      )

;; wdired can change permissions
(setq wdired-allow-to-change-permissions t)

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
  (define-key dired-mode-map (kbd "E") 'wdired-change-to-wdired-mode)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

;;; abbrev config
(setq-default abbrev-mode nil)

;;; c
(setq c-basic-offset 2)
(setq c-default-style "linux")

;;; indent / tabs
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq sentence-end-double-space nil)

(defun my-spaces-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq tab-width 2))

(add-hook 'c-mode-hook 'my-spaces-mode-hook)
(add-hook 'python-mode-hook 'my-spaces-mode-hook)
(add-hook 'js-mode-hook 'my-spaces-mode-hook)

(setq python-indent-offset 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq sgml-basic-offset 2)

;; when mc opened, DO NOT ELECTRIC INDENT
(defun my-mc-disable-auto-indent ()
  (electric-indent-local-mode -1))

(defun my-mc-enable-auto-indent ()
  (electric-indent-local-mode 1))

(add-hook 'multiple-cursors-mode-enabled-hook #'my-mc-disable-auto-indent)
(add-hook 'multiple-cursors-mode-disabled-hook #'my-mc-enable-auto-indent)

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

;; special comment style hook
(defun my-comment-style-hook ()
  "Seçili satırlar veya imleç için uygun satır başı yorum karakteri ayarla."
  (cond
   ;; C, C++, Java
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode)
        (eq major-mode 'java-mode)
        (eq major-mode 'js-mode)
        (eq major-mode 'typescript-mode))
    (setq comment-start "// "
          comment-end ""
          comment-style 'plain))
   ;; Python
   ((eq major-mode 'python-mode)
    (setq comment-start "# "
          comment-end ""
          comment-style 'plain))))

;; add hooks
(add-hook 'c-mode-hook 'my-comment-style-hook)
(add-hook 'c++-mode-hook 'my-comment-style-hook)
(add-hook 'java-mode-hook 'my-comment-style-hook)
(add-hook 'js-mode-hook 'my-comment-style-hook)
(add-hook 'typescript-mode-hook 'my-comment-style-hook)
(add-hook 'python-mode-hook 'my-comment-style-hook)

(global-set-key (kbd "C-c d") 'duplicate-line)

;; mc config
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-m l" . mc/edit-lines)
         ("C-S-m a" . mc/mark-all-like-this)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-S-m s" . mc/mark-all-symbols-like-this-in-defun)
         ("C-S-m n" . mc/skip-to-next-like-this)
         ("C-S-m p" . mc/skip-to-previous-like-this))
  :config
  ;; show cursor count at modeline
  (add-to-list 'mode-line-format '(:eval (format " [MC: %d]" (mc/num-cursors))) t))

(setq mc/always-run-for-all t)

;; repeat configs, you can go next or previous on mc with > or <
;; > or n: next, < or p: previous
(use-package repeat
  :config
  (repeat-mode 1)
  (defvar mc-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd ">") 'mc/mark-next-like-this)
      (define-key map (kbd "<") 'mc/mark-previous-like-this)
      (define-key map (kbd "n") 'mc/mark-next-like-this)
      (define-key map (kbd "p") 'mc/mark-previous-like-this)
      map))
  (put 'mc/mark-next-like-this 'repeat-map 'mc-repeat-map)
  (put 'mc/mark-previous-like-this 'repeat-map 'mc-repeat-map))

;; prevent kill rings to shitting the clipboard
(defun my-delete-line-no-kill (orig-fun &rest args)
  (let ((kill-ring nil)
        (select-enable-clipboard nil)
        (interprogram-cut-function nil))
    (apply orig-fun args)))

;; make all kill funcs to NOT cut, JUST DELETE
(advice-add 'kill-line :around #'my-delete-line-no-kill)
(advice-add 'kill-whole-line :around #'my-delete-line-no-kill)
(advice-add 'backward-kill-word :around #'my-delete-line-no-kill)
(advice-add 'kill-word :around #'my-delete-line-no-kill)

(setq kill-whole-line t)

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
 '(package-selected-packages
	 '(yasnippet-snippets yasnippet multiple-cursors intel-hex-mode rust-mode haskell-mode markdown-mode cmake-mode magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
