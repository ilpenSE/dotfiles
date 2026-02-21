(require 'treesit nil t)

;;; tree sitter
;; M-x treesit-install-language-grammar, and then type the language name
(setq treesit-language-source-alist
      '(
	      (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")
	      (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.0" "src")
	      ))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))

;;; indent / tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq sentence-end-double-space nil)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

;; since makefiles require proper tabs, we provide it
(defun use-proper-tabs-hook ()
  (setq indent-tabs-mode t))
(add-hook 'makefile-mode-hook #'use-proper-tabs-hook)

;; normal modes indents
(setq c-basic-offset 2)
(setq c-default-style "linux")
(setq css-indent-offset 2)
(setq sgml-basic-offset 2)
(setq rust-indent-offset 2)
(setq js-indent-level 2)

;; treesitter modes indents
(setq js-ts-mode-indent-offset 2)
(setq typescript-ts-mode-indent-offset 2)
(setq tsx-ts-mode-indent-offset 2)

;; special comment style hook
(defun my-comment-style-hook ()
  "Seçili satırlar veya imleç için uygun satır başı yorum karakteri ayarla."
  (cond
   ;; C, C++, Java etc
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode)
        (eq major-mode 'rust-mode)
        (eq major-mode 'java-mode)
        (eq major-mode 'js-ts-mode)
        (eq major-mode 'typescript-ts-mode))
    (setq comment-start "// "
          comment-end ""
          comment-style 'plain))
   ;; SQL
   ((or (eq major-mode 'sql-mode))
    (setq comment-start "-- ")
    (setq comment-end "")
    (setq comment-style 'plain))
   ;; Python
   ((eq major-mode 'python-mode)
    (setq comment-start "# "
          comment-end ""
          comment-style 'plain))))

;; add hooks for comment
(add-hook 'c-mode-hook 'my-comment-style-hook)
(add-hook 'c++-mode-hook 'my-comment-style-hook)
(add-hook 'rust-mode-hook 'my-comment-style-hook)
(add-hook 'java-mode-hook 'my-comment-style-hook)
(add-hook 'js-ts-mode-hook 'my-comment-style-hook)
(add-hook 'typescript-ts-mode-hook 'my-comment-style-hook)
(add-hook 'python-mode-hook 'my-comment-style-hook)
(add-hook 'sql-mode-hook 'my-comment-style-hook)

;;; ts/js linter/autocomplete
(use-package flycheck
  :ensure t
  :hook ((js-ts-mode . flycheck-mode)
         (typescript-ts-mode . flycheck-mode)
         (tsx-ts-mode . flycheck-mode)))

; eglot
(use-package eglot
  :ensure t)

(remove-hook 'prog-mode-hook #'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'html-mode-hook 'eglot-ensure)
