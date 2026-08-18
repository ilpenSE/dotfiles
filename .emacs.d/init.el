;;; Package utils
(require 'package)
(setq package-check-signature nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

(set-language-environment "English")

;;; theme package
(use-package gruber-darker-theme
  :ensure t
  :config
  (load-theme 'gruber-darker t))

;;; visuals
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 180
                    :weight 'regular
                    :width 'normal)

;; Disabled
(cua-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-whitespace-mode 0)
(electric-indent-mode -1)
(setq auto-revert-verbose nil
      ring-bell-function 'ignore
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      initial-buffer-choice "~/programming")
(setq-default abbrev-mode nil)
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (abbrev-mode -1)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enabled
(global-subword-mode 1)
(global-font-lock-mode 1)
(line-number-mode t)
(column-number-mode t)
(global-display-line-numbers-mode 1)
(setq auto-revert-use-notify t
      display-line-numbers-type 'relative
      vc-follow-symlinks t
      global-auto-revert-non-file-buffers t
      wdired-allow-to-change-permissions t)

;;; colored compilation
(use-package ansi-color
  :ensure t)
(defun my-compilation-colorize ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook #'my-compilation-colorize)

;;; change backup and autosaves dir
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/autosaves/" t)))

;; company
(use-package company
  :ensure t
  :init
  (global-company-mode 1)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-frontends '(company-preview-if-just-one-frontend))
  (company-backends '(company-dabbrev-code company-keywords company-files)))

;; vertico and marginalia is the best
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package vertico ;; vertical minibuffer
  :ensure t
  :init
  (vertico-mode))

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

(defun setup-cc--source-env (script)
  (with-temp-buffer
    (call-process
     "bash" nil t nil
     "-c"
     (format "source %s >/dev/null 2>&1 && env -0"
             (shell-quote-argument script)))

    (goto-char (point-min))
    (while (search-forward "\0" nil t)
      (let ((line (buffer-substring-no-properties
                   (save-excursion
                     (goto-char (1- (point)))
                     (search-backward "\0" nil t)
                     (if (looking-at "\0") (forward-char))
                     (point))
                   (1- (point)))))
        (when (string-match "^\\([^=]+\\)=\\(.*\\)$" line)
          (let ((var (match-string 1 line))
                (val (match-string 2 line)))
            (setenv var val)
            (when (string= var "PATH")
              (setq exec-path (parse-colon-path val)))))))))

;; Cross compilation setup
(defvar setup-cc-root (expand-file-name "~/.local"))

(defun setup-cc--prepend-path (dir)
  (setenv "PATH" (concat dir path-separator (getenv "PATH")))
  (add-to-list 'exec-path dir))

(defun setup-cc--prepend-ld (dir)
  (setenv "LD_LIBRARY_PATH"
          (concat dir path-separator (or (getenv "LD_LIBRARY_PATH") ""))))

(defun setup-cc-linux ()
  (interactive)
  (message "Native Linux toolchain enabled."))

(defun setup-cc-mingw ()
  (interactive)
  (setup-cc--prepend-path
   (expand-file-name "llvm-mingw/bin" setup-cc-root))
  (setup-cc--prepend-ld
   (expand-file-name "llvm-mingw/lib" setup-cc-root))
  (message "llvm-mingw enabled."))

(defun setup-cc-osx ()
  (interactive)
  (setup-cc--prepend-path (expand-file-name "osxcross/bin" setup-cc-root))
  (setup-cc--prepend-ld (expand-file-name   "osxcross/lib" setup-cc-root))
  (message "osxcross enabled."))

(defun setup-cc-msvc (arch)
  (interactive
   (list (completing-read "Architecture: "
                          '("x64" "x86" "arm64"))))
  (setup-cc--prepend-path (expand-file-name "msvc-wine/bin" setup-cc-root))
  (setup-cc--source-env
   (expand-file-name
    (format "msvc-wine/bin/%s/msvcenv.sh" arch)
    setup-cc-root)))

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode)
  :hook (find-file . (lambda () ; auto enable this when buffer contains ^L
                       (when (save-excursion
                               (goto-char (point-min))
                               (search-forward "\f" nil t))
                         (page-break-lines-mode 1)))))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; yas config
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
;; for adding a new snippet: M-x yas-new-snippet or C-c C-n
;; and enter a name and a keyboard then after the comments type the code
;; Then press C-c C-c, if it asks the mode, you just say c-mode or smth that in which mode do you want to use
;; that snippet and then save the file

(use-package magit
  :ensure t
  :config
  (add-hook 'server-visit-hook 'git-commit-setup)
  (add-hook 'git-commit-setup-hook 'magit-commit-diff))

;; will set custom file to <emacs directory>/custom.el (by default: ~/.emacs.d/custom.el)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

; create custom file if not exists
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))

; load custom file
(when (file-exists-p custom-file)
  (load custom-file))

;;; loading other lisp files
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "langs")
(load "my-dired")
(load "keybindings")
(load "mc")
(load "simpc-mode")
