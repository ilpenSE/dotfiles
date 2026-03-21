;;; dired
(setq dired-listing-switches "-lah"
      dired-kill-when-opening-new-dired-buffer t
      dired-by-name-regexp nil
      dired-use-ls-dired nil
      )

;; wdired can change permissions
(setq wdired-allow-to-change-permissions t)

(defface my-dired-executable-face
  '((t (:foreground "aqua" :weight bold)))
  "Face used for executable files in Dired."
  :group 'dired-faces)

(defun my-dired-highlight-executables ()
  "Highlight ONLY the filename of executable files in Dired."
  (font-lock-add-keywords
   nil
   '(( "^..[-rwx]*x[-rwx]*x[-rwx]*x[[:space:]]+.*[[:space:]]+\\(.+\\)$" 
       (1 'my-dired-executable-face t)))))

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
(add-hook 'dired-mode-hook 'my-dired-highlight-executables)

(defun my-dired ()
  (interactive)
  (dired default-directory))
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd ".") 'my-dired-toggle-hidden)
  (define-key dired-mode-map (kbd "E") 'wdired-change-to-wdired-mode))
