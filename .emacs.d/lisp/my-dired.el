;;; dired
(setq dired-listing-switches "-lah")
(setq dired-use-ls-dired nil)
(setq dired-kill-when-opening-new-dired-buffer t)

(add-hook 'dired-mode-hook
          (lambda ()
            (setq-local dired-actual-switches dired-listing-switches)
            (setq-local whitespace-mode nil)))

(defun default-dired ()
  (interactive)
  (dired default-directory))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "E") 'wdired-change-to-wdired-mode))
