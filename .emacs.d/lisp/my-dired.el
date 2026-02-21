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
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd ".") 'my-dired-toggle-hidden)
  (define-key dired-mode-map (kbd "E") 'wdired-change-to-wdired-mode))

(global-set-key (kbd "C-x d") 'my-dired)
