;; avy mode
(use-package avy
  :ensure t)
;; region copy: M-w
;; region cut: C-w
;; paste: C-y, paste from kill ring: M-y

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;; M-S-up and M-S-down to scroll up by 1 line (doesnt move cursor if cursor is visible)
(global-set-key (kbd "M-S-<down>")
                (lambda ()
                  (interactive)
                  (scroll-up-line)))
(global-set-key (kbd "M-S-<up>")
                (lambda ()
                  (interactive)
                  (scroll-down-line)))

;; untabify
(defun my-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))
(global-set-key (kbd "C-c u") #'my-untabify-buffer)

(add-hook 'simpc-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-f") #'astyle-buffer)))

(global-set-key (kbd "C-x d") 'my-dired)

(global-set-key (kbd "<f5>") #'compile)
(global-set-key (kbd "<f6>") #'recompile)

(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
(global-set-key (kbd "M--") 'duplicate-line)

(global-set-key (kbd "C-z") #'undo)
(global-set-key (kbd "C-S-z") #'undo-redo)
