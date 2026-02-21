;; avy mode
(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-word-1)
         ("M-g f" . avy-goto-line)))
;; region copy: M-w
;; region cut: C-w
;; paste: C-y, paste from kill ring: M-y

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;; M-E to mark until end of the line
(global-set-key (kbd "M-E")
                (lambda ()
                  (interactive)
                  (set-mark (point))
                  (end-of-line)))

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

(global-set-key (kbd "<f5>") #'compile)
(global-set-key (kbd "<f6>") #'recompile)

(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
(global-set-key (kbd "C-c d") 'duplicate-line)

(global-set-key (kbd "C-z") #'undo)
(global-set-key (kbd "C-S-z") #'undo-redo)
