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

