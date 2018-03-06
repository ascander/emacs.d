(setq-default dired-dwim-target t)
(setq-default dired-listing-switches "-alh")

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls
                  dired-listing-switches (concat dired-listing-switches " --group-directories-first -v"))))

(after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode)
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%"))))

(when (maybe-require-package 'diff-hl)
  (after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(when (maybe-require-package 'stripe-buffer)
  (after-load 'dired
    (add-hook 'dired-mode-hook 'stripe-buffer-mode)))

(provide 'init-dired)
