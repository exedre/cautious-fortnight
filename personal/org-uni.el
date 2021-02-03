(defun org-uni-my-replaces ()
  (interactive)
  (save-excursion
    (point-min)
    (query-replace-regexp  "\\. " ".
")
    (point-min)
    (insert-file "testata.org-template")))


(defun my/emind-add-par ()
  (interactive)
  (if (eq 32 (following-char))
      (just-one-space -1))
  (end-of-line)
  (insert " § ")
  (just-one-space -1))


(defun my/emind-add-head ()
  (interactive)
  (end-of-line)
  (org-insert-heading)
  (just-one-space -1))

(defun my/emind-zap-head ()
  (interactive)
  (end-of-line)
  (insert " >> ")
  (just-one-space -1)
  (zap-to-char 1 ?\s)
  )

(global-unset-key (kbd "M--"))
(global-set-key (kbd "M-- .") 'my/emind-add-par)
(global-set-key (kbd "M-- -") 'my/emind-add-head)
(global-set-key (kbd "M-- M-.") 'my/emind-zap-head)
