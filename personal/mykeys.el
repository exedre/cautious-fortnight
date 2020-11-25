(setq win-cycle-dir 1)
(defun select-last-nbor-window ()
  "Switch to the previous window"
  (setq win-cycle-dir (* win-cycle-dir -1))
  (interactive)
  (other-window win-cycle-dir))

(defun select-next-window ()
  "Switch to the next window"
  (setq win-cycle-dir 1)
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (setq win-cycle-dir -1)
  (interactive)
  (select-window (previous-window)))



(global-set-key (kbd "M-<up>") 'select-next-window)
(global-set-key (kbd "<XF86Forward>") 'select-next-window)

(global-set-key (kbd "M-<down>")  'select-previous-window)
(global-set-key (kbd "<XF86Back>")  'select-previous-window)


(global-set-key (kbd "C-ù") 'select-last-nbor-window)
(global-set-key (kbd "M-ù") 'select-last-nbor-window)
(global-set-key (kbd "M-\\") 'ns-next-frame)
