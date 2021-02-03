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


(fset 'my/rewrap-para
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([5 backspace 44 32 4 escape 108] 0 "%d")) arg)))

(global-set-key (kbd "M-ò") 'my/rewrap-para)

(global-set-key (kbd "M-<up>") 'select-next-window)
(global-set-key (kbd "<XF86Forward>") 'select-next-window)

(global-set-key (kbd "M-<down>")  'select-previous-window)
(global-set-key (kbd "<XF86Back>")  'select-previous-window)


(global-set-key (kbd "C-ù") 'select-last-nbor-window)
(global-set-key (kbd "M-ù") 'select-last-nbor-window)
(global-set-key (kbd "M-\\") 'ns-next-frame)
(global-set-key (kbd "s-ò") (lambda () (interactive) (insert "@")) )
(global-set-key (kbd "s-à") (lambda () (interactive) (insert "#")) )
