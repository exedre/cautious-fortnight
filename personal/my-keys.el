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


(global-set-key (kbd "C-§") 'select-last-nbor-window)
(global-set-key (kbd "C-`") 'other-frame)

(global-set-key (kbd "M-~") 'select-last-nbor-window)
;(global-set-key (kbd "M-\\") 'ns-next-frame)
;(global-set-key (kbd "s-ò") (lambda () (interactive) (insert "@")) )
;(global-set-key (kbd "s-à") (lambda () (interactive) (insert "#")) )

(global-set-key (kbd "C-/") 'undo-tree-undo)
(global-set-key (kbd "C-<backspace>") 'sp-backward-kill-word)

(global-set-key (kbd "C-/") 'undo-tree-undo)



; https://eastmanreference.com/complete-list-of-applescript-key-codes
(global-set-key (kbd "H-<left>")
                '(lambda ()
                   (interactive)
                   (ns-do-applescript
                    "set old to (path to frontmost application as text)
		     activate application \"VLC\"
                     tell application \"System Events\" to key code 123 using {option down, command down}
		     activate application old                                                                        ")))


(global-set-key (kbd "C-<escape>")
                '(lambda ()
                   (interactive)
                   (ns-do-applescript
                    "tell application \"VLC\" to play")))

(global-set-key (kbd "C-<help>")
                '(lambda ()
                   (interactive)
                   (ns-do-applescript
                    "tell application \"VLC\" to play")))


;; ORG mode keymaps


(define-key org-mode-map (kbd "C-<return>") 'org-meta-return)
(define-key org-mode-map (kbd "C-<left>") 'left-word)
(define-key org-mode-map (kbd "C-<right>") 'right-word)

(define-key org-mode-map (kbd "C-S-<right>") 'org-metaright)
(define-key org-mode-map (kbd "C-S-<left>") 'org-metaleft)
(define-key org-mode-map (kbd "C-H-<right>") 'org-shiftmetaright)
(define-key org-mode-map (kbd "C-H-<left>") 'org-shiftmetaleft)
(define-key org-mode-map (kbd "M-<return>") 'org-insert-heading-respect-content)
