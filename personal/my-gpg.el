;; Don't bring up key recipient dialogue.
(require 'epa-file)
(setq epa-file-select-keys nil)
(setq epa-file-encrypt-to '("emmanuele.somma@logioshermes.org"))

(epa-file-enable)

;; Adapted from https://stackoverflow.com/a/15854362/6085242.
(defun kill-gpg-buffers ()
  "Kill GPG buffers."
  (interactive)
  (let ((buffers-killed 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (string-match ".*\.gpg$" (buffer-name buffer))
          (message "Auto killing .gpg buffer '%s'" (buffer-name buffer))
          (when (buffer-modified-p buffer)
            (save-buffer))
          (kill-buffer buffer)
          (setq buffers-killed (+ buffers-killed 1)))))
    (unless (zerop buffers-killed)
      ;; Kill gpg-agent.
      (shell-command "gpgconf --kill gpg-agent")
      (message "%s .gpg buffers have been autosaved and killed" buffers-killed))))

(run-with-idle-timer 3600 t 'kill-gpg-buffers)
