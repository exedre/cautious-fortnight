(when (executable-find "fortune")
  (setq initial-scratch-message
        (with-temp-buffer
          (shell-command "fortune" t)
          (shell-command "date" t)
          (let ((comment-start ";;"))
            (comment-region (point-min) (point-max)))
          (concat (buffer-string) "\n"))))
