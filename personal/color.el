(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


(setenv "LANG" "it_IT.UTF-8")
(setenv "LC_ALL" "it_IT.UTF-8")
(setenv "LC_CTYPE" "it_IT.UTF-8")

(setq whitespace-style '(face tabs empty trailing ))
