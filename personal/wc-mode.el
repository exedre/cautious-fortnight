(require 'wc-mode)
: ;; Suggested setting
(global-set-key (kbd "s-w") 'wc-mode)

;; Override the counting of words to count commas (fields)
(setq wc-count-words-function
      (function (lambda (rstart rend)
                  (how-many "[^,\\n]+" rstart rend))))
