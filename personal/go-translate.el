(require 'google-translate)
;(require 'google-translate-default-ui)
(require 'google-translate-smooth-ui)
; (global-set-key "\C-ct" 'google-translate-smooth-translate)
(global-set-key "\C-c \C-t 5" 'google-translate-at-point-reverse)
(global-set-key "\C-c \C-t 6" 'google-translate-at-point)
(global-set-key "\C-c \C-t 7" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)



   (use-package google-translate
       :demand t
       :init
            (require 'google-translate)

       :functions (my-google-translate-at-point google-translate--search-tkk)
       :custom
       (google-translate-backend-method 'curl)
       :config
       (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
       (defun my-google-translate-at-point()
         "reverse translate if prefix"
         (interactive)
         (if current-prefix-arg
             (google-translate-at-point)
           (google-translate-at-point-reverse)))
       :bind
       ("C-t". my-google-translate-at-point))



;; (require 'go-translate)
;; (setq go-translate-local-language "en")
;; (setq go-translate-target-language "it")


;; (setq go-translate-buffer-follow-p t)       ; focus the result window
;; (setq go-translate-buffer-source-fold-p t)  ; fold the source text in the result window
;; ; (setq go-translate-buffer-window-config ..) ; config the result window as your wish



;(global-set-key "\C-ct" 'go-translate)
;(global-set-key "\C-cT" 'go-translate-popup)
