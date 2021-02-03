;; (defun org-export-latex-no-toc (depth)
;;   (when depth
;;     (format "%% Org-mode is exporting headings to %s levels.\n"
;;             depth)))
;; (setq org-export-latex-format-toc-function 'org-export-latex-no-toc)


(use-package org2blog
  :ensure t)

(setq org2blog/wp-blog-alist
      '(("nottolino"
         :url "https://nottolino.exedre.org/xmlrpc.php"
         :username "admin")))

(require 'ox-odt)

(defun org-uni-export-no-headings ()
  (interactive)
  (save-excursion
    (point-min)
    (flush-lines "^*+ ")
    (flush-lines ":TOC:")
    (org-odt-export-to-odt)
    (kill-buffer)))


(defun org-global-props (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))


(defun org-global-prop-value (key)
  "Get global org property KEY of current buffer."
  (org-element-property :value (car (org-global-props key))))

; (require 'ox-bibtex)


(defun markdown-convert-buffer-to-org ()
  "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           (format "pandoc -f markdown -t org -o %s"
                                   (concat (file-name-sans-extension (buffer-file-name)) ".org"))))



(require 'ox-publish)    ;C-x C-e to load org-publish
(setq org-publish-project-alist
      '(
("org-source"                         ;org-source files to be transformed into html files
 :base-directory "~/Documents/developer/editors/emacs/publish_html/org/"
 :base-extension "org"
 :publishing-directory "~/Documents/developer/editors/emacs/publish_html/public_html/"
 :recursive t
 :publishing-function org-html-publish-to-html
 :table-of-contents nil               ;nil fail, generated TOC
 :style "<link rel=\"stylesheet\"
      href=\"../other/mystyle.css\"
      type=\"text/css\"/>"            ;style fail, no style link was generated

;properties listed in left column of https://orgmode.org/manual/Publishing-options.html
 :with-toc nil                        ;nil pass, did not generate TOC
 :headline-levels 4
 :section-numbers nil                 ;nil pass, generated headings without sections numbers
 :html-head-include-default-style nil ;nil pass, did not generate 160-lines of commented style
 :org-html-postamble nil              ;nil fail, postamble was generated
 )

("org-static"
 :base-directory "~/Documents/developer/editors/emacs/publish_html/org/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/Documents/developer/editors/emacs/publish_html/public_html/"
 :recursive t
 :publishing-function org-publish-attachment
 )

("org" :components ("org-source" "org-static"))
      ))                 ;C-x C-e and repeat after every change to org-publish-project-alist
;to publish: SPC u SPC SPC org-publish-project


(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (shell . t)
   (scheme . t)
   ))



(setq org-confirm-babel-evaluate nil)


(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))


(require 'calendar)
(defun insdate-insert-current-date (&optional omit-day-of-week-p)
  "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
  (interactive "P*")
  (insert (concat " consultato il "
                  (calendar-date-string
                   (calendar-current-date) nil
		   omit-day-of-week-p))))

; (global-set-key "\C-c\C-n" `insdate-insert-current-date)

(define-key org-mode-map (kbd "C-c n") `insdate-insert-current-date)


; natural size of images
; (setq org-latex-image-default-width "")
; (setq org-latex-image-default-height "")
; (setq org-latex-image-default-height "")

(setq org-export-allow-bind-keywords t)


(defun kdm/html2org-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (setq cmd "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t org")
  (kill-new (shell-command-to-string cmd))
  (yank))


(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)
