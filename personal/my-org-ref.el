; https://nickgeorge.net/science/org_ref_setup/
; https://www.economics.utoronto.ca/osborne/latex/BIBTEX.HTM
(require 'org-ref)

(setq reftex-default-bibliography '("~/Dropbox/Bibliografie/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Dropbox/Bibliografie/notes.org"
      org-ref-default-bibliography '("~/Dropbox/Bibliografie/references.bib")
      org-ref-pdf-directory "~/Dropbox/Bibliografie/bibtex-pdfs/")

(org-ref-define-citation-link "citealp")

(setq bibtex-completion-bibliography "~/Dropbox/Bibliografie/references.bib"
      bibtex-completion-library-path "~/Dropbox/Bibliografie/bibtex-pdfs"
      bibtex-completion-notes-path "~/Dropbox/Bibliografie/helm-bibtex-notes")

;; open pdf with system pdf viewer (works on mac)
(setq bibtex-completion-pdf-open-function
      (lambda (fpath)
        (start-process "open" "*open*" "open" fpath)))

;; alternative
;; (setq bibtex-completion-pdf-open-function 'org-open-file)


;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

;; %latex -interaction nonstopmode -output-directory %o %f
