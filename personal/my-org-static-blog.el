(setq org-static-blog-publish-title "Nottolino")
(setq org-static-blog-publish-url "https://nottolino.exedre.org/")
(setq org-static-blog-publish-directory "~/Dropbox/VITA/projects/blog/")
(setq org-static-blog-posts-directory "~/Dropbox/VITA/projects/blog/posts/")
(setq org-static-blog-drafts-directory "~/Dropbox/VITA/projects/blog/drafts/")
(setq org-static-blog-enable-tags t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

;; This header is inserted into the <head> section of every page:
;;   (you will need to create the style sheet at
;;    ~/projects/blog/static/style.css
;;    and the favicon at
;;    ~/projects/blog/static/favicon.ico)
(setq org-static-blog-page-header
"<meta name=\"author\" content=\"Emmanuele Somma\">
<meta name=\"referrer\" content=\"no-referrer\">
<link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">")

;; This preamble is inserted at the beginning of the <body> of every page:
;;   This particular HTML creates a <div> with a simple linked headline
(setq org-static-blog-page-preamble
"<div class=\"header\">
  <a href=\"https://nottolino.exedre.org\">Nottolino</a>
</div>")

;; This postamble is inserted at the end of the <body> of every page:
;;   This particular HTML creates a <div> with a link to the archive page
;;   and a licensing stub.
(setq org-static-blog-page-postamble
"<div id=\"archive\">
  <a href=\"https://nottolino.exedre.org/archive.html\">Other posts</a>
</div>
<center><a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a><br /><span xmlns:dct=\"https://purl.org/dc/terms/\" href=\"https://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">exedre.org</span> by <a xmlns:cc=\"https://creativecommons.org/ns#\" href=\"https://exedre.org\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">Emmanuele Somma</a> is licensed under a <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.</center>")


(add-to-list 'auto-mode-alist
             (cons
              (concat org-static-blog-posts-directory ".*\\.org\\'")
              'org-static-blog-mode))
