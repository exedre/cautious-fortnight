(setq gnus-select-method
      '(nnimap "imap.gmail.com"))


(use-package gnus
  :config
  ;; accounts
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nntp "news.gwene.org")
          (nnmaildir "prv" (directory "~/.mail/prv"))
          (nnmaildir "inf" (directory "~/.mail/inf"))
          (nnmaildir "pub" (directory "~/.mail/pub"))))

  (setq nnir-method-default-engines
        '((nnmaildir . notmuch)))

  (setq gnus-parameters
        '((".*"                         ; fallback option
           (posting-style
            (gcc "nnmaildir+inf:Sent")
            (From
             (format "%s <%s>" user-full-name
                     (prot-common-auth-get-field "inf" :user)))))
          ("prv"
           (posting-style
            (gcc "nnmaildir+prv:Sent")
            (From
             (format "%s <%s>" user-full-name
                     (prot-common-auth-get-field "prv" :user)))))
          ("pub"
           (posting-style               ; Uses default name+mail
            (gcc "nnmaildir+pub:Sent")))))

  (setq gnus-gcc-mark-as-read t)
  (setq gnus-agent t)
  (setq gnus-novice-user nil)
  ;; checking sources
  (setq gnus-check-new-newsgroups 'ask-server)
  (setq gnus-read-active-file 'some)
  ;; dribble
  (setq gnus-use-dribble-file t)
  (setq gnus-always-read-dribble-file t)
  :bind ("C-c m" . gnus))

(use-package nnmail
  :config
  (setq nnmail-expiry-wait 30))

(use-package gnus-agent
  :after gnus
  :config
  (setq gnus-agent-article-alist-save-format 1)  ; uncompressed
  (setq gnus-agent-cache t)
  (setq gnus-agent-confirmation-function 'y-or-n-p)
  (setq gnus-agent-consider-all-articles nil)
  (setq gnus-agent-directory "~/News/agent/")
  (setq gnus-agent-enable-expiration 'ENABLE)
  (setq gnus-agent-expire-all nil)
  (setq gnus-agent-expire-days 30)
  (setq gnus-agent-mark-unread-after-downloaded t)
  (setq gnus-agent-queue-mail t)        ; queue if unplugged
  (setq gnus-agent-synchronize-flags nil))


(use-package gnus-art
  :after gnus
  :demand
  :config
  (setq gnus-article-browse-delete-temp 'ask)
  (setq gnus-article-over-scroll nil)
  (setq gnus-article-show-cursor t)
  (setq gnus-article-sort-functions
        '((not gnus-article-sort-by-number)
          (not gnus-article-sort-by-date)))
  (setq gnus-article-truncate-lines nil)
  (setq gnus-html-frame-width 80)
  (setq gnus-html-image-automatic-caching t)
  (setq gnus-inhibit-images t)
  (setq gnus-max-image-proportion 0.7)
  (setq gnus-treat-display-smileys nil)
  (setq gnus-article-mode-line-format "%G %S %m")
  (setq gnus-visible-headers
        '("^From:" "^To:" "^Cc:" "^Subject:" "^Newsgroups:" "^Date:"
          "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
          "^X-Mailer:"))
  (setq gnus-sorted-header-list gnus-visible-headers)
  :hook (gnus-article-mode-hook . (lambda ()
                                    (setq-local fill-column 80)))
  :bind (:map gnus-article-mode-map
              ("i" . gnus-article-show-images)
              ("s" . gnus-mime-save-part)
              ("o" . gnus-mime-copy-part)))

(use-package gnus-async
  :after gnus
  :config
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15))


(use-package gnus-group
  :after gnus
  :demand
  :config
  (setq gnus-level-subscribed 6)
  (setq gnus-level-unsubscribed 7)
  (setq gnus-level-zombie 8)
  (setq gnus-activate-level 2)
  (setq gnus-list-groups-with-ticked-articles nil)
  (setq gnus-group-sort-function
        '((gnus-group-sort-by-unread)
          (gnus-group-sort-by-alphabet)
          (gnus-group-sort-by-rank)))
  (setq gnus-group-line-format "%M%p%P%5y:%B%(%g%)\n")
  (setq gnus-group-mode-line-format "%%b")
  :hook ((gnus-group-mode-hook . hl-line-mode)
         (gnus-select-group-hook . gnus-group-set-timestamp))
  :bind (:map gnus-group-mode-map
              ("M-n" . gnus-topic-goto-next-topic)
              ("M-p" . gnus-topic-goto-previous-topic)))

(use-package gnus-topic
  :after (gnus gnus-group)
  :config
  (setq gnus-topic-display-empty-topics nil)
  :hook (gnus-group-mode-hook . gnus-topic-mode))


(use-package gnus-sum
  :after (gnus gnus-group)
  :demand
  :config
  (setq gnus-auto-select-first nil)
  (setq gnus-summary-ignore-duplicates t)
  (setq gnus-suppress-duplicates t)
  (setq gnus-save-duplicate-list t)
  (setq gnus-summary-goto-unread nil)
  (setq gnus-summary-make-false-root 'adopt)
  (setq gnus-summary-thread-gathering-function
        'gnus-gather-threads-by-subject)
  (setq gnus-summary-gather-subject-limit 'fuzzy)
  (setq gnus-thread-sort-functions
        '((not gnus-thread-sort-by-date)
          (not gnus-thread-sort-by-number)))
  (setq gnus-subthread-sort-functions
        'gnus-thread-sort-by-date)
  (setq gnus-thread-hide-subtree nil)
  (setq gnus-thread-ignore-subject nil)
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today at %R")
          ((+ (* 60 60 24) (gnus-seconds-today)) . "Yesterday, %R")
          (t . "%Y-%m-%d %R")))

  ;; When the %f specifier in `gnus-summary-line-format' matches my
  ;; name, this will use the contents of the "To:" field, prefixed by
  ;; the string I specify.  Useful when checking your "Sent" summary or
  ;; a mailing list you participate in.
  (setq gnus-ignored-from-addresses "Emmanuele Somma")
  (setq gnus-summary-to-prefix "To: ")

  (setq gnus-summary-line-format "%U%R %-18,18&user-date; %4L:%-25,25f %B%s\n")
  (setq gnus-summary-mode-line-format "[%U] %p")
  (setq gnus-sum-thread-tree-false-root "")
  (setq gnus-sum-thread-tree-indent " ")
  (setq gnus-sum-thread-tree-single-indent "")
  (setq gnus-sum-thread-tree-leaf-with-other "+-> ")
  (setq gnus-sum-thread-tree-root "")
  (setq gnus-sum-thread-tree-single-leaf "\\-> ")
  (setq gnus-sum-thread-tree-vertical "|")

  (defun prot/gnus-summary-save-parts-all ()
    "Save relevant message MIME parts to a desired directory.
EXPERIMENTAL"
    (interactive)
    (let ((dir "~/Downloads/mail-attachments"))
      (if major-mode 'gnus-summary-mode
        (progn
          (unless (file-directory-p dir)
            (make-directory dir))
          (gnus-summary-save-parts "text/.*" dir nil t))
        (user-error "Not in a `gnus' summary buffer"))))

  :hook (gnus-summary-mode-hook . hl-line-mode)
  :bind (:map gnus-summary-mode-map
              ("X x" . prot/gnus-summary-save-parts-all)
              ("<delete>" . gnus-summary-delete-article)
              ("n" . gnus-summary-next-article)
              ("p" . gnus-summary-prev-article)
              ("N" . gnus-summary-next-unread-article)
              ("P" . gnus-summary-prev-unread-article)
              ("M-n" . gnus-summary-next-thread)
              ("M-p" . gnus-summary-prev-thread)
              ("C-M-n" . gnus-summary-next-group)
              ("C-M-p" . gnus-summary-prev-group)
              ("C-M-^" . gnus-summary-refer-thread)))


(use-package gnus-srvr
  :after gnus
  :hook ((gnus-browse-mode-hook gnus-server-mode-hook) . hl-line-mode))


(use-package gnus-win
  :config
  (gnus-add-configuration
   '(article
     (horizontal 1.0
                 (vertical 40 (group 1.0))
                 (vertical 1.0
                           (summary 0.16 point)
                           (article 1.0)))))

  (gnus-add-configuration
   '(summary
     (horizontal 1.0
                 (vertical 40 (group 1.0))
                 (vertical 1.0 (summary 1.0 point))))))


(use-package gnus-dired
  :after (gnus dired)
  :hook (dired-mode-hook . gnus-dired-mode))
