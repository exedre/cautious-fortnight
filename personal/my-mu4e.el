(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)


;; this setting allows to re-sync and re-index mail
;; by pressing U

;; ,(make-mu4e-context
;;   :name "fiona"
;;   :match-func
;;   (lambda (_)
;;     (string-equal "fiona" (mu4e-context-name mu4e~context-current)))
;;   :enter-func (lambda () (mu4e-message "Entering Private context"))
;;   :leave-func (lambda () (progn (mu4e-message "Leaving Private context") (mu4e-clear-caches)))
;;   :vars '((mu4e-maildir . "~/Maildir/google-fiona")
;;           (mu4e-mu-home . "~/.mu/fiona")
;;           (mu4e-get-mail-command . "~/bin/get-my-mail.sh -a GFiona")
;;           (user-mail-address . "fiona-robusta@gmail.com")
;;           (mu4e-sent-folder . "/[GMail].Sent")
;;           (mu4e-refile-folder . "/[GMail].Archive")
;;           (mu4e-drafts-folder . "/[GMail].Drafts")
;;           (mu4e-trash-folder . "/[GMail].Trash")
;;           ))
;; ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
;; ;; guess or ask the correct context, e.g.

;; start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
;; (setq mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
;; default is to ask
;; (setq mu4e-compose-context-policy nil)


(use-package mu4e
  ;; This is an exception because I install it from the Arch Linux
  ;; package archives (depends on non-Emacs code)
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :config
  (setq
   mue4e-headers-skip-duplicates  t
   mu4e-view-show-images t
   mu4e-compose-format-flowed nil
   mu4e-date-format "%y/%m/%d"
   mu4e-headers-date-format "%Y/%m/%d"
   mu4e-change-filenames-when-moving t
;   mu4e-attachments-dir "~/Downloads"

   mu4e-maildir       "~/Maildir"   ;; top-level Maildir
   ;; note that these folders below must start with /
   ;; the paths are relative to maildir root
   mu4e-refile-folder "/Archive"
   ;mu4e-sent-folder   "/Sent"
   ;mu4e-drafts-folder "/Drafts"
   ;mu4e-trash-folder  "/Trash"
   )
  ;; (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-use-fancy-chars nil)
  (setq mu4e-headers-advance-after-mark t)
  (setq mu4e-headers-auto-update t)
  ;(setq mu4e-headers-date-format "%F")
  ;(setq mu4e-headers-time-format "%R")
  ;(setq mu4e-headers-long-date-format "%F, %R")

  (setq mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 6)
          (:mailing-list . 10)
          (:from . 22)
          (:subject)))

;  (setq mu4e-get-mail-command "true")
  (setq mu4e-hide-index-messages t)
  (setq mu4e-update-interval (* 60 5))
  (setq mu4e-completing-read-function 'completing-read)
  (setq mu4e-compose-signature "Emmanuele Somma\nexedre.org\n")
  (setq mu4e-compose-signature-auto-include t)
  ; (setq mu4e-maildir "~/.mail")
  (setq mu4e-attachment-dir "~/Downloads/mail-attachments")
  (setq mu4e-sent-messages-behavior 'sent)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask)
  (setq mu4e-index-lazy-check t)
  (setq mu4e-change-filenames-when-moving t) ; better for `mbsync'?
  (setq mu4e-modeline-max-width 30)
  (setq mu4e-display-update-status-in-modeline t)
  ;(setq mu4e-view-show-images nil)
  (setq mu4e-decryption-policy 'ask)
  (setq mu4e-headers-results-limit -1)
  (setq mu4e-get-mail-command  "~/bin/get-my-mail.sh")

;(fset 'my-move-to-trash "mTrash")

(setq
 mu4e-contexts
 `( ,(make-mu4e-context
      :name "viminale5"
      :enter-func (lambda () (mu4e-message "Entering Private context"))
      :leave-func (lambda () (progn (mu4e-message "Leaving Private context") (mu4e-clear-caches)))
      :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "viminale5@gmail.com")))
      :vars '((mu4e-maildir . "~/Maildir/google-viminale5")
              ;(mu4e-mu-home . "~/.mu/viminale5")
              (mu4e-get-mail-command . "~/bin/get-my-mail.sh -a GViminale5")
              (user-mail-address . "viminale5@gmail.com")
              ;(mu4e-sent-folder . "/sent")
              ;(mu4e-refile-folder . "/archive")
              ;(mu4e-drafts-folder . "/drafts")
              (mu4e-trash-folder . "/google-viminale5/[GMail].Trash")
              ))
    ,(make-mu4e-context
      :name "wexedre"
      :enter-func (lambda () (mu4e-message "Entering Private context"))
      :leave-func (lambda () (progn (mu4e-message "Leaving Private context") (mu4e-clear-caches)))
      :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "exedre@winstonsmith.info")))
      :vars '((mu4e-maildir . "~/Maildir/winstonsmith-exedre/")
                                        ; (mu4e-mu-home . "~/.mu/b91744")
              (mu4e-get-mail-command . "~/bin/get-my-mail.sh -a Wexedre")
              (user-mail-address . "exedre@winstonsmith.info")
                                        ;(mu4e-sent-folder . "/sent")
              (mu4e-refile-folder . "/archive")
                                        ;(mu4e-drafts-folder . "/drafts")
              (mu4e-trash-folder . "/winstonsmith-exedre/Trash")
              ))
    ,(make-mu4e-context
      :name "fiona"
      :enter-func (lambda () (mu4e-message "Entering Private context"))
      :leave-func (lambda () (progn (mu4e-message "Leaving Private context") (mu4e-clear-caches)))
      :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "b97144@gmail.com")))
      :vars '((mu4e-maildir . "~/Maildir/google-fiona.robusta/")
                                        ; (mu4e-mu-home . "~/.mu/b91744")
              (mu4e-get-mail-command . "~/bin/get-my-mail.sh -a GFiona")
              (user-mail-address . "fiona.robusta@gmail.com")
                                        ;(mu4e-sent-folder . "/sent")
              (mu4e-refile-folder . "/archive")
                                        ;(mu4e-drafts-folder . "/drafts")
              (mu4e-trash-folder . "/google-fiona.robusta/[GMail].Trash")
              ))
    ,(make-mu4e-context
      :name "b97144"
      :enter-func (lambda () (mu4e-message "Entering Private context"))
      :leave-func (lambda () (progn (mu4e-message "Leaving Private context") (mu4e-clear-caches)))
      :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "b97144@gmail.com")))
      :vars '((mu4e-maildir . "~/Maildir/google-b97144")
                                        ; (mu4e-mu-home . "~/.mu/b91744")
              (mu4e-get-mail-command . "~/bin/get-my-mail.sh -a GB97144")
              (user-mail-address . "b97144@gmail.com")
                                        ;(mu4e-sent-folder . "/sent")
              (mu4e-refile-folder . "/archive")
                                        ;(mu4e-drafts-folder . "/drafts")
              (mu4e-trash-folder . "/google-b97144/[GMail].Cestino")
              ))
    ))


  ;; (setq mu4e-contexts
  ;;       `(,(make-mu4e-context
  ;;           :name "vrp" ; Is there no way to specify a key for switching?
  ;;           :enter-func (lambda () (mu4e-message "Entering PRV"))
  ;;           :leave-func (lambda () (mu4e-message "Leaving PRV"))
  ;;           :match-func (lambda (msg)
  ;;                         (when msg
  ;;                           (mu4e-message-contact-field-matches
  ;;                            msg :to (prot-common-auth-get-field "prv" :user))))
  ;;           :vars `((user-mail-address . ,(prot-common-auth-get-field "prv" :user))))
  ;;         ,(make-mu4e-context
  ;;           :name "inf"
  ;;           :match-func (lambda (msg)
  ;;                         (when msg
  ;;                           (mu4e-message-contact-field-matches
  ;;                            msg :to (prot-common-auth-get-field "inf" :user))))
  ;;           :vars `((user-mail-address . ,(prot-common-auth-get-field "inf" :user))))
  ;;         ,(make-mu4e-context
  ;;           :name "pub"
  ;;           :match-func (lambda (msg)
  ;;                         (when msg
  ;;                           (mu4e-message-contact-field-matches
  ;;                            msg :to (prot-common-auth-get-field "pub" :user))))
  ;;           :vars `((user-mail-address . ,(prot-common-auth-get-field "pub" :user))))))

  (setq mu4e-bookmarks
        '((:name "Unread messages" :query "g:unread AND NOT g:trashed" :key ?u)
          (:name "Today's messages" :query "d:today..now" :key ?t)
          (:name "Last 7 days" :query "d:7d..now" :key ?w)
          (:name "PRV Unread"
                 :query "maildir:\"/google-exedre@gmail.com/[GMail].All Mail\" AND g:unread AND NOT g:trashed"
                 :key ?v)
          (:name "PRV Inbox"
                 :query `,(format "to:%s"
                                  (prot-common-auth-get-field "prv" :user))
                 :key ?V)
          (:name "INF Unread"
                 :query `,(format "to:%s %s"
                                  (prot-common-auth-get-field "inf" :user)
                                  "AND g:unread AND NOT g:trashed")
                 :key ?i)
          (:name "INF Inbox"
                 :query `,(format "to:%s"
                                  (prot-common-auth-get-field "inf" :user))
                 :key ?I)
          (:name "PUB Unread"
                 :query `,(format "to:%s %s"
                                  (prot-common-auth-get-field "pub" :user)
                                  "AND g:unread AND NOT g:trashed")
                 :key ?p)
          (:name "PUB Inbox"
                 :query `,(format "to:%s"
                                  (prot-common-auth-get-field "pub" :user))
                 :key ?P)))

  :bind (("C-c M" . mu4e)
         :map mu4e-headers-mode-map
         ("!" .  (lambda (&optional arg)
                   (interactive "P")
                   (if arg
                       (mu4e-headers-mark-for-unflag)
                     (mu4e-headers-mark-for-flag))))
         ;("d" . my-move-to-trash)
         ("r" . mu4e-headers-mark-for-read)
         ("u" . mu4e-headers-mark-for-unread)
         ("M-u" . mu4e-headers-mark-for-unmark)
         ("C-M-u" . mu4e-mark-unmark-all)))

; (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
;(define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)


;; Function to interactively prompt for a destination (minimally changed from mu4e~mark-get-move-target() )
(defun my~mark-get-copy-target ()
  "Ask for a copy target, and propose to create it if it does not exist."
  (interactive)
  ;;  (mu4e-message-at-point) ;; raises error if there is none
  (let* ((target (mu4e-ask-maildir "Copy message to: "))
         (target (if (string= (substring target 0 1) "/")
                     target
                   (concat "/" target)))
         (fulltarget (concat mu4e-maildir target)))
    (when (or (file-directory-p fulltarget)
              (and (yes-or-no-p
                    (format "%s does not exist.  Create now?" fulltarget))
                   (mu4e~proc-mkdir fulltarget)))
      target)))

;; Function to duplicate a message given by its docid, msg, and that will be copied to target when the mark is executed.
(defun copy-message-to-target(docid msg target)
  (let (
        (new_msg_path nil) ;; local variable
        (msg_flags (mu4e-message-field msg :flags))
        )
    ;; 1. target is already determined interactively when executing the mark (:ask-target)

    ;; 2. Determine the path for the new file: we use mu4e~draft-message-filename-construct from
    ;; mu4e-draft.el to create a new random filename, and append the original's msg_flags
    (setq new_msg_path (format "%s/%s/cur/%s" mu4e-maildir target (mu4e~draft-message-filename-construct
    (mu4e-flags-to-string msg_flags))))

    ;; 3. Copy the message using file system call (copy-file) to new_msg_path:
    ;; (See e.g. mu4e-draft.el > mu4e-draft-open > resend)
    (copy-file (mu4e-message-field msg :path) new_msg_path)

    ;; 4. Add the information to the database (may need to update current search query with 'g' if duplicating to current box. Try also 'V' to toggle the display of duplicates)
    (mu4e~proc-add new_msg_path (mu4e~mark-check-target target))
    )
  )

;; Set this up for marking: see https://www.djcbsoftware.nl/code/mu/mu4e/Adding-a-new-kind-of-mark.html
(add-to-list 'mu4e-marks
    '(copy
     :char ("c" . "c")
     :prompt "copy"
     :ask-target  my~mark-get-copy-target
     :action copy-message-to-target))
(mu4e~headers-defun-mark-for copy)
(define-key mu4e-headers-mode-map (kbd "c") 'mu4e-headers-mark-for-copy)
