(require 'org-roam)

(setq org-roam-directory "~/Dropbox/org-roam")

(bind-key "C-h /" 'org-roam-find-file)
(bind-key [f8] 'org-roam-find-file)
(bind-key "C-h 0" 'org-roam-capture)
(bind-key "C-h i" 'org-roam-insert)
(bind-key "C-h d" 'org-roam-find-directory)
(bind-key "C-h u" 'org-roam-db-build-cache)
(bind-key "C-h t" 'org-roam-dailies-capture-today)
(bind-key "C-h j" 'org-roam-dailies-find-date)
(bind-key "C-h y" 'org-roam-dailies-find-yesterday)
(bind-key "C-h x" 'org-roam-switch-to-buffer)
(bind-key "C-h r" 'org-roam-buffer-toggle-display)

(setq org-roam-db-update-method 'immediate)
(setq org-roam-dailies-directory "~/Dropbox/org-roam/daily")

(setq org-roam-capture-templates

      '(("d" "default" plain #'org-roam-capture--get-point :file-name "%<%Y-%m-%d>-${slug}" :head "#+title: ${title}\n#+ROAM_TAGS: %^{org-roam-tags}\n#+created: %u\n#+last_modified: %U\n%?" :unnarrowed t :jump-to-captured t)

	("l" "clipboard" plain #'org-roam-capture--get-point "%i%a" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n#+ROAM_TAGS: %? \n" :unnarrowed t :prepend t :jump-to-captured t)

	("s" "sermon" plain #'org-roam-capture--get-point (file "~/Dropbox/org-roam/diary/sermon-template.txt") :file-name "%<%Y-%m-%d>_sermon_${slug}" :head "#+title: ${Title of the Message}\n#+ROAM_TAGS: Sermon\n#+DATE: %T\n#+VENUE: ${Venue}\n#+FORMAT: ${Format}\n#+STARTUP: showall\n#+DESCRIPTION: ${Main concept of service}\n#+OPTIONS: \\n:t\n" :unnarrowed t :immediate-finish t :jump-to-captured t)))

(setq org-roam-completion-system 'ivy)
