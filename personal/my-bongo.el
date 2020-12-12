(use-package bongo
  :ensure t :defer t
  :init (progn
          (setq bongo-default-directory "/Users/exedre/Library/Mobile Documents/com~apple~CloudDocs/STORE/0-FILOSOFIA/0-RISORSE/2019-L5-IIa-Filosofia teoretica/0-AUDIO"
				bongo-confirm-flush-playlist nil
				bongo-insert-whole-directory-trees nil)))
(use-package bongo
  :ensure t :defer t
  :init (progn
		  (defun my/bongo-play-first ()
			(interactive)
			(with-current-buffer (bongo-buffer)
			  (save-excursion
				(beginning-of-buffer)
				(next-line 14)
				(bongo-play))))

		  (defun my/bongo-play-last ()
			(interactive)
			(with-current-buffer (bongo-buffer)
			  (save-excursion
				(end-of-buffer)
				(previous-line)
				(bongo-play))))

		  (defun my/bongo-kill-current ()
			(interactive)
			(with-current-buffer (bongo-buffer)
			  (save-excursion
				(bongo-stop)
				(bongo-recenter)
				(bongo-kill)
				(if (= 0 (my/difference-line-point-end-buffer))
					(previous-line))
				(bongo-play))))

		  (setq bongo-default-directory "~/Music/"
				bongo-confirm-flush-playlist nil
				bongo-insert-whole-directory-trees nil))
  :config(progn
		  (bind-key "." 'my/youtube-dl bongo-mode-map)
		  (bind-key "C-c C-c" 'my/bongo-play-first bongo-mode-map)
		  (bind-key "C-c C-t" 'my/bongo-play-last bongo-mode-map)))
(defhydra hydra-bongo (:color blue :hint nil)
  "
       ^_c_^             ^_,_^         _p_: pause/resume   _i_: insert
       ^^↑^^             ^^↑^^         _s_ :start/stop     _k_: kill
   _h_ ←   → _n_     _a_ ←   → _e_     _l_: library        _u_: youtube
       ^^↓^^             ^^↓^^         _r_: random
       ^_t_^             ^_o_^
"
  ("." bongo-playlist :color red)
  ("h" bongo-play-previous :color pink)
  ("c" my/bongo-play-first  :color pink)
  ("n" bongo-play-next :color pink)
  ("t" my/bongo-play-last :color pink)
  ("," bongo-seek-backward-60 :color pink)
  ("a" bongo-seek-backward-10 :color pink)
  ("e" bongo-seek-forward-60 :color pink)
  ("o" bongo-seek-forward-10 :color pink)
  ("p" bongo-pause/resume :color red)
  ("s" bongo-start/stop :color pink)
  ("l" bongo :color red)
  ("r" bongo-play-random :color red)
  ("i" bongo-insert-file :color red)
  ("k" my/bongo-kill-current :color pink)
  ("u" my/youtube-dl)
  ("g" my/kill-buffer :color red)
  ("q" nil :color blue))


;; (global-set-key (kbd "<f7>") 'my/bongo-play-first)
;; (global-set-key (kbd "<f9>") 'bongo-pause/resume)
;; (global-set-key (kbd "<f8>") 'bongo-seek-backward-10)
;; (global-set-key (kbd "<f10>") 'bongo-seek-forward-10)
;; (global-set-key (kbd "<f11>") 'bongo-seek-to)
;; (global-set-key (kbd "<C-f7>") 'bongo-start/stop)
