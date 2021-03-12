(defun torrent-view (&optional file)
  "View the current buffer or FILE as a bencoded torrent metainfo file."
  (interactive "fTorrent file: ")
  (with-temp-buffer
    (insert-file-literally file)
    (goto-char (point-min))
    (let* ((data (bdecode-buffer))
	   (info (cdr (assoc "info" data)))
	   (files (cdr (assoc "files" info))))
      (set-buffer (get-buffer-create file))
      (erase-buffer)
      (insert "        Size  Filename")
      (newline)
      (dolist (file files)
	(let ((path (cdr (assoc "path" file)))
	      (length (cdr (assoc "length" file))))
	  (insert (format "%12d  " length)
		  (mapconcat 'identity path path-separator))
	  (newline)))
      (goto-char (point-min))
      (view-buffer (current-buffer)))))