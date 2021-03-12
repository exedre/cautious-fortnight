(defcustom org-emind-ref-meta-list
  '("dc_creator" "dc_contributor" "dcterms_rightsholder"
    "dc_title" "dcterms_created" "dcterms_modified")
  "List of ref-meta field for cxl file."
  )

(defcustom org-emind-default-node-entities '((">" . "&gt;")
                                             ("<" . "&lt;")
                                             (" *§ *" . "&#xa;")
                                             ("\"" . "&quot;"))
  "Alist of default graph attributes and values.
Each item in the alist should be a cons cell of the form (ATTRIB . VALUE)
where ATTRIB and VALUE are strings.
For a list of value attributes, see here: https://graphviz.gitlab.io/_pages/doc/info/attrs.html"
  :type '(alist :key-type (string :tag "Attribute") :value-type (string :tag " Value"))
  :group 'org-emind)

(defun org-emind-translate-entities (text)
  "Translate entities for node contents."
  (car
   (last
    (mapcar #'(lambda (x)
                (setq text
                      (replace-regexp-in-string
                       (car x) (cdr x) text)))
            org-emind-default-node-entities))))

(defun org-emind--decoration-string (title)

  )

(defun org-emind--ident-node (title slink elink id)
  (when title
    (if (string-match "^ *\\([[:alnum:]\.]+\\)@ *\\(.+\\) *$" title)
        (let ((label id)
              (phrase (match-string 2 title)))
          (list 'node (md5 label)
                label phrase (nth 1 slink) (nth 1 elink)))
      (list 'node (md5 id) title title (nth 1 slink)(car elink)))))

(defun org-emind--ident-link (title pos id)
  (when title
    (if (string-match "^ *\\(\\([[:alnum:]\.]+\\)@ *\\)?\\(.*?\\) *\\(\+ *\\(.+\\)\\)? *$" title)
        (let* ((label id)
               (phrase (match-string 3  title))
               (conn (match-string 5  title))
               (connections (when conn
                              (mapcar
                               'md5
                               (split-string
                                (replace-regexp-in-string
                                 "^ +" " "
                                 (replace-regexp-in-string
                                  " +$" ""
                                  conn)) "[ ,]")))))
          (list 'linking-phrase
                (md5 label)
                label
                phrase nil connections))
      (list 'linking-phrase
            (md5 id) title title nil nil))))

(defun org-emind-get-node (title id)
  (if (string-match
       "^\\( *\\(.*\\) *||\\)? *\\(.*?\\)\\( *>> *\\(.*?\\)\\)? *$"
       title)
      (let* ((slink- (match-string 2 title) )
             (node- (match-string 3 title))
             (elink- (match-string 5 title) )
             (slink (org-emind--ident-link slink- 'start (concat id ".S")))
             (elink (org-emind--ident-link elink- 'end (concat id ".E")))
             (node (org-emind--ident-node node-
                                           slink
                                           elink (concat id ".N"))))
        (list slink node elink))))

;; < inbound link > || < node > >> < outbound link >
;; outbound link is <label>@ <connections> |> <phrase>
;;  inbound link is <label>@ <phrase> |> <connections>
;;   connections is <label>(,<label>)*
;;          node is <label>@ <phrase>
;;
;;
;; (org-emind-get-node "A")
;; (org-emind-get-node "A1@A")
;; (org-emind-get-node " A || A2@Bq")
;; (org-emind-get-node "AC@ Avant + A1 A4 || 88j@ djnewkoedjow >> kk@ AC + ACX")
;; (org-emind-get-node "A@ Avant + A A4 || 88j@ djnewkoedjow >> D@A1 + A")
;; (org-emind-get-node "A@ Avant + A B || B@ djnewkoedjow >>+ A B")
;; (org-emind-get-node "B@ This is a node named B that is linked to >> + B   ")

(defun org-emind--decoration-count-cr (text pos)
  "Return position moved back of cr translation entity"
  (if (> pos 0)
      (- pos (* 4  (s-count-matches "&#xa;" text 0 (1+ pos))))
    pos))

(defun org-emind--decoration (text sep)
  "Remove bold characters and store their position.

 entity should be already translated
"
  (let ((decorations nil))
    (while
        (string-match
         (concat sep "\\([^" sep  "]+?\\)\\*" sep) text)
      (let ((from (match-beginning 0))
            (to (match-end 0)))
        (cons (list (org-emind--decoration-count-cr text from)
                    (- (org-emind--decoration-count-cr text to) 2))
              decorations)
        (setq text (replace-regexp-in-string
                    "\\(\\*\\([^\\*]*\\)\\*\\).*\\'"
                    "\\2" text nil nil 1))))
    (list text decorations)))

(defun org-emind-add-node (tree node id parents children)
  (if node
      (let*
          (
           (phrase (nth 3 node))
           (obj (list
                 (car node)   ; type
                 (nth 1 node) ; md5
                 id          ; label
                 phrase      ; phrase
                 )))
        (cons obj tree))
    tree))

(defun org-emind-add-connection (tree from terminals)
  (when  from
    (let ((terminals (if (listp terminals) terminals (list terminals))))
      (mapcar (lambda (to)
                (when to
                  (let ((obj (list
                              'connection
                              (md5 (concat from to)) ; id
                              from ; md5
                              to)))  ; children
                    (push obj tree))))
              terminals)))
  tree)

(defun org-emind--setup-tree ()
  "Create tree of all directed pairs of headlines for constructing the concept map."
  (let ((num 0)
        (nodes nil)
        (parents (make-hash-table :test 'equal)))
    (org-element-map
        (org-element-parse-buffer
         'headline)
        'headline
      (lambda (hl)
        (let* ((id (format "%d.%d" (org-element-property :begin hl) num))
               (block
                   (org-emind-get-node
                    (org-element-property :title hl)
                    id))
               (slink (car block))
               (node (nth 1 block))
               (elink (nth 2 block))
               (parent-md5
                (gethash
                 (org-element-property
                  :begin
                  (org-element-property :parent hl))
                 parents nil)))
          (puthash (org-element-property :begin hl)
                   (nth 1 node) parents)
          (setq num (1+ num))
          ; NODE and its outbound connections
          (setq nodes (org-emind-add-node
                       nodes node (concat id ".N")
                       (if slink (nth 1 slink) parent-md5)  ; left-link
                       (nth 1 elink)))                      ; right-link
          (setq nodes (org-emind-add-connection
                       nodes (nth 1 node) (nth 1 elink)))
          ; IN LINK and its connections
          (setq nodes (org-emind-add-node
                       nodes elink (concat id ".E")
                       (nth 1 node) (nth 5 elink)))
          (setq nodes (org-emind-add-connection
                       nodes (nth 1 elink) (nth 5 elink)))
          ; OUT LINK and its connections
          (setq nodes (org-emind-add-node
                       nodes slink (concat id ".S")
                       parent-md5 (list (nth 1 node))))
          (setq nodes (org-emind-add-connection
                       nodes (nth 1 slink) (nth 1 node)))
          ; Upper CONNECTION TO node if no slink or to slink
          (setq nodes (org-emind-add-connection
                       nodes parent-md5 (if slink
                                            (nth 1 slink)
                                          (nth 1 node))))

          )))
    nodes))

(defun org-emind-select (nodes types &optional ids side)
  (let ((types (if (listp types) types (list types)))
        (ids (if (listp ids) ids (list ids)))
        (side (if side side 'from))
        --acc)
    (letrec ((--walk-tree
              (lambda (--data)
                (let* ((--elem (car --data))
                       (--type (car --elem)))
                  (cond
                   ((not --data))
                   (t (when (memq --type types)
                        (if ids
                            (when (member (nth (if (eq side 'from) 1 2) --elem) ids)
                              (push --elem --acc))
                          (push --elem --acc)))
                      (funcall --walk-tree (cdr --data))))
                  ))))
      (funcall --walk-tree nodes)
      --acc)))


(defun org-emind--ids (which nodes)
  (let ((which (cond ((eq which 'from) 1)((eq which 'to) 2)(t 1) )))
    (mapcar (lambda (x) (nth which x))
            nodes)))

(defun org-emind-data (&optional id)
  (let* ((nodes (org-emind--setup))
         (lnodes (org-emind-select nodes 'node id))
         (lconns (org-emind-select nodes 'connection (org-emind--ids 'from lnodes)))
         (lphrase (org-emind-select nodes 'linking-phrase (org-emind--ids 'to lconns))))
    (list lnodes lconns lphrase)))

(defun org-emind-tree ()
  (let* ((nodes (org-emind--setup))
         (lnodes (org-emind-select nodes 'node))
         --acc)
    (letrec ((--walk-tree
              (lambda (--data)
                (let* ((--elem (car --data))
                       (--type (car --elem)))
                  (cond
                   ((not --data))
                   (t (unless (org-emind-select
                               nodes
                               'connection
                               (nth 1 --elem))
                        (push (car --data) --acc))
                      (funcall --walk-tree (cdr --data))))
                  ))))
      (funcall --walk-tree lnodes)
      (nreverse --acc))
    )
  )


(defun org-emind--unconnected-nodes (&optional nodes)
  (let* ((nodes (if nodes nodes (org-emind--setup)))
         (lnodes (org-emind-select nodes 'node))
         --acc)
    (letrec ((--walk-tree
              (lambda (--data)
                (let* ((--elem (car --data))
                       (--type (car --elem)))
                  (cond
                   ((not --data))
                   (t (unless (org-emind-select
                               nodes
                               'connection
                               (nth 1 --elem))
                        (push (car --data) --acc))
                      (funcall --walk-tree (cdr --data))))
                  ))))
      (funcall --walk-tree lnodes)
      (nreverse --acc))
    )
  )

(defun org-emind--connections-to-nodes (nodes id)
  (let* (
         (lnodes (org-emind-select nodes 'node (org-emind--ids 'from id)))
         (lconns (org-emind-select nodes 'connection (org-emind--ids 'from id) 'to))
         (lphrase (org-emind-select nodes 'linking-phrase (org-emind--ids 'from lconns))))
    (list lnodes lconns lphrase))
  )


(defun org-emind-default-filename (treenamep)
  "Return a default filename for saving the tree diagram.
If TREENAMEP is non-nil include in the filename the name of the top level header of the tree."
  (concat
   (file-name-sans-extension (buffer-name))
   "_emind"
   (if treenamep
       (concat
        "-"
	(replace-regexp-in-string
         " +" "_"
         (nth 4 (org-heading-components)))))))

;; export functions
;



(defun org-emind-write-named (name &optional debug linksp)
  "Create a concept map output based on the org tree in the current buffer, with name NAME.
To customize, see the org-emind group.
If LINKSP is non-nil include graph edges for org links."
  (let ((cxl (cxl-source
              (org-emind--setup-tree)
              (org-collect-keywords org-emind-ref-meta-list))))
    (with-temp-file (concat name ".cxl")
      (insert cxl)
      (insert "\n")))
  (message (format "File %s ready." (concat name ".cxl")))
  )

;;;###autoload
(defun org-emind-write-with-prompt nil
  "Prompt for an output FILENAME (without extension) to write your output and .dot files."
  (let ((filename (read-file-name "What is the file name you would like to save to?")))
    (org-emind-write-named filename (concat filename ".cxl")
                            (y-or-n-p "Include org links? "))))

;;;###autoload
(defun org-emind-write-index (&optional promptp)
  "Create a concept map based on all org trees in the current buffer.
The concept map will be named the same name as the current buffer.
To customize, see the org-emind group.
If called with prefix arg (or PROMPTP is non-nil), then call `org-emind-write-with-prompt'."
  (interactive "P")
  (let ((fill-v org-emind-do-fill))
    (setq org-emind-do-fill nil)
    (setq org-emind-do-index t)
    (if promptp
        (org-emind-write-with-prompt)
      (org-emind-write-named
       (org-emind-default-filename nil)))
    (setq org-emind-do-fill fill-v)
    (setq org-emind-do-fill nil)))

;;;###autoload
(defun org-emind-write (&optional promptp)
  "Create a concept map based on all org trees in the current buffer.
The concept map will be named the same name as the current buffer.
To customize, see the org-emind group.
If called with prefix arg (or PROMPTP is non-nil), then call `org-emind-write-with-prompt'."
  (interactive "P")
  (if promptp
      (org-emind-write-with-prompt)
    (org-emind-write-named
     (org-emind-default-filename nil))))

;;;###autoload
(defun org-emind-write-current-branch (&optional promptp)
  "Create a directed graph output based on just the current org tree branch.
To customize, see the org-emind group.
If called with prefix arg (or PROMPTP is non-nil), then call `org-emind-write-with-prompt'."
  (interactive "P")
  (org-narrow-to-subtree)
  (let ((filename (if promptp
                      (org-emind-write-with-prompt)
		    (org-emind-write-named
                     (org-emind-default-filename nil) t))))
    (widen)
    filename))

;;;###autoload
(defun org-emind-write-current-tree (&optional promptp)
  "Create a directed graph output based on the whole current org tree.
If called with prefix arg (or PROMPTP is non-nil), then call `org-emind-write-with-prompt'."
  (interactive "P")
  (save-restriction
    (ignore-errors (outline-up-heading 100))
    (org-emind-write-current-branch promptp)))
