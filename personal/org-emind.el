';;; org-emind.el --- Creates a directed graph from org-mode files
;; Author: Ted Wiles <theodore.wiles@gmail.com>
;; Keywords: orgmode, extensions, graphviz, dot
;; Package-Version: 20180826.2340
;; Package-Commit: 95347b2f9291f5c5eb6ebac8e726c03634c61de3
;; Version: 0.4
;; URL: https://github.com/theodorewiles/org-emind
;; Package-Requires: ((emacs "24") (dash "1.8.0") (org "8.2.10"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file LICENSE.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package takes an org-mode tree and converts it into a
;; file that can be read into graphviz in order to visually show the
;; tree as a directed graph.  Mail to <theodore.wiles@gmail.com> to discuss
;; features and additions.  All suggestions are more than welcome.

;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;  `org-emind-write'
;;    Create a digraph based on all org trees in the current buffer.
;;    Keybinding: M-x org-emind-write
;;  `org-emind-write-current-branch'
;;    Create a directed graph output based on just the current org tree branch.
;;    Keybinding: M-x org-emind-write-current-branch
;;  `org-emind-write-current-tree'
;;    Create a directed graph output based on the whole current org tree.
;;    Keybinding: M-x org-emind-write-current-tree
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `org-emind-unflatten-command'
;;    Shell executable command for running the UNFLATTEN command.
;;    default = "unflatten -l3"
;;  `org-emind-dot-command'
;;    Shell executable command for running the DOT command.
;;    default = "dot"
;;  `org-emind-dot-output'
;;    Format of the DOT output.  Defaults to PDF.
;;    default = "pdf"
;;  `org-emind-engine'
;;    Sets the layout engine used in your graphs.
;;    default = "dot"
;;  `org-emind-default-node-attribs'
;;    Alist of default node attributes and values.
;;    default = '(("shape" . "plaintext"))
;;  `org-emind-default-edge-attribs'
;;    Alist of default edge attributes and values.
;;    default = nil
;;  `org-emind-default-graph-attribs'
;;    Alist of default graph attributes and values.
;;    default = '(("autosize" . "false") ("size" . "9,12") ("resolution" . "100") ...))
;;  `org-emind-node-formats'
;;    Assoc list of (NAME . FN) pairs where NAME is a value for the :OMM-NODE-FMT property
;;    See also `org-emind-make-node-fn'
;;    default = nil
;;  `org-emind-edge-formats'
;;    Assoc list of (NAME . FN) pairs where NAME is a value for the :OMM-EDGE-FMT property
;;    See also `org-emind-make-edge-fn'
;;    default = nil
;;  `org-emind-edge-format-default'
;;    Default format string for graph edges, e.g. "[style=dotted]".
;;    default = ""
;;  `org-emind-reserved-colors'
;;    List of colors that will not be used for coloring tags.
;;    default = nil
;;  `org-emind-tag-colors'
;;    An alist of (TAG . COLOR) pairs for choosing colors for tags.
;;    default = nil
;;  `org-emind-include-text'
;;    A boolean indicating whether our not to include paragraph text in body of nodes.
;;    default = t
;;  `org-emind-include-images'
;;    A boolean indicating whether our not to include images in body of nodes.
;;    default = t


;; The headings of the org-mode file are treated as node text in the resulting tree.
;; Org-mode heading tags are included in the resulting tree as additional cells
;; within the node.

;; The tags are color-coded to be consistent across the tree.

;; Tree interleaving is also possible by naming multiple org-mode headings
;; with the same heading.

;; NOTE: this requires the GRAPHVIZ software.  This is installable on
;; windows using cygwin.

;; To install, add this code to your .emacs:
;; (load "org-emind.el")

;; If on linux, customize the values of `org-emind-unflatten-command'
;; and `org-emind-dot-command' to have the values corresponding to
;; the executables in your system.

;; Then, run "M-x org-emind-write" to create a graph of all trees in the current buffer,

;; You can customize the style of the graph by adding :OMM-NODE-FMT and :OMM-EDGE-FMT properties
;; to the headlines in the tree.

;; The latest version is available at:
;;
;; https://github.com/theodorewiles/org-emind
;;

;;; Code:

(require 'dash)
(require 'org)
(require 'ht)
(require 'subr-x)

(defconst org-emind-version "0.4")

(defgroup org-emind nil
  "Convert org-mode tree into a graphviz directed graph"
  :group 'org)



(defcustom org-emind-unflatten-command "unflatten -l3"
  "Shell executable command for running the UNFLATTEN command."
  :type 'string
  :group 'org-emind)

(defcustom org-emind-dot-command "dot"
  "Shell executable command for running the DOT command."
  :type 'string
  :group 'org-emind)

(defcustom org-emind-dot-output '("pdf" "png" "jpeg" "svg" "eps" "gif" "tiff")
  "List of formats for the DOT output file.
If more than one are specified then the user will be prompted to choose one.
To find a list of available formats, on the command line enter: dot -T?"
  :type '(repeat (string :tag "File type"))
  :group 'org-emind)

(defcustom org-emind-display nil
  "How the results should be displayed:
nil = don't display results
current = display results in current window
window = display results in new window
frame = display results in new frame"
  :type '(choice (const :tag "Don't display" nil)
		 (const :tag "Display in current window" current)
		 (const :tag "Display in new window" window)
		 (const :tag "Display in new frame" frame))
  :group 'org-emind)

(defcustom org-emind-engine "dot"
  "Sets the layout engine used in your graphs.
See the graphviz user manual for description of these options."
  :type '(choice
          (const :tag "Directed Graph" "dot")
          (const :tag "Undirected Spring Graph" "neato")
          (const :tag "Radial Layout" "twopi")
          (const :tag "Circular Layout" "circo")
          (const :tag "Undirected Spring Force-Directed" "fdp"))
  :group 'org-emind)

(defcustom org-emind-default-node-attribs '(("shape" . "plaintext"))
  "Alist of default node attributes and values.
Each item in the alist should be a cons cell of the form (ATTRIB . VALUE)
where ATTRIB and VALUE are strings.
For a list of value attributes, see here: https://graphviz.gitlab.io/_pages/doc/info/attrs.html"
  :type '(alist :key-type (string :tag "Attribute") :value-type (string :tag " Value"))
  :group 'org-emind)

(defcustom org-emind-default-edge-attribs nil
  "Alist of default edge attributes and values.
Each item in the alist should be a cons cell of the form (ATTRIB . VALUE)
where ATTRIB and VALUE are strings.
For a list of value attributes, see here: https://graphviz.gitlab.io/_pages/doc/info/attrs.html"
  :type '(alist :key-type (string :tag "Attribute") :value-type (string :tag " Value"))
  :group 'org-emind)

(defcustom org-emind-default-graph-attribs '(("autosize" . "false")
						("size" . "9,12")
						("resolution" . "100")
						("nodesep" . "0.75")
						("overlap" . "false")
						("spline" . "true")
						("rankdir" . "LR"))
  "Alist of default graph attributes and values.
Each item in the alist should be a cons cell of the form (ATTRIB . VALUE)
where ATTRIB and VALUE are strings.
For a list of value attributes, see here: https://graphviz.gitlab.io/_pages/doc/info/attrs.html"
  :type '(alist :key-type (string :tag "Attribute") :value-type (string :tag " Value"))
  :group 'org-emind)

(defcustom org-emind-node-formats nil
  "Assoc list of (NAME . FN) pairs where NAME is a value for the :OMM-NODE-FMT property
of a node/headline, and FN is a function which outputs a format string to be placed after the
node name (e.g. \"[label='Node1',color='red']\").
The function FN should take the following 5 arguments which can be used to construct the format:

TITLE = the label string for the node
TAGS = a list of org tags for the current node
COLOR = the contents of the OMM-COLOR property for the current node
HM = a hash map of colors
EL = an org element obtained from `org-element-map'

Note: the :OMM-NODE-FMT property is inherited by children of the node/headline where it is defined."
  :type '(alist :key-type (string :tag "              Name")
		:value-type (function :tag "Format function"))
  :group 'org-emind)

(defcustom org-emind-edge-formats nil
  "Assoc list of (NAME . FN) pairs where NAME is a value for the :OMM-EDGE-FMT property
of a node/headline, and FN is a function which outputs a format string to be placed after an
edge (e.g. \"[style=dotted]\").
The function FN should take the following 2 arguments which can be used to construct the format:

HM = a hash map of colors
EL = an org element obtained from `org-element-map'

Note: the :OMM-EDGE-FMT property affects edges leading to the node at which it is defined, and
is inherited by children of that node/headline."
  :type '(alist :key-type (string :tag "              Name")
		:value-type (function :tag "Format function"))
  :group 'org-emind)

(defcustom org-emind-edge-format-default ""
  "Default format string for graph edges, e.g. \"[style=dotted]\"."
  :type 'string
  :group 'org-emind)

(defcustom org-emind-reserved-colors nil
  "List of colors that will not be used for coloring tags.
These colors will be excluded when random tag colors are chosen by `org-emind-rgb'
so that you can use them for other things.
Each color should be in hexadecimal form, e.g: \"#e3cfbc\", where the consecutive pairs
of hexdigits indicate levels of red, green and blue respectively.
It is not necessary to include any colors with levels below 7d, as these are not used
for creating random tag colors."
  :type '(repeat string)
  :group 'org-emind)

(defcustom org-emind-tag-colors nil
  "An alist of (TAG . COLOR) pairs for choosing colors for tags.
Any tags not listed here will be colored with randomly selected colors that dont
clash with those in `org-emind-reserved-colors'.
Each color should be in hexadecimal form, e.g: \"#e3cfbc\", where the consecutive pairs
of hexdigits indicate levels of red, green and blue respectively.

Note: you can also set tag colors by altering the hashmap passed as an argument to functions
defined in `org-emind-node-formats'."
  :type '(alist :key-type (string :tag "     Tag") :value-type (string :tag "Color"))
  :group 'org-emind)

(defcustom org-emind-include-text t
  "A boolean indicating whether our not to include paragraph text in body of nodes.
   default = t"
  :type 'boolean
  :group 'org-emind
  )

(defcustom org-emind-include-images t
  "A boolean indicating whether our not to include paragraph text in body of nodes.
   default = t"
  :type 'boolean
  :group 'org-emind
  )

(defvar org-emind-node-id 0
  "Variable containing current node id")

(defvar org-emind-edge-id 0
  "Variable cointaining current edge id")

(defvar org-emind-nodes (make-hash-table :test 'equal)
  "nodes list id -> el"
  )

(defvar org-emind-edges (make-hash-table :test 'equal)
  "nodes list id node ->  id node"
  )

(defun org-emind-nodes-add (node)
  "add node to nodes hash"
  (and (not (gethash (cdr node) org-emind-nodes nil))
       (message (concat "Adding " (cdr node) " as " (number-to-string (car node))))
       (puthash (cdr node) (car node) org-emind-nodes )))

(defun swap (cell)
  (cons (cdr cell) (car cell)))

(defun org-emind-edges-add (edge-a edge-b)
  "add edge to edges hash"
  (puthash  (list (swap edge-a) (swap edge-b)) (list (swap edge-a) (swap edge-b)) org-emind-edges ))

(defun org-emind-cxl-node-id (el)
  "Returns id part of element"
  (cdr el))

(defun org-emind-cxl-node-name (el)
  "Returns name part of element."
  (nth 0 el))

(defun org-emind-get-property (prop el &optional inheritp)
  "Get property PROP from an org element EL, using inheritance if INHERITP is non-nil.
PROP can be either the property symbol (beginning with :), or the name of the property (with or without :).
If there is a column summary value for the property that has recently be calculated it will be used."
  (let* ((node el)
	 (propstr (if (stringp prop)
		      (upcase (if (string-match "^:" prop)
				  (substring prop 1)
				prop))
		    (substring (symbol-name prop) 1)))
	 (prop (if (stringp prop) (intern (concat ":" propstr)) prop))
	 (val (or (cdr (cl-find propstr (get-text-property
					 (org-element-property :begin el)
					 'org-summaries)
				:test (lambda (x y) (equal (caar y) x))))
		  (org-element-property prop el))))
    (while (and inheritp
		(not val)
		(not (eq (org-element-type node) 'org-data)))
      (setq node (org-element-property :parent node)
	    val (org-element-property prop node)))
    val))

(defun org-emind-narrow-to-heading-content (b)
  "Narrow to the region until the next headline, if applicable"
  (let* ((new-end
	  (org-element-map (org-element-parse-buffer 'object 'true)
	      'headline
	    (lambda (x)
	      (if (not
		   (= (org-element-property :begin x) b))
		  b nil))
	    nil 'true)))
    (if new-end
	(progn
	  (widen)
	  (narrow-to-region b new-end)))))




(defun org-emind-first-headline (e)
  "Figure out the first headline within element E."
  (let* ((parent (org-element-property :parent e)))
    (if parent
        (if (eq (org-element-type parent) 'headline)
            parent
          (org-emind-first-headline parent))
      nil)))


(defun org-emind-write-tags (el)
  "Tags of element"
  (let* ((ts (org-element-property :title el))
         (parent (org-element-property :parent el))
         (id (gethash ts org-emind-nodes (setq org-emind-node-id (1+ org-emind-node-id)))))
    (cons id ts)
    ))

(defun org-emind-data ()
  "Create graph  of all directed pairs of headlines for constructing the digraph."
  (clrhash org-emind-nodes)
  (clrhash org-emind-edges)
  (org-element-map
         (org-element-parse-buffer 'headline)
         'headline
       (lambda (hl)
         (let ((parent (org-element-property :parent hl)))
           (and (eq (org-element-type parent) 'headline)
                (let ((parent-tags (org-emind-write-tags parent))
                      (this-tags (org-emind-write-tags hl)))
                  (org-emind-nodes-add parent-tags)
                  (org-emind-nodes-add this-tags)
                  (org-emind-edges-add parent-tags this-tags)
                  ))))))



(defun org-emind-make-cxl ()
  "Create the dot file from DATA."
  (let* (
         (nodes (ht->alist org-emind-nodes))
         (edges (ht->alist org-emind-edges))
         (title (plist-get (org-export-get-environment) ':title))
        )
    (concat (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<cmap xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns=\"http://cmap.ihmc.us/xml/cmap/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:vcard=\"http://www.w3.org/2001/vcard-rdf/3.0#\">
    <res-meta>
        <dc:title>%s</dc:title>
         <dc:format>x-cmap/x-storable</dc:format>
 </res-meta>
<map>
\n" title)
            "<!-- nodes -->\n<concept-list>\n"
            (mapconcat #'(lambda (x) (format "<concept id=\"%s\" label=\"%s\" />"
                                             (number-to-string (org-emind-cxl-node-id x))
                                             (org-emind-cxl-node-name x)))
             nodes "\n")
            "\n</concept-list>\n"
            "<!-- EDGES -->\n<connection-list>\n"
            (mapconcat #'(lambda (x) (format "<connection from-id='%s' to-id='%s'/>"
                                             (number-to-string (org-emind-cxl-node-id (nth 0 (nth 0 x))))
                                             (number-to-string (org-emind-cxl-node-id (nth 1 (nth 0 x))))))
                       edges "\n")
            "\n</connection-list>\n"
            "        <concept-appearance-list>
        </concept-appearance-list>
        <linking-phrase-appearance-list>
        </linking-phrase-appearance-list>
        <connection-appearance-list>
        </connection-appearance-list>
        <style-sheet-list>
        </style-sheet-list>
        <extra-graphical-properties-list>
        </extra-graphical-properties-list>
"
            "</map>\n</cmap>"

            )))

(defun org-emind-update-message (filename process event)
  "Write an update message on the output of running org-emind based on PROCESS and EVENT.
Open FILENAME according to value of `org-emind-display'."
  (let* ((e (with-current-buffer "*org-emind-errors*"
	      (buffer-string))))
    (if (string= e "")
        (princ (format "Org mind map %s" event))
      (princ (format "Org mind map %sErrors: %s" event e)))
    (if (string= event "finished\n")
	(progn
	  (cl-case org-emind-display
	    (nil nil)
	    (current (find-file filename))
	    (window (find-file-other-window filename))
	    (frame (switch-to-buffer-other-frame (find-file-noselect filename))))
	  (cl-case major-mode
	    (pdf-view-mode (pdf-view-fit-page-to-window))
	    (doc-view-mode (doc-view-fit-page-to-window)))))))

(defun org-emind-write-named (name &optional debug linksp)
  "Create a directed graph output based on the org tree in the current buffer, with name NAME.
To customize, see the org-emind group.
If DEBUG is non-nil, then print the dot command to the *Messages* buffer,
and print the dotfile to the *Messages* buffer or to a file if DEBUG is a filename.
If LINKSP is non-nil include graph edges for org links."
  (setq org-emind-node-id 0)
  (org-emind-data)
  (let ((cxl (org-emind-make-cxl)))
    (with-temp-file (concat name ".cxl")
      (insert cxl)
      (insert "\n")
      )
    ))

;;;###autoload
(defun org-emind-write-with-prompt nil
  "Prompt for an output FILENAME (without extension) to write your output and .dot files."
  (let ((filename (read-file-name "What is the file name you would like to save to?")))
    (org-emind-write-named filename (concat filename ".cxl")
			      (y-or-n-p "Include org links? "))))

(defun org-emind-default-filename (treenamep)
  "Return a default filename for saving the tree diagram.
If TREENAMEP is non-nil include in the filename the name of the top level header of the tree."
  (concat (file-name-sans-extension (buffer-name))
	  "_mindmap"
	  (if treenamep
	      (concat "-"
		      (replace-regexp-in-string " +" "_"
                                                (nth 4 (org-heading-components)))))))

;;;###autoload
(defun org-emind-write (&optional promptp)
  "Create a digraph based on all org trees in the current buffer.
The digraph will be named the same name as the current buffer.
To customize, see the org-emind group.
If called with prefix arg (or PROMPTP is non-nil), then call `org-emind-write-with-prompt'."
  (interactive "P")
  (if promptp (org-emind-write-with-prompt)
    (org-emind-write-named (org-emind-default-filename nil))))

;;;###autoload
(defun org-emind-write-current-branch (&optional promptp)
  "Create a directed graph output based on just the current org tree branch.
To customize, see the org-emind group.
If called with prefix arg (or PROMPTP is non-nil), then call `org-emind-write-with-prompt'."
  (interactive "P")
  (org-narrow-to-subtree)
  (let ((filename (if promptp (org-emind-write-with-prompt)
		    (org-emind-write-named (org-emind-default-filename nil) t))))
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

;;;###autoload
(defmacro org-emind-make-node-fn (name doc props &optional shape color other)
  "Create a function org-emind-NAME-node for use with :OMM-NODE-FMT writing node properties.
The created function should be added to `org-emind-node-formats' and the associated string
can be used as the :OMM-NODE-FMT for a tree.
Document the function with the DOC arg.
PROPS is a list of either property & format string pairs, or individual property names,
which will be placed in each node, e.g: ((\"PROB\" \"probability=%s\") \"COST\").
For property names with no format string, \"%s=%s\" will be used with the property name and value.

The node shape and background color can be specified with the optional SHAPE and COLOR arguments,
and any other attributes (e.g. \"fontsize=30\") can be specified with the OTHER argument.
Each of these arguments can be either a string or a form which is evaluated for each node,
and returns a string.

Example: (org-emind-make-node-fn decisiontree \"Draw decision tree\" (\"COST\" (\"NOTES\" \"Notes: %s\")) nil
			   (cond ((equal (org-emind-get-property :todo-keyword el) \"ACTION\") \"red\")
				 ((equal (org-emind-get-property :todo-keyword el) \"STATE\") \"yellow\")
				 ((equal (org-emind-get-property :todo-keyword el) \"DECISION\") \"green\")))

You could put this code in your emacs startup file (e.g. ~/.emacs) and then add to `org-emind-node-formats'
the pair '(\"decisiontree\" . org-emind-decisiontree-node), and use \":OMM-NODE-FMT: decisiontree\" as a
tree property."
  `(defun ,(intern (concat "org-emind-" (symbol-name name) "-node"))
       (title tags color hm el)
     ,doc
     (let* ((numtags (if tags (length tags)))
	    (colspan (if tags (int-to-string numtags)))
	    (propstxt
	     (cl-remove
	      nil (list ,@(mapcar
			   (lambda (p)
			     (cond ((stringp p)
				    `(--if-let (org-emind-get-property ,p el)
					 (concat ,(upcase p) "=" it)))
				   ((consp p)
				    `(--if-let (org-emind-get-property ,(car p) el)
					 (format ,(nth 1 p) it)))
				   (t (error "Invalid props value"))))
			   props))))
	    (shape ,shape)
	    (color (or color ,color))
	    (other ,other))
       (concat "[label=<<table" (if shape " border=\"0\"") ">"
	       (if numtags (concat "<tr><td colspan=\"" colspan "\" ") "<tr><td")
	       (if (and color (not shape)) (concat " bgcolor=\"" color "\" "))
	       ">" title "</td></tr>"
	       (mapconcat (lambda (p)
			    (concat "<tr>" (org-emind-add-color hm p numtags) "</tr>"))
			  propstxt "")
	       (if numtags
		   (concat "<tr>"
			   (mapconcat (-partial 'org-emind-add-color hm) tags "")
			   "</tr>"))
	       "</table>>"
	       (if shape (concat ",shape=" shape (if color (concat ",style=filled,color=" color))))
	       (if other (concat "," other)) "];"))))

;;;###autoload
(defmacro org-emind-make-edge-fn (name doc props &optional style color other)
  "Create a function org-emind-write-NAME for writing edge properties which can be used for :OMM-EDGE-FMT.
Document the function with the DOC arg.
PROPS is a list of either property & format string pairs, or individual property names,
which will concatenated and used to label the edges, e.g: ((\"PROB\" \"probability=%s\") \"COST\").
For property names with no format string \"%s=%s\" will be used with the property name and value.

The edge style and color can be specified with the optional STYLE and COLOR arguments,
and any other attributes (e.g. \"fontsize=30\") can be specified with the OTHER argument.
Each of these arguments can be either a string or a form which is evaluated for each node,
and returns a string.

Example: (org-emind-make-edge-fn decisiontree \"Draw decision tree\" (\"PROB\"))

You could put this code in your emacs startup file (e.g. ~/.emacs) and then add to `org-emind-edge-formats'
the pair '(\"decisiontree\" . org-emind-decisiontree-edge), and use \":OMM-EDGE-FMT: decisiontree\" as a
tree property."
  `(defun ,(intern (concat "org-emind-" (symbol-name name) "-edge"))
       (hm el)
     ,doc
     (let* ((propstxt (cl-remove
		       nil (list ,@(mapcar (lambda (p)
					     (cond ((stringp p)
						    `(--if-let (org-emind-get-property ,p el)
							 (concat ,(upcase p) "=" it)))
						   ((consp p)
						    `(--if-let (org-emind-get-property ,(car p) el)
							 (format ,(nth 1 p) it)))
						   (t (error "Invalid props value"))))
					   props))))
	    (style ,style)
	    (color ,color)
	    (other ,other))
       (concat "[label=\"" (mapconcat 'identity propstxt ",") "\""
	       (if color (concat ",color=\"" color "\" "))
	       (if style (concat ",style=\"" style "\""))
	       (if other (concat "," other)) "]"))))

(defun org-emind-export-message nil
  "Message string for `org-export-dispatch' buffer."
  (if (> (length org-emind-dot-output) 1)
      "Select output file format"
    (concat "As " (car org-emind-dot-output) " file")))

;; Add a tool bar icon
;; (define-key org-mode-map [tool-bar org-button]
;; '(menu-item "Write the org-mode file mind map to disk." org-emind-write-with-prompt
;;    :image (image :type xpm :file "info.xpm")
;;    ))

;; Add menu items
;; (define-key org-mode-map [menu-bar Org Diagram]
;;   (cons "Graphviz diagram" (make-sparse-keymap "Graphviz diagram")))

;; (define-key org-mode-map [menu-bar Org Diagram all]
;;   '("Diagram of whole buffer" . org-emind-write))

;; (define-key org-mode-map [menu-bar Org Diagram current]
;;   '("Diagram of current tree" . org-emind-write-current-tree))

;; (define-key org-mode-map [menu-bar Org Diagram branch]
;;   '("Diagram of current branch" . org-emind-write-current-branch))

;; (global-set-key (kbd "<f4>") 'org-emind-write)

(provide 'org-emind)
;;; org-emind.el ends here
