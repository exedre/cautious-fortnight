;;; org-cxlmap --- Creates a directed graph from org-mode files -*- lexical-binding: t -*-
;; Author: Ted Wiles <theodore.wiles@gmail.com>
;; Keywords: orgmode, extensions, graphviz, dot
;; Package-Version: 20180826.2340
;; Package-Commit: 95347b2f9291f5c5eb6ebac8e726c03634c61de3
;; Version: 0.4
;; URL: https://github.com/theodorewiles/org-cxlmap
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
;;  `org-cxlmap-write'
;;    Create a digraph based on all org trees in the current buffer.
;;    Keybinding: M-x org-cxlmap-write
;;  `org-cxlmap-write-current-branch'
;;    Create a directed graph output based on just the current org tree branch.
;;    Keybinding: M-x org-cxlmap-write-current-branch
;;  `org-cxlmap-write-current-tree'
;;    Create a directed graph output based on the whole current org tree.
;;    Keybinding: M-x org-cxlmap-write-current-tree
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `org-cxlmap-unflatten-command'
;;    Shell executable command for running the UNFLATTEN command.
;;    default = "unflatten -l3"
;;  `org-cxlmap-dot-command'
;;    Shell executable command for running the DOT command.
;;    default = "dot"
;;  `org-cxlmap-dot-output'
;;    Format of the DOT output.  Defaults to PDF.
;;    default = "pdf"
;;  `org-cxlmap-engine'
;;    Sets the layout engine used in your graphs.
;;    default = "dot"
;;  `org-cxlmap-default-node-attribs'
;;    Alist of default node attributes and values.
;;    default = '(("shape" . "plaintext"))
;;  `org-cxlmap-default-edge-attribs'
;;    Alist of default edge attributes and values.
;;    default = nil
;;  `org-cxlmap-default-graph-attribs'
;;    Alist of default graph attributes and values.
;;    default = '(("autosize" . "false") ("size" . "9,12") ("resolution" . "100") ...))
;;  `org-cxlmap-node-formats'
;;    Assoc list of (NAME . FN) pairs where NAME is a value for the :OMM-NODE-FMT property
;;    See also `org-cxlmap-make-node-fn'
;;    default = nil
;;  `org-cxlmap-edge-formats'
;;    Assoc list of (NAME . FN) pairs where NAME is a value for the :OMM-EDGE-FMT property
;;    See also `org-cxlmap-make-edge-fn'
;;    default = nil
;;  `org-cxlmap-edge-format-default'
;;    Default format string for graph edges, e.g. "[style=dotted]".
;;    default = ""
;;  `org-cxlmap-reserved-colors'
;;    List of colors that will not be used for coloring tags.
;;    default = nil
;;  `org-cxlmap-tag-colors'
;;    An alist of (TAG . COLOR) pairs for choosing colors for tags.
;;    default = nil
;;  `org-cxlmap-include-text'
;;    A boolean indicating whether our not to include paragraph text in body of nodes.
;;    default = t
;;  `org-cxlmap-include-images'
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
;; (load "org-cxlmap.el")

;; If on linux, customize the values of `org-cxlmap-unflatten-command'
;; and `org-cxlmap-dot-command' to have the values corresponding to
;; the executables in your system.

;; Then, run "M-x org-cxlmap-write" to create a graph of all trees in the current buffer,

;; You can customize the style of the graph by adding :OMM-NODE-FMT and :OMM-EDGE-FMT properties
;; to the headlines in the tree.

;; The latest version is available at:
;;
;; https://github.com/theodorewiles/org-cxlmap
;;

;;; Code:

(require 'asoc)
(require 'dash)
(require 'org)
(require 'ht)
(require 's)
(require 'subr-x)

(defconst org-cxlmap-version "0.4")

(defgroup org-cxlmap nil
  "Convert org-mode tree into a graphviz directed graph"
  :group 'org)



(defcustom org-cxlmap-unflatten-command "unflatten -l3"
  "Shell executable command for running the UNFLATTEN command."
  :type 'string
  :group 'org-cxlmap)

;; nodechar . (background color , border color)

;; border -shape -thicness -style see https://cmap.ihmc.us/xml/CXL.html

(defcustom org-cxlmap-default-node-attribs '(("@" . ("font-style=\"bold\""
                                                     "background-color=\"200,255,200,255\""
                                                     "border-color=\"255,0,0,255\""))
                                             ("L" . ())
                                             ("N" . ("border-color=\"0,0,0,255\""))
                                             ("X" . ("font-style=\"bold\""
                                                     "background-color=\"200,255,200,255\""
                                                     "border-color=\"255,0,0,255\""))
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
  :group 'org-cxlmap)

; (assoc "L" org-cxlmap-default-node-attribs 'string-equal)

(defcustom org-cxlmap-default-node-entities'(("^.+\\(>>\\|||\\)" . "")
                                             (">" . "&gt;")
                                             ("<" . "&lt;")
                                             (" *§ *" . "&#xa;")
                                             ("\"" . "&quot;")
                                             ("^ +". ""))
  "Alist of default graph attributes and values.
Each item in the alist should be a cons cell of the form (ATTRIB . VALUE)
where ATTRIB and VALUE are strings.
For a list of value attributes, see here: https://graphviz.gitlab.io/_pages/doc/info/attrs.html"
  :type '(alist :key-type (string :tag "Attribute") :value-type (string :tag " Value"))
  :group 'org-cxlmap)

(defcustom org-cxlmap-node-formats nil
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
  :group 'org-cxlmap)

(defcustom org-cxlmap-edge-formats nil
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
  :group 'org-cxlmap)

(defcustom org-cxlmap-edge-format-default ""
  "Default format string for graph edges, e.g. \"[style=dotted]\"."
  :type 'string
  :group 'org-cxlmap)

(defcustom org-cxlmap-reserved-colors nil
  "List of colors that will not be used for coloring tags.
These colors will be excluded when random tag colors are chosen by `org-cxlmap-rgb'
so that you can use them for other things.
Each color should be in hexadecimal form, e.g: \"#e3cfbc\", where the consecutive pairs
of hexdigits indicate levels of red, green and blue respectively.
It is not necessary to include any colors with levels below 7d, as these are not used
for creating random tag colors."
  :type '(repeat string)
  :group 'org-cxlmap)

(defcustom org-cxlmap-tag-colors nil
  "An alist of (TAG . COLOR) pairs for choosing colors for tags.
Any tags not listed here will be colored with randomly selected colors that dont
clash with those in `org-cxlmap-reserved-colors'.
Each color should be in hexadecimal form, e.g: \"#e3cfbc\", where the consecutive pairs
of hexdigits indicate levels of red, green and blue respectively.

Note: you can also set tag colors by altering the hashmap passed as an argument to functions
defined in `org-cxlmap-node-formats'."
  :type '(alist :key-type (string :tag "     Tag") :value-type (string :tag "Color"))
  :group 'org-cxlmap)

(defcustom org-cxlmap-include-text t
  "A boolean indicating whether our not to include paragraph text in body of nodes.
   default = t"
  :type 'boolean
  :group 'org-cxlmap
  )

(defcustom org-cxlmap-include-images t
  "A boolean indicating whether our not to include paragraph text in body of nodes.
   default = t"
  :type 'boolean
  :group 'org-cxlmap
  )


(defcustom org-cxlmap-do-fill t
  "A boolean indicating whether our not to fill paragraph text in body of nodes.
   default = t"
  :type 'boolean
  :group 'org-cxlmap
  )

(defcustom org-cxlmap-do-index nil
  "A boolean indicating whether our not to fill paragraph text in body of nodes.
   default = t"
  :type 'boolean
  :group 'org-cxlmap
  )

(defcustom org-cxlmap-fill-column 60
  "Fill column for strings wraps"
  :type 'number
  :group 'org-cxlmap
  )

(defvar org-cxlmap-node-id 0
  "Variable containing current node id")

(defvar org-cxlmap-edge-id 0
  "Variable cointaining current edge id")

(defvar org-cxlmap-nodes (make-hash-table :test 'equal)
  "nodes list title -> id")

(defvar org-cxlmap-edges (make-hash-table :test 'equal)
  "nodes list id -> node1 node2")

(defvar org-cxlmap-bold-strings (asoc-make)
  "list of bolfaces in form ( id . ((from . to) (from . to)))")

(defun org-cxlmap-nodes-add (type id title &optional composed)
  "add node to nodes hash"
  ;(unless composed (setq title (format "%04d-%s" id title)))
  (let ((name (concat type title)))
    (unless (gethash name org-cxlmap-nodes nil)
      (message (format "NODE-ADD: %04d : %s " id name))
      (puthash name id org-cxlmap-nodes))))

(defun org-cxlmap-edges-add (edge-a edge-b)
  "add edge to edges hash"
  (let ((label (format "F%04dT%04d" edge-a edge-b) ))
    (unless (gethash label org-cxlmap-edges nil)
      (message (format "EDGE-ADD: %04d to %04d as %s" edge-a edge-b label))
      (puthash label (list edge-a edge-b) org-cxlmap-edges))))

(defun org-cxlmap-cxl-node-id (el)
  "Returns id part of element"
  (cdr el))

(defun org-cxlmap-cxl-node-name (el)
  "Returns name part of element."
  (replace-regexp-in-string "^.\\(.+|\\)?" "" (nth 0 el)))

(defun org-cxlmap-cxl-node-type (el)
  "Returns type part of element."
  (substring (nth 0 el) 0 1))

(defun org-cxlmap-cxl-node-props (el)
  "Returns name part of element."
  (if (string-match-p "|" (nth 0 el))
      (replace-regexp-in-string "|.*$" "|" (nth 0 el))
    (concat (substring (nth 0 el) 0 1) (substring (nth 0 el) 0 1) "|")))

(defun org-cxlmap-get-property (prop el &optional inheritp)
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

(defun org-cxlmap-narrow-to-heading-content (b)
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


(defun org-cxlmap-first-headline (e)
  "Figure out the first headline within element E."
  (let* ((parent (org-element-property :parent e)))
    (if parent
        (if (eq (org-element-type parent) 'headline)
            parent
          (org-cxlmap-first-headline parent))
      nil)))


(defun org-cxlmap-id-get (title nodes)
  "Give title element id using `org-cxlmap-cxl-node-id'"
  (if title
      (let ((id (or (gethash (concat "N" title) nodes)
                    (gethash (concat "L" title) nodes))))
        (unless id
          (setq org-cxlmap-node-id (1+ org-cxlmap-node-id)
                id org-cxlmap-node-id))
      id)))



(defun org-cxlmap-data-sub (this-title this-id parent)
  (let* ((pparent-title (org-element-property :title parent))
         (pparent-id (org-cxlmap-id-get pparent-title org-cxlmap-nodes))
         (composed nil)
         (type "N")
         (parent-title (or (and (string-match "\\(.+\\) *>>" this-title)
                                (setq type "L")
                                (setq composed 't)
                                (format "L%04d|%s" pparent-id
                                        (replace-regexp-in-string
                                         "^ +" ""
                                         (match-string 1 this-title))))
                           pparent-title))
         (parent-id (org-cxlmap-id-get parent-title org-cxlmap-nodes)))
    (if parent-title
        (prog2
            (unless (eq pparent-id parent-id)
              (org-cxlmap-edges-add pparent-id parent-id))
            (org-cxlmap-nodes-add type parent-id parent-title composed)
            (org-cxlmap-edges-add parent-id this-id)))
    (org-cxlmap-nodes-add "N" this-id this-title)))

(defun org-cxlmap-data ()
  "Create graph  of all directed pairs of headlines for constructing the digraph."
  (clrhash org-cxlmap-nodes)
  (clrhash org-cxlmap-edges)
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (hl)
      (let* ((parent (org-element-property :parent hl))
             (this-title (org-element-property :title hl))
             (this-id (org-cxlmap-id-get this-title org-cxlmap-nodes)))
        (org-cxlmap-data-sub this-title this-id parent)))))

(defun org-cxlmap-translate-entities (text)
  (car
   (last
    (mapcar #'(lambda (x)
                (setq text
                      (replace-regexp-in-string
                       (car x) (cdr x) text)))
            org-cxlmap-default-node-entities))))

(defun org-cxlmap-text-fill (text fill)
  "Fill text to given column if `org-cxlmap-do-fill'"
  (if org-cxlmap-do-fill
      (let ((newtext "")
            (last 0)
            (words (split-string (replace-regexp-in-string " *§ *" " " text) " "))
            (col (or (string-match "§" text) fill)))
        (mapcar #'(lambda (word)
                    (progn
                      (if (> (+ (- (length newtext) last) (length word)) col )
                          (and (setq newtext (concat newtext " § "))
                               (setq last (length newtext))))
                      (setq newtext (concat newtext " "  word))))
                words)
        (replace-regexp-in-string " +" " "newtext))
    text))

;(org-cxlmap-text-fill "Una parte di questa realtà, la “realtà sociale”, è il prodotto dell’azione e dell’interpretazione delle persone. Le azioni delle persone creano la realtà sociale e, nello stesso tempo, la riproducono costantemente (talvolta, la trasformano)" 60)

(defun org-cxlmap-make-cxl--edges-appearance (edges)
  "Create the cxl string for edges appearance."
  (concat
   "<!-- EDGES -->\n<connection-appearance-list>\n"
   (mapconcat
    #'(lambda (x)
        (let ((id (car x))
              (id1 (nth 0 (cdr x)))
              (id2 (nth 1 (cdr x))))
          (format "<connection-appearance id='E%s' from-pos='right' to-pos='left'/>" id id1 id2)))
    edges "\n")
   "\n</connection-appearance-list>\n"))

(defun org-cxlmap-make-cxl--edges (edges)
  "Create the cxl string for edges."
  (concat
   "<!-- EDGES -->\n<connection-list>\n"
   (mapconcat
    #'(lambda (x)
        (let ((id (car x))
              (id1 (nth 0 (cdr x)))
              (id2 (nth 1 (cdr x))))
          (format "<connection id='E%s' from-id='%s' to-id='%s'/>" id id1 id2)))
    edges "\n")
   "\n</connection-list>\n"))

(defun org-cxlmap-props-get (title)
  "Get properties from TITLE."
  (if (and title (string-match "^\\(.\\)\\(.\\)\\(.*\\)|" title))
    (let* ((type (match-string 1 title))
           (prop (assoc (match-string 2 title) org-cxlmap-default-node-attribs 'string-equal)) )
      (mapconcat 'identity (cdr prop) " "))
    ""))

(defun org-cxlmap--bold-count-cr (text pos)
  "Return position moved back of cr translation entity"
  (if (> pos 0)
      (- pos (* 4  (s-count-matches "&#xa;" text 0 (1+ pos))))
    pos))

(defun org-cxlmap-bold (id text)
  "Remove bold characters and store their position"
  (while  (string-match "\\*\\([^\\*]+\\)\\*" text)
    (message "IN" text)
    (let ((from (match-beginning 0))
          (to (match-end 0)))
      (asoc-put! org-cxlmap-bold-strings
                 id
                 (list (org-cxlmap--bold-count-cr text from)
                       (- (org-cxlmap--bold-count-cr text to) 2)))
      (setq text (replace-regexp-in-string
                  "\\(\\*\\([^\\*]*\\)\\*\\).*\\'"
                  "\\2" text nil nil 1))
      (message "IN " text from to)))
  text)

;; (setq org-cxlmap-bold-strings (asoc-make))
;; (org-cxlmap-bold 1 "prov a *puzza* allo *sprofondo*")
;; (asoc-pop! org-cxlmap-bold-strings 1)

(defun org-cxlmap-make-cxl--node (nodes)
  "Create the cxl string for nodes."
  (setq org-cxlmap-bold-strings (asoc-make))
  (concat
   "<!-- nodes -->\n<concept-list>\n"
   (mapconcat
    #'(lambda (x)
        (let* ((id (org-cxlmap-cxl-node-id x))
              (type (org-cxlmap-cxl-node-type x))
              (title (org-cxlmap-cxl-node-name x))
              (idx   (if org-cxlmap-do-index
                         (format "parent-id=\"X%d\" group-type=\"stack\" group-order=\"1\"" id)
                         "")))
          (when (string-equal type "N")
            (concat
             (if org-cxlmap-do-index
                 (format "<concept id=\"X%d\" label=\"\"/>" id))
             (format "<concept id=\"%d\" label=\"%s\" %s />" id
                     (org-cxlmap-bold
                      id
                      (org-cxlmap-translate-entities
                       (org-cxlmap-text-fill title org-cxlmap-fill-column)))
                     idx)))))
    nodes "\n")
   "\n</concept-list>\n"))

(defun org-cxlmap-make-cxl--node-appearance (nodes)
  "Create the cxl string for nodes appearance."
  (concat
   "<!-- nodes -->\n<concept-appearance-list>\n"
   (mapconcat
    #'(lambda (x)
        (let* ((id (org-cxlmap-cxl-node-id x))
              (type (org-cxlmap-cxl-node-type x))
              (title (org-cxlmap-cxl-node-name x)))
          (if (string-equal type "N")
              (concat
               (if org-cxlmap-do-index
                   (format "<concept-appearance id=\"X%d\" expanded=\"true\"/>" id))
               (format "<concept-appearance id=\"%d\" %s" id
                       (org-cxlmap-props-get
                        (org-cxlmap-cxl-node-props x)))
               (if (asoc-contains-key-p org-cxlmap-bold-strings id)
                   (concat ">\n"
                           (mapconcat
                            'identity
                            (asoc--map
                                (when (eq key id)
                                  (format "\t<localized-style begin=\"%d\" end=\"%d\" font-color=\"255,0,0,255\" font-style=\"bold\"/>"
                                          (car value) (cadr value)))
                              org-cxlmap-bold-strings) "\n")
                           "\n</concept-appearance>")
                 "/>")))))
    nodes "\n")
   "\n</concept-appearance-list>\n"))


(defun org-cxlmap-make-cxl--links (nodes)
  "Create the cxl string for links."
  (concat
   "<!-- Linking Phrases -->\n<linking-phrase-list>\n"
   (mapconcat
    #'(lambda (x)
        (let ((id (org-cxlmap-cxl-node-id x))
              (type (org-cxlmap-cxl-node-type x))
              (title (org-cxlmap-cxl-node-name x)))
          (if (string-equal type "L")
              (format "<linking-phrase id=\"%d\" label=\"%s\" />" id
                      (org-cxlmap-bold
                       id
                       (org-cxlmap-translate-entities
                        (org-cxlmap-text-fill title org-cxlmap-fill-column)))))))
    nodes "\n")
   "\n</linking-phrase-list>\n"))

(defun org-cxlmap-make-cxl--links-appearance (nodes)
  "Create the cxl string for links appearance."
  (concat
   "<!-- Linking Phrases -->\n<linking-phrase-appearance-list>\n"
   (mapconcat
    #'(lambda (x)
        (let ((id (org-cxlmap-cxl-node-id x))
              (type (org-cxlmap-cxl-node-type x))
              (title (org-cxlmap-cxl-node-name x)))
          (if (string-equal type "L")
              (concat
               (format "<linking-phrase-appearance id=\"%d\" %s" id
                       (org-cxlmap-props-get
                        (org-cxlmap-cxl-node-props x)))
               (if (asoc-contains-key-p org-cxlmap-bold-strings id)
                   (concat ">\n"
                           (mapconcat 'identity
                                      (asoc--map
                                          (when (eq key id)
                                            (format "\t<localized-style begin=\"%d\" end=\"%d\" font-style=\"bold\"/>"
                                                    (car value) (cadr value)))
                                        org-cxlmap-bold-strings) "\n")
                           "\n</linking-phrase-appearance>")
                 "/>")))))
    nodes "\n")
   "\n</linking-phrase-appearance-list>\n"))


(defun org-cxlmap-make-cxl ()
  "Create the dot file from DATA."
  (let* (
         (nodes (ht->alist org-cxlmap-nodes))
         (edges (ht->alist org-cxlmap-edges))
         (title (plist-get (org-export-get-environment) ':title))
        )
    (concat (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<cmap xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns=\"http://cmap.ihmc.us/xml/cmap/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:vcard=\"http://www.w3.org/2001/vcard-rdf/3.0#\">\m<res-meta>\n        <dc:title>%s</dc:title>\n         <dc:format>x-cmap/x-storable</dc:format>\n </res-meta>\n<map>\n" title)
            (org-cxlmap-make-cxl--node nodes)
            (org-cxlmap-make-cxl--node-appearance nodes)
            (org-cxlmap-make-cxl--links nodes)
            (org-cxlmap-make-cxl--links-appearance nodes)
            (org-cxlmap-make-cxl--edges edges)
            (org-cxlmap-make-cxl--edges-appearance edges)
            "
        <linking-phrase-appearance-list>
        </linking-phrase-appearance-list>
        <connection-appearance-list>
        </connection-appearance-list>
        <style-sheet-list>
        </style-sheet-list>
        <extra-graphical-properties-list>
        </extra-graphical-properties-list>
"
            "</map>\n</cmap>")))

(defun org-cxlmap-layout ()
  "Layout the map"
  (interactive)
  (let ((edges (ht->alist org-cxlmap-edges))
        (tree
         (mapcar #'(lambda (x) (list (car x) .
                                     (make-hash-table :test 'equal)))
                 (ht->alist org-cxlmap-nodes)))
        )
    (mapcar #'(lambda (x)
                (let* ((from (cadr x))
                       (to (caddr x))
                       (leaf (assoc from tree)))
                  (when leaf )
                  (cons (cons from (list to)) leaf)))
            edges)))


(defun org-cxlmap-update-message (filename process event)
  "Write an update message on the output of running org-cxlmap based on PROCESS and EVENT.
Open FILENAME according to value of `org-cxlmap-display'."
  (let* ((e (with-current-buffer "*org-cxlmap-errors*"
	      (buffer-string))))
    (if (string= e "")
        (princ (format "Org mind map %s" event))
      (princ (format "Org mind map %sErrors: %s" event e)))
    (if (string= event "finished\n")
	(progn
	  (cl-case org-cxlmap-display
	    (nil nil)
	    (current (find-file filename))
	    (window (find-file-other-window filename))
	    (frame (switch-to-buffer-other-frame (find-file-noselect filename))))
	  (cl-case major-mode
	    (pdf-view-mode (pdf-view-fit-page-to-window))
	    (doc-view-mode (doc-view-fit-page-to-window)))))))

(defun org-cxlmap-write-named (name &optional debug linksp)
  "Create a directed graph output based on the org tree in the current buffer, with name NAME.
To customize, see the org-cxlmap group.
If DEBUG is non-nil, then print the dot command to the *Messages* buffer,
and print the dotfile to the *Messages* buffer or to a file if DEBUG is a filename.
If LINKSP is non-nil include graph edges for org links."
  (setq org-cxlmap-node-id 0)
  (org-cxlmap-data)
  (let ((cxl (org-cxlmap-make-cxl)))
    (with-temp-file (concat name ".cxl")
      (insert cxl)
      (insert "\n")
      )
    ))

;;;###autoload
(defun org-cxlmap-write-with-prompt nil
  "Prompt for an output FILENAME (without extension) to write your output and .dot files."
  (let ((filename (read-file-name "What is the file name you would like to save to?")))
    (org-cxlmap-write-named filename (concat filename ".cxl")
                            (y-or-n-p "Include org links? "))))

(defun org-cxlmap-default-filename (treenamep)
  "Return a default filename for saving the tree diagram.
If TREENAMEP is non-nil include in the filename the name of the top level header of the tree."
  (concat
   (file-name-sans-extension (buffer-name))
   "_mindmap"
   (if treenamep
       (concat
        "-"
	(replace-regexp-in-string
         " +" "_"
         (nth 4 (org-heading-components)))))))

;;;###autoload
(defun org-cxlmap-write-index (&optional promptp)
  "Create a digraph based on all org trees in the current buffer.
The digraph will be named the same name as the current buffer.
To customize, see the org-cxlmap group.
If called with prefix arg (or PROMPTP is non-nil), then call `org-cxlmap-write-with-prompt'."
  (interactive "P")
  (let ((fill-v org-cxlmap-do-fill))
    (setq org-cxlmap-do-fill nil)
    (setq org-cxlmap-do-index t)
    (if promptp
        (org-cxlmap-write-with-prompt)
      (org-cxlmap-write-named
       (org-cxlmap-default-filename nil)))
    (setq org-cxlmap-do-fill fill-v)
    (setq org-cxlmap-do-fill nil)))

;;;###autoload
(defun org-cxlmap-write (&optional promptp)
  "Create a digraph based on all org trees in the current buffer.
The digraph will be named the same name as the current buffer.
To customize, see the org-cxlmap group.
If called with prefix arg (or PROMPTP is non-nil), then call `org-cxlmap-write-with-prompt'."
  (interactive "P")
  (if promptp
      (org-cxlmap-write-with-prompt)
    (org-cxlmap-write-named
     (org-cxlmap-default-filename nil))))

;;;###autoload
(defun org-cxlmap-write-current-branch (&optional promptp)
  "Create a directed graph output based on just the current org tree branch.
To customize, see the org-cxlmap group.
If called with prefix arg (or PROMPTP is non-nil), then call `org-cxlmap-write-with-prompt'."
  (interactive "P")
  (org-narrow-to-subtree)
  (let ((filename (if promptp
                      (org-cxlmap-write-with-prompt)
		    (org-cxlmap-write-named
                     (org-cxlmap-default-filename nil) t))))
    (widen)
    filename))

;;;###autoload
(defun org-cxlmap-write-current-tree (&optional promptp)
  "Create a directed graph output based on the whole current org tree.
If called with prefix arg (or PROMPTP is non-nil), then call `org-cxlmap-write-with-prompt'."
  (interactive "P")
  (save-restriction
    (ignore-errors (outline-up-heading 100))
    (org-cxlmap-write-current-branch promptp)))

;;;###autoload
(defmacro org-cxlmap-make-node-fn (name doc props &optional shape color other)
  "Create a function org-cxlmap-NAME-node for use with :OMM-NODE-FMT writing node properties.
The created function should be added to `org-cxlmap-node-formats' and the associated string
can be used as the :OMM-NODE-FMT for a tree.
Document the function with the DOC arg.
PROPS is a list of either property & format string pairs, or individual property names,
which will be placed in each node, e.g: ((\"PROB\" \"probability=%s\") \"COST\").
For property names with no format string, \"%s=%s\" will be used with the property name and value.

The node shape and background color can be specified with the optional SHAPE and COLOR arguments,
and any other attributes (e.g. \"fontsize=30\") can be specified with the OTHER argument.
Each of these arguments can be either a string or a form which is evaluated for each node,
and returns a string.

Example: (org-cxlmap-make-node-fn decisiontree \"Draw decision tree\" (\"COST\" (\"NOTES\" \"Notes: %s\")) nil
			   (cond ((equal (org-cxlmap-get-property :todo-keyword el) \"ACTION\") \"red\")
				 ((equal (org-cxlmap-get-property :todo-keyword el) \"STATE\") \"yellow\")
				 ((equal (org-cxlmap-get-property :todo-keyword el) \"DECISION\") \"green\")))

You could put this code in your emacs startup file (e.g. ~/.emacs) and then add to `org-cxlmap-node-formats'
the pair '(\"decisiontree\" . org-cxlmap-decisiontree-node), and use \":OMM-NODE-FMT: decisiontree\" as a
tree property."
  `(defun ,(intern (concat "org-cxlmap-" (symbol-name name) "-node"))
       (title tags color hm el)
     ,doc
     (let* ((numtags (if tags (length tags)))
	    (colspan (if tags (int-to-string numtags)))
	    (propstxt
	     (cl-remove
	      nil (list ,@(mapcar
			   (lambda (p)
			     (cond ((stringp p)
				    `(--if-let (org-cxlmap-get-property ,p el)
					 (concat ,(upcase p) "=" it)))
				   ((consp p)
				    `(--if-let (org-cxlmap-get-property ,(car p) el)
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
			    (concat "<tr>" (org-cxlmap-add-color hm p numtags) "</tr>"))
			  propstxt "")
	       (if numtags
		   (concat "<tr>"
			   (mapconcat (-partial 'org-cxlmap-add-color hm) tags "")
			   "</tr>"))
	       "</table>>"
	       (if shape (concat ",shape=" shape (if color (concat ",style=filled,color=" color))))
	       (if other (concat "," other)) "];"))))

;;;###autoload
(defmacro org-cxlmap-make-edge-fn (name doc props &optional style color other)
  "Create a function org-cxlmap-write-NAME for writing edge properties which can be used for :OMM-EDGE-FMT.
Document the function with the DOC arg.
PROPS is a list of either property & format string pairs, or individual property names,
which will concatenated and used to label the edges, e.g: ((\"PROB\" \"probability=%s\") \"COST\").
For property names with no format string \"%s=%s\" will be used with the property name and value.

The edge style and color can be specified with the optional STYLE and COLOR arguments,
and any other attributes (e.g. \"fontsize=30\") can be specified with the OTHER argument.
Each of these arguments can be either a string or a form which is evaluated for each node,
and returns a string.

Example: (org-cxlmap-make-edge-fn decisiontree \"Draw decision tree\" (\"PROB\"))

You could put this code in your emacs startup file (e.g. ~/.emacs) and then add to `org-cxlmap-edge-formats'
the pair '(\"decisiontree\" . org-cxlmap-decisiontree-edge), and use \":OMM-EDGE-FMT: decisiontree\" as a
tree property."
  `(defun ,(intern (concat "org-cxlmap-" (symbol-name name) "-edge"))
       (hm el)
     ,doc
     (let* ((propstxt (cl-remove
		       nil (list ,@(mapcar (lambda (p)
					     (cond ((stringp p)
						    `(--if-let (org-cxlmap-get-property ,p el)
							 (concat ,(upcase p) "=" it)))
						   ((consp p)
						    `(--if-let (org-cxlmap-get-property ,(car p) el)
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

(defun org-cxlmap-export-message nil
  "Message string for `org-export-dispatch' buffer."
  (if (> (length org-cxlmap-dot-output) 1)
      "Select output file format"
    (concat "As " (car org-cxlmap-dot-output) " file")))

;; Add a tool bar icon
;; (define-key org-mode-map [tool-bar org-button]
;; '(menu-item "Write the org-mode file mind map to disk." org-cxlmap-write-with-prompt
;;    :image (image :type xpm :file "info.xpm")
;;    ))

;; Add menu items
;; (define-key org-mode-map [menu-bar Org Diagram]
;;   (cons "Graphviz diagram" (make-sparse-keymap "Graphviz diagram")))

;; (define-key org-mode-map [menu-bar Org Diagram all]
;;   '("Diagram of whole buffer" . org-cxlmap-write))

;; (define-key org-mode-map [menu-bar Org Diagram current]
;;   '("Diagram of current tree" . org-cxlmap-write-current-tree))

;; (define-key org-mode-map [menu-bar Org Diagram branch]
;;   '("Diagram of current branch" . org-cxlmap-write-current-branch))

;; (global-set-key (kbd "<f4>") 'org-cxlmap-write)

(provide 'org-cxlmap)
;;; org-cxlmap.el ends here
