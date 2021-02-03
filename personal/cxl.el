;;; cxl.el --- CXL files management
;; Author: Emmanuele Somma <emmanuele@exedre.org>
;; Keywords: cmap, mind map
;; Package-Version:
;; Package-Commit:
;; Version: 0.1
;; URL: https://github.com/exedre/cxl-el
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
;;  `org-emind-wrap-line-length'
;;    Line length within graphviz nodes.
;;    default = 30
;;  `org-emind-wrap-legend-line-length'
;;    Line length of the graphviz legend.
;;    default = 45
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
(require 'subr-x)

(defconst cxl-version "0.4")

(defgroup cxl nil
  "Convert org-mode tree into a graphviz directed graph"
  :group 'org)

(defun cxl-load (fname)
  (interactive "f")
  (let* ((xml-dom-tree (with-temp-buffer
                         (insert-file-contents fname)
                         (libxml-parse-xml-region (point-min) (point-max))))
         ;; Get first level children with "img" tag.
         (concept-nodes (dom-by-tag xml-dom-tree 'concept)))
    (dom-pp xml-dom-tree)
    ))

(defun cxl-save (fname)
  )

(defun cxl-add-node ())

(provide 'cxl)
;;; cxl.el ends here
