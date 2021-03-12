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
;;    Create a concept map based on all org trees in the current buffer.
;;    Keybinding: M-x org-emind-write
;;  `org-emind-write-current-branch'
;;    Create a concept map output based on just the current org tree branch.
;;    Keybinding: M-x org-emind-write-current-branch
;;  `org-emind-write-current-tree'
;;    Create a concept map output based on the whole current org tree.
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

(require 'cl-macs)
(require 'dash)
(require 'org)
(require 'subr-x)
(require 's)

(defconst cxl-version "0.4")

(defgroup cxl nil
  "Convert org-mode tree into a graphviz concept map"
  :group 'org)

(defcustom cxl-highlights '(("«»" . "font-color=\"255,0,0,255\"")
                            )
  "Alist of highlights."
  :type '(alist :key-type (string :tag "Attribute") :value-type (string :tag " Value"))
  :group 'cxl )

(defcustom cxl-default-node-entities '((">" . "&gt;")
                                       ("<" . "&lt;")
                                       ("\"" . "&quot;")
                                       ("\n" . "&#xa;"))
  "Alist of default graph attributes and values.
Each item in the alist should be a cons cell of the form (ATTRIB . VALUE)
where ATTRIB and VALUE are strings.
For a list of value attributes, see here: https://graphviz.gitlab.io/_pages/doc/info/attrs.html"
  :type '(alist :key-type (string :tag "Attribute") :value-type (string :tag " Value"))
  :group 'cxl)

(defcustom cxl-fill-column 60
  "Fill column for strings wraps"
  :type 'number
  :group 'cxl
  )

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

(defun cxl--make-text-decorations (type text)
  "Remove bold characters and store their position"
  (cond
   ((eq type 'highlights)
    (let* ((highlights (mapcar (lambda (x) (cons (cons (substring (car x) 0 1) (substring (car x) 1 2)) (cdr x))) cxl-highlights))
           (regex-format "\\(\\(%s\\)\\(.+?\\)%s\\)" )
           --decs
           (--last -1)
           (--prev 0)
           (--text (replace-regexp-in-string "&[^;]+;" "O" text)))
      (cl-loop for highlight in highlights
               do
               (let ((ops (car highlight)))
                 (while (and (< --last (length --text))
                             (string-match (format regex-format (car ops) (cdr ops)) --text (1+ --last)))
                   (let* ((--match (match-string 0 --text))
                          (--char (substring --match 0 1) )
                          (--symb (cdr highlight) )
                          (--beginning (match-beginning 0))
                          (--end  (setq --last (match-end 0))))
                     (push (list --symb --beginning --end (cdr highlight)) --decs)))))
      --decs))
   ((eq type 'decorations)
    (let ((decorations '(("*" . "bold")
                         ("/" . "italics")
                         ("=" . "verbatim")
                         ("_" . "underlined")
                         ("~" . "code")))
          (regex-format "\\(\\([\\*/=_~]\\)\\([^\\2]+?\\)\\2\\)")
          --decs
          (--last -1)
          (--prev 0)
          (--text (replace-regexp-in-string "&[^;]+;" "O" text)))
      (while (and (< --last (length --text))
                  (string-match regex-format --text (1+ --last)))
        (let* ((--match (match-string 0 --text))
               (--char (substring --match 0 1) )
               (--symb (cdr (assoc --char decorations)) )
               (--beginning (match-beginning 0))
               (--end  (setq --last (- (match-end 0) 2))))
          (setq --text
                (concat
                 (substring --text 0 --beginning)
                 (match-string 3 --text)
                 (substring --text (+ 2 --last))))
          (push (list --symb --beginning --end (format "font-style=\"%s\"" --symb)) --decs)))
      --decs))
   ((eq type 'text)
    (let ((regex-format "\\(\\([\\*/=_~]\\)\\([^\\2]+?\\)\\2\\)"))
      (replace-regexp-in-string regex-format "\\3" text nil nil 1)))))

(defun cxl--make-text (text)
  (cxl--make-text-decorations
   'text
   (cxl--make-text-translate-entities
    (cxl--make-text-autofill
     text cxl-fill-column))))

(defun cxl--make-text-autofill (text fill-column)
  "Fill text to given column if `org-cxlmap-do-fill'"
  (defun wrap (text fill-column)
    (let ((column fill-column)
          --text
          --line)
      (letrec ((--walk-tree
                (lambda (--data)
                  (let* ((--word (car --data)))
                    (cond
                     ((not --data))
                     ((< (length --word) column)
                      (push --word --line)
                      (setq column (- column (length --word) 1))
                      (funcall --walk-tree (cdr --data)))
                     (t (push --line --text)
                        (setq --line nil)
                        (push --word --line)
                        (setq column (- fill-column (length --word) 1))
                        (funcall --walk-tree (cdr --data))))))))
        (funcall --walk-tree (split-string text)))
      --text))
  (let ((n-match (s-count-matches "§" text)))
    (replace-regexp-in-string
     " +" " "
     (cond
      ((> n-match 1)
       (replace-regexp-in-string " *§ *" "\n" text))
      ((= n-match 1)
       (let* ((fill-column (length (car (s-split " *§ *" text)))))
         (s-word-wrap fill-column (replace-regexp-in-string " *§ *" " " text))))
      (t
       (s-word-wrap fill-column text))))))


(setq web-mode-html-entities
      '(("10" . 10)
        ("quot" . 34)
        ("amp" . 38)
     ("apos" . 39)
     ("lt" . 60)
     ("gt" . 62)
     ("nbsp" . 160)
     ("iexcl" . 161)
     ("cent" . 162)
     ("pound" . 163)
     ("curren" . 164)
     ("yen" . 165)
     ("brvbar" . 166)
     ("sect" . 167)
     ("uml" . 168)
     ("copy" . 169)
     ("ordf" . 170)
     ("laquo" . 171)
     ("not" . 172)
     ("shy" . 173)
     ("reg" . 174)
     ("macr" . 175)
     ("deg" . 176)
     ("plusmn" . 177)
     ("sup2" . 178)
     ("sup3" . 179)
     ("acute" . 180)
     ("micro" . 181)
     ("para" . 182)
     ("middot" . 183)
     ("cedil" . 184)
     ("sup1" . 185)
     ("ordm" . 186)
     ("raquo" . 187)
     ("frac14" . 188)
     ("frac12" . 189)
     ("frac34" . 190)
     ("iquest" . 191)
     ;; ("Agrave" . 192)
     ;; ("Aacute" . 193)
     ;; ("Acirc" . 194)
     ;; ("Atilde" . 195)
     ;; ("Auml" . 196)
     ;; ("Aring" . 197)
     ;; ("AElig" . 198)
     ;; ("Ccedil" . 199)
     ;; ("Egrave" . 200)
     ;; ("Eacute" . 201)
     ;; ("Ecirc" . 202)
     ;; ("Euml" . 203)
     ;; ("Igrave" . 204)
     ;; ("Iacute" . 205)
     ;; ("Icirc" . 206)
     ;; ("Iuml" . 207)
     ;; ("ETH" . 208)
     ;; ("Ntilde" . 209)
     ;; ("Ograve" . 210)
     ;; ("Oacute" . 211)
     ;; ("Ocirc" . 212)
     ;; ("Otilde" . 213)
     ;; ("Ouml" . 214)
     ;; ("times" . 215)
     ;; ("Oslash" . 216)
     ;; ("Ugrave" . 217)
     ;; ("Uacute" . 218)
     ;; ("Ucirc" . 219)
     ;; ("Uuml" . 220)
     ;; ("Yacute" . 221)
     ;; ("THORN" . 222)
     ;; ("szlig" . 223)
     ;; ("agrave" . 224)
     ;; ("aacute" . 225)
     ;; ("acirc" . 226)
     ;; ("atilde" . 227)
     ;; ("auml" . 228)
     ;; ("aring" . 229)
     ;; ("aelig" . 230)
     ;; ("ccedil" . 231)
     ;; ("egrave" . 232)
     ;; ("eacute" . 233)
     ;; ("ecirc" . 234)
     ;; ("euml" . 235)
     ;; ("igrave" . 236)
     ;; ("iacute" . 237)
     ;; ("icirc" . 238)
     ;; ("iuml" . 239)
     ;; ("eth" . 240)
     ;; ("ntilde" . 241)
     ;; ("ograve" . 242)
     ;; ("oacute" . 243)
     ;; ("ocirc" . 244)
     ;; ("otilde" . 245)
     ;; ("ouml" . 246)
     ;; ("divide" . 247)
     ;; ("oslash" . 248)
     ;; ("Ugrave" . 249)
     ;; ("Uacute" . 250)
     ;; ("Ucirc" . 251)
     ;; ("Uuml" . 252)
     ;; ("yacute" . 253)
     ;; ("thorn" . 254)
     ;; ("yuml" . 255)
     ;; ("OElig" . 338)
     ;; ("oelig" . 339)
     ;; ("Scaron" . 352)
     ;; ("scaron" . 353)
     ("Yuml" . 376)
     ("fnof" . 402)
     ("circ" . 710)
     ("tilde" . 732)
     ("Alpha" . 913)
     ("Beta" . 914)
     ("Gamma" . 915)
     ("Delta" . 916)
     ("Epsilon" . 917)
     ("Zeta" . 918)
     ("Eta" . 919)
     ("Theta" . 920)
     ("Iota" . 921)
     ("Kappa" . 922)
     ("Lambda" . 923)
     ("Mu" . 924)
     ("Nu" . 925)
     ("Xi" . 926)
     ("Omicron" . 927)
     ("Pi" . 928)
     ("Rho" . 929)
     ("Sigma" . 931)
     ("Tau" . 932)
     ("Upsilon" . 933)
     ("Phi" . 934)
     ("Chi" . 935)
     ("Psi" . 936)
     ("Omega" . 937)
     ("alpha" . 945)
     ("beta" . 946)
     ("gamma" . 947)
     ("delta" . 948)
     ("epsilon" . 949)
     ("zeta" . 950)
     ("eta" . 951)
     ("theta" . 952)
     ("iota" . 953)
     ("kappa" . 954)
     ("lambda" . 955)
     ("mu" . 956)
     ("nu" . 957)
     ("xi" . 958)
     ("omicron" . 959)
     ("pi" . 960)
     ("rho" . 961)
     ("sigmaf" . 962)
     ("sigma" . 963)
     ("tau" . 964)
     ("upsilon" . 965)
     ("phi" . 966)
     ("chi" . 967)
     ("psi" . 968)
     ("omega" . 969)
     ("thetasym" . 977)
     ("Upsih" . 978)
     ("piv" . 982)
     ("ensp" . 8194)
     ("emsp" . 8195)
     ("thinsp" . 8201)
     ("zwnj" . 8204)
     ("zwj" . 8205)
     ("lrm" . 8206)
     ("rlm" . 8207)
     ("ndash" . 8211)
     ("mdash" . 8212)
     ("lsquo" . 8216)
     ("rsquo" . 8217)
     ("sbquo" . 8218)
     ("ldquo" . 8220)
     ("rdquo" . 8221)
     ("bdquo" . 8222)
     ("dagger" . 8224)
     ("Dagger" . 8225)
     ("bull" . 8226)
     ("hellip" . 8230)
     ("permil" . 8240)
     ("prime" . 8242)
     ("Prime" . 8243)
     ("lsaquo" . 8249)
     ("rsaquo" . 8250)
     ("oline" . 8254)
     ("frasl" . 8260)
     ("euro" . 8364)
     ("image" . 8465)
     ("weierp" . 8472)
     ("real" . 8476)
     ("trade" . 8482)
     ("alefsym" . 8501)
     ("larr" . 8592)
     ("uarr" . 8593)
     ("rarr" . 8594)
     ("darr" . 8595)
     ("harr" . 8596)
     ("crarr" . 8629)
     ("lArr" . 8656)
     ("UArr" . 8657)
     ("rArr" . 8658)
     ("dArr" . 8659)
     ("hArr" . 8660)
     ("forall" . 8704)
     ("part" . 8706)
     ("exist" . 8707)
     ("empty" . 8709)
     ("nabla" . 8711)
     ("isin" . 8712)
     ("notin" . 8713)
     ("ni" . 8715)
     ("prod" . 8719)
     ("sum" . 8721)
     ("minus" . 8722)
     ("lowast" . 8727)
     ("radic" . 8730)
     ("prop" . 8733)
     ("infin" . 8734)
     ("ang" . 8736)
     ("and" . 8743)
     ("or" . 8744)
     ("cap" . 8745)
     ("cup" . 8746)
     ("int" . 8747)
     ("there4" . 8756)
     ("sim" . 8764)
     ("cong" . 8773)
     ("asymp" . 8776)
     ("ne" . 8800)
     ("equiv" . 8801)
     ("le" . 8804)
     ("ge" . 8805)
     ("sub" . 8834)
     ("sup" . 8835)
     ("nsub" . 8836)
     ("sube" . 8838)
     ("supe" . 8839)
     ("oplus" . 8853)
     ("otimes" . 8855)
     ("perp" . 8869)
     ("sdot" . 8901)
     ("lceil" . 8968)
     ("rceil" . 8969)
     ("lfloor" . 8970)
     ("rfloor" . 8971)
     ("lang" . 9001)
     ("rang" . 9002)
     ("loz" . 9674)
     ("spades" . 9824)
     ("clubs" . 9827)
     ("hearts" . 9829)
     ("diams" . 9830)))

(defun  cxl--make-text-translate-entities (text)
  (setq text (s-replace "&" "&amp;" text))
  (dolist (pair web-mode-html-entities)
    (unless (= (cdr pair) 38)
      (let* ((str (char-to-string (cdr pair))))
        (setq text
              (s-replace str
                         (concat "&#" (number-to-string (cdr pair)) ";")
                         text)))))
  (replace-regexp-in-string
   " +" " "
   (replace-regexp-in-string
    " *&#xa; *" "&#xa;"
    text)))

(defun cxl--node (id label text)
  (format "<concept id=\"%s\" label=\"%s\"/>\n"
          id (cxl--make-text text)))

(defun cxl--node-appearance (id label text)
  (apply 'concat (cl-loop for decoration in
                          (append (cxl--make-text-decorations 'decorations text)
                                  (cxl--make-text-decorations 'highlights  text))
                          collect
                          (let ((style (nth 3 decoration))
                                (begin (nth 1 decoration))
                                (end (nth 2 decoration)))
                            (format "<concept-appearance id=\"%s\"><localized-style begin=\"%d\" end=\"%d\" %s/></concept-appearance>\n" id begin end style)))))


(defun cxl--connection (id from to)
  (format "<connection id=\"%s\" from-id=\"%s\" to-id=\"%s\"/>\n" id from to))

(defun cxl--connection-appearance (id from to)
  (format "<connection-appearance id=\"%s\" from-pos=\"%s\" to-pos=\"%s\"/>\n" id "right" "left"))


(defun cxl--linking-phrase (id label text)
  (format "<linking-phrase id=\"%s\" label=\"%s\"/>\n"
          id (cxl--make-text text)))

(defun cxl--linking-phrase-appearance (id label text)

  (apply 'concat (cl-loop for decoration in
                          (append (cxl--make-text-decorations 'decorations text)
                                  (cxl--make-text-decorations 'highlights  text))
                          collect
                          (let ((style (nth 3 decoration))
                                (begin (nth 1 decoration))
                                (end (nth 2 decoration)))
                            (format "<linking-phrase-appearance id=\"%s\"><localized-style begin=\"%d\" end=\"%d\" %s/></linking-phrase-appearance>\n" id begin end style)))))

(defun cxl-select (tree types &optional ids side)
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
      (funcall --walk-tree tree)
      --acc)))



(defun eval-list (func-list &optional prefix postfix)
  (let ((prefix (if prefix prefix "cxl--"))
        (postfix (if postfix postfix ""))
        --acc)
    (letrec ((--walk-tree
              (lambda (--data)
                (let* ((--elem (car --data))
                       (--func (car --elem))
                       (--args (cdr --elem)))
                  (cond
                   ((not --data))
                   (t (progn
                        (push (apply (intern (concat prefix (symbol-name --func) postfix) ) --args ) --acc)
                        (funcall --walk-tree (cdr --data))
                        )))
                  ))))
      (funcall --walk-tree func-list)
      --acc)))

(defmacro cxl-part (part elements &optional prefix postfix)
  `(concat (format "<%s>\n" ,part )
           (apply 'concat (eval-list ,elements ,prefix ,postfix))
           (format "</%s>\n" ,part )))

(defun cxl--res-meta (info)
  (concat "<res-meta>\n"
          (apply 'concat
                 (cl-loop for meta in info collect
                          (let ((key (s-downcase (replace-regexp-in-string "_" ":" (car meta))))
                                (value (nth 1 meta)))
                            (format "<%s>%s</%s>\n" key value key)
                            )))
          "<dc:format>x-cmap/x-storable</dc:format>\n </res-meta>\n")
  )

(defun cxl--style-sheet-list ()
  (concat "<style-sheet-list>\n"
          "</style-sheet-list>\n"))

(defun cxl--extra-graphical-properties-list ()
  (concat "<extra-graphical-properties-list>\n"
          "</extra-graphical-properties-list>\n"))


(defun cxl--cmap (res-meta map)
  (concat
   "<cmap xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns=\"http://cmap.ihmc.us/xml/cmap/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:vcard=\"http://www.w3.org/2001/vcard-rdf/3.0#\">\n"
   res-meta
   map
   "</cmap>"
   )
  )

(defun cxl--map (map)
  (concat
   "\n<map>\n"
   map
   (cxl--style-sheet-list)
   (cxl--extra-graphical-properties-list)
   "\n</map>\n"))

(defun cxl--xml-heading ()
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")

(defun cxl-source (components res-meta)
  (let* ((nodes (cxl-select components 'node))
         (linking-phrases (cxl-select components 'linking-phrase))
         (connections (cxl-select components 'connection))
        (map
         (concat
          (cxl-part "concept-list" nodes) "\n"
          (cxl-part "linking-phrase-list" linking-phrases) "\n"
          (cxl-part "connection-list" connections) "\n"
          (cxl-part "concept-appearance-list" nodes "cxl--" "-appearance") "\n"
          (cxl-part "linking-phrase-appearance-list" linking-phrases "cxl--" "-appearance") "\n"
          (cxl-part "connection-appearance-list" connections "cxl--" "-appearance") "\n"
           )))
    (concat
     (cxl--xml-heading)
     (cxl--cmap
      (cxl--res-meta res-meta)
      (cxl--map map)))))


;; (message
;;  (cxl-make-from-components
;;   '((node "27f237e6b7f96587b6202ff3607ad88a" "1.0.N" "_NON_ *posso* scambiare termini che denotano lo stesso oggetto&#xa;*senza* =alterare= il valore di verit&agrave; (/contingenza/ -&#xa;possibilit/&agrave;)/")
;;     )))


;; (message
;;  (cxl-make-from-components
;;   '((node "27f237e6b7f96587b6202ff3607ad88a" "1.0.N" "_NON_ *posso* scambiare termini che denotano lo stesso oggetto&#xa;*senza* =alterare= il valore di verit&agrave; (/contingenza/ -&#xa;possibilit/&agrave;)/")
;;     (node "f1ca994244fab48429da22af0e839868" "6.1.N" "A1.1")
;;     (node "c9512565ef6194ca664dc41ec0de7a53" "35.2.N" "B1")
;;     (node "1a2ddc2db4693cfd16d534cde5572cc1" "55.3.N" "C1")
;;     (node "27326266c7129a217e06eb0fe6392398" "68.4.N" "C1.1")
;;     (connection "435d71d27cd8672649611d11d8cfb74A" "f1ca994244fab48429da22af0e839868" "435d71d27cd8672649611d11d8cfb744")
;;     (connection "435d71d27cd8672649611d11d8cfb74B" "435d71d27cd8672649611d11d8cfb744" "1a2ddc2db4693cfd16d534cde5572cc1")
;;     (connection "435d71d27cd8672649611d11d8cfb74C" "3ce5e8ca1c957ca22703011e60779b38" "f1ca994244fab48429da22af0e839868")
;;     (connection "435d71d27cd8672649611d11d8cfb74D" "27f237e6b7f96587b6202ff3607ad88a" "3ce5e8ca1c957ca22703011e60779b38")
;;     (connection "435d71d27cd8672649611d11d8cfb74E" "c9512565ef6194ca664dc41ec0de7a53" "321a095168f37403127b8117f0620142")
;;     (connection "435d71d27cd8672649611d11d8cfb74F" "321a095168f37403127b8117f0620142" "05ae1f06f9cdd5ee9832244ef2ec2465")
;;     (connection "435d71d27cd8672649611d11d8cfb740" "5048e00a028b6e2a69de2c26e69ffa47" "1a2ddc2db4693cfd16d534cde5572cc1")
;;     (connection "435d71d27cd8672649611d11d8cfb741" "1a2ddc2db4693cfd16d534cde5572cc1" "27326266c7129a217e06eb0fe6392398")
;;     (linking-phrase "435d71d27cd8672649611d11d8cfb744" "6.1.E" "")
;;     (linking-phrase "3ce5e8ca1c957ca22703011e60779b38" "6.1.S" "LA1.1.S")
;;     (linking-phrase "321a095168f37403127b8117f0620142" "35.2.E" "")
;;     (linking-phrase "5048e00a028b6e2a69de2c26e69ffa47" "55.3.S" "C1.E"))))

(provide 'cxl)
;;; cxl.el ends here


;; (cxl-node "27f237e6b7f96587b6202ff3607ad88a" "1.0.N" "A1")

;; (node "27f237e6b7f96587b6202ff3607ad88a" "1.0.N" "*NON* posso scambiare §termini che denotano lo stesso oggetto senza alterare il valore di verità (contingenza - possibilità)")

;; (node "27f237e6b7f96587b6202ff3607ad88a" "1.0.N" "*NON* posso scambiare termini che denotano lo stesso oggetto senza alterare il valore di verità (contingenza - possibilità)")

;; (cxl--make-text-autofill "*NON* posso scambiare §termini che denotano lo stesso oggetto senza alterare il valore di verità (contingenza - possibilità)" 60)


;;(cxl--make-text-decorations 'decorations "_NON_ *posso* scambiare termini che denotano lo stesso oggetto&#xa;*senza* =alterare= il valore di verit&agrave; (/contingenza/ -&#xa;possibilit/&agrave;)/")
