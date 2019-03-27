;;; torus.el --- A buffer groups manager             -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Chimay

;; Author : Chimay
;; Name: Torus
;; Package-Version: 1.10
;; Package-requires: ((emacs "26"))
;; Keywords: files, buffers, groups, persistent, history, layout, tabs
;; URL: https://github.com/chimay/torus

;;; Commentary:

;; If you ever dreamed about creating and switching buffer groups at will
;; in Emacs, Torus is the tool you want.
;;
;; In short, this plugin let you organize your buffers by creating as
;; many buffer groups as you need, add the files you want to it and
;; quickly navigate between :
;;
;;   - Buffers of the same group
;;   - Buffer groups
;;   - Workspaces, ie sets of buffer groups
;;
;; Note that :
;;
;;   - A location is a pair (buffer (or filename) . position)
;;   - A buffer group, in fact a location group, is called a circle
;;   - A set of buffer groups is called a torus (a circle of circles)
;;
;; Original idea by Stefan Kamphausen, see https://www.skamphausen.de/cgi-bin/ska/mtorus
;;
;; See https://github.com/chimay/torus/blob/master/README.org for more details

;;; License:
;;; ----------------------------------------------------------------------

;; This file is not part of Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Credits:
;;; ----------------------------------------------------------------------

;; Stefan Kamphausen, https://www.skamphausen.de/cgi-bin/ska/mtorus
;; Sebastian Freundt, https://sourceforge.net/projects/mtorus.berlios/

;;; Structure:
;;; ----------------------------------------------------------------------

;;                     root
;;                   +---+---+
;;                   |   | X |
;;                   +-+-+---+
;;                     |
;;                     |
;;                   tree
;; +---------+---------+-------+---------+
;; | torus 1 | torus 2 | ...   | torus M |
;; +---------+----+----+-------+---------+
;;                |
;;                |
;;          +-----+--------+
;;          | "torus name" |
;;          +--------------+         +---------------+
;;          |  cercle 1    +---------+ "circle name" |
;;          +--------------+         +---------------+   .
;;          |  cercle 2    |         |  location 1   |
;;          +--------------+         +---------------+       +------+----------+
;;          |  ...         |         |  location 2   +-------+ file | position |
;;          +--------------+         +---------------+       +------+----------+
;;          |  cercle N    |         |  ...          |
;;          +--------------+         +---------------+
;;                                   |  location P   |
;;                                   +---------------+

;;; Code:
;;; ----------------------------------------------------------------------

;;; Requires
;;; ------------------------------------------------------------

(eval-when-compile
  (require 'duo)
  (require 'cl-lib)
  (require 'cl-extra)
  (require 'seq)
  (require 'subr-x))

(declare-function cl-copy-seq "cl-lib")

(declare-function cl-subseq "cl-extra")

(declare-function cl-position "cl-lib")
(declare-function cl-find "cl-lib")
(declare-function cl-remove "cl-lib")

(declare-function seq-intersection "seq")
(declare-function seq-filter "seq")
(declare-function seq-group-by "seq")

(declare-function string-join "subr-x")

;;; Custom
;;; ------------------------------------------------------------

(defgroup ttorus nil
  "An interface to navigating groups of buffers."
  :tag "ttorus"
  :link '(url-link :tag "Home Page"
                   "https://github.com/chimay/ttorus")
  :link '(emacs-commentary-link
                  :tag "Commentary in ttorus.el" "ttorus.el")
  :prefix "ttorus-"
  :group 'environment
  :group 'extensions
  :group 'convenience)

(defcustom ttorus-prefix-key "s-t"
  "Prefix key for the ttorus key mappings.
Will be processed by `kbd'."
  :type 'string
  :group 'ttorus)

(defcustom ttorus-binding-level 1
  "Whether to activate optional keybindings."
  :type 'integer
  :group 'ttorus)

(defcustom ttorus-verbosity 1
  "Level of verbosity.
1 = normal
2 = light debug
3 = heavy debug."
  :type 'integer
  :group 'ttorus)

(defcustom ttorus-dirname user-emacs-directory
  "The directory where the ttorus are read and written."
  :type 'string
  :group 'ttorus)

(defcustom ttorus-load-on-startup nil
  "Whether to load ttorus on startup of Emacs."
  :type 'boolean
  :group 'ttorus)

(defcustom ttorus-save-on-exit nil
  "Whether to save ttorus on exit of Emacs."
  :type 'boolean
  :group 'ttorus)

(defcustom ttorus-autoread-file nil
  "The file to load on startup when `ttorus-load-on-startup' is t."
  :type 'string
  :group 'ttorus)

(defcustom ttorus-autowrite-file nil
  "The file to write before quitting Emacs when `ttorus-save-on-exit' is t."
  :type 'string
  :group 'ttorus)

(defcustom ttorus-backup-number 3
  "Number of backups of ttorus files."
  :type 'integer
  :group 'ttorus)

(defcustom ttorus-maximum-history-elements 50
  "Maximum number of elements in history variables.
See `ttorus-history' and `torus-minibuffer-history'."
  :type 'integer
  :group 'ttorus)

(defcustom ttorus-maximum-horizontal-split 3
  "Maximum number of horizontal split, see `ttorus-split-horizontally'."
  :type 'integer
  :group 'ttorus)

(defcustom ttorus-maximum-vertical-split 4
  "Maximum number of vertical split, see `ttorus-split-vertically'."
  :type 'integer
  :group 'ttorus)

(defcustom ttorus-display-tab-bar nil
  "Whether to display a tab bar in `header-line-format'."
  :type 'boolean
  :group 'ttorus)

(defcustom ttorus-separator-torus-circle " >> "
  "String between ttorus and circle in the dashboard."
  :type 'string
  :group 'ttorus)

(defcustom ttorus-separator-circle-location " > "
  "String between circle and location(s) in the dashboard."
  :type 'string
  :group 'ttorus)

(defcustom ttorus-location-separator " | "
  "String between location(s) in the dashboard."
  :type 'string
  :group 'ttorus)

(defcustom ttorus-prefix-separator "/"
  "String between the prefix and the circle names.
The name of the new circles will be of the form :
\"User_input_prefix `ttorus-prefix-separator' Name_of_the_added_circle\"
without the spaces. If the user enter a blank prefix,
the added circle names remain untouched."
  :type 'string
  :group 'ttorus)

(defcustom ttorus-join-separator " & "
  "String between the names when joining.
The name of the new object will be of the form :
\"Object-1 `ttorus-join-separator' Object-2\"
without the spaces."
  :type 'string
  :group 'ttorus)

;;; Variables
;;; ------------------------------------------------------------

(defvar torus-tree nil
  "The tree is a list of ttoruses.
Each ttorus has a name and a list of circles :
\(\"ttorus name\" . list-of-circles)
Each circle has a name and a list of locations :
\(\"circle name\" . list-of-locations)
Each location contains a filename and a position :
\(filename . position)")

(defvar torus-root (list torus-tree)
  "The root is a reference to the tree.
More precisely, it’s a cons whose car is `torus-tree'.")

(defvar ttorus-index nil
  "Alist containing locations and where to find them.
Each element has the form :
\((ttorus . circle) . (file . position))")

(defvar ttorus-history nil
  "Alist containing history of locations in all ttoruses.
Each element is of the form :
\((ttorus . circle) . (file . position))")

(defvar torus-minibuffer-history nil
  "History of user input in minibuffer.")

(defvar ttorus-layout nil
  "List containing split layout of all circles in all ttoruses.
Each element is of the form:
\((\"ttorus name\" . \"circle name\") . layout)
The layout is stored as a character code :
?m manual
?o one window
?h horizontal
?v vertical
?g grid
main window on
  ?l left
  ?r right
  ?t top
  ?b bottom")

(defvar ttorus-line-col nil
  "Alist storing locations and corresponding lines & columns in files.
Each element is of the form :
\((file . position) . (line . column))
Allows to display lines & columns.")

;;; Current
;;; ------------------------------

;; Reference to cons of current objects

(defvar torus-current-torus nil
  "Cons of current torus in `torus-tree'.")

(defvar torus-current-circle nil
  "Cons of current circle in `torus-current-torus'.")

(defvar torus-current-location nil
  "Cons of current location in `torus-current-circle'.")

(defvar torus-current-index nil
  "Cons of current entry in `torus-index'.")

(defvar torus-current-history nil
  "Cons of current entry in `torus-history'.")

;;; Last
;;; ------------------------------

(defvar torus-last-torus nil
  "Last torus in `torus-tree'.")

(defvar torus-last-circle nil
  "Last circle in `torus-current-torus'.")

(defvar torus-last-location nil
  "Last location in `torus-current-circle'.")

;;; Transient
;;; ------------------------------

(defvar ttorus-markers nil
  "Alist containing markers to opened files.
Each element is of the form :
\((file . position) . marker)
Contain only the files opened in buffers.")

(defvar ttorus-original-header-lines nil
  "Alist containing original header lines, before ttorus changed it.
Each element is of the form :
\(buffer . original-header-line)")

;;; Files
;;; ------------------------------

(defvar ttorus-file-extension ".el"
  "Extension of ttorus files.")

;;; Prompts
;;; ------------------------------

;;; Empty
;;; ---------------

(defvar ttorus--message-empty-tree
  "Torus Tree is empty. Please add a location with torus-add-location.")

(defvar ttorus--message-empty-torus
  "Torus %s is empty. Please add a location with torus-add-location.")

(defvar ttorus--message-empty-circle
  "Circle %s in Torus %s is empty. Please add a location with torus-add-location.")

;;; Menus
;;; ---------------

(defvar ttorus--message-reset-choice
  "Reset [a] all [3] tree [i] index [h] history [m] minibuffer history [l] layout\n\
      [p] line & col [C-m] markers [o] orig header line")

(defvar ttorus--message-print-choice
  "Print [a] all [3] tree [i] index [h] history [m] minibuffer history [l] layout\n\
      [p] line & col [C-m] markers [C-o] orig header line")

(defvar ttorus--message-alternate-choice
  "Alternate [m] in meta ttorus [t] in ttorus [c] in circle [T] ttoruses [C] circles")

(defvar ttorus--message-reverse-choice
  "Reverse [l] locations [c] circle [d] deep : locations & circles")

(defvar ttorus--message-autogroup-choice
  "Autogroup by [p] path [d] directory [e] extension")

(defvar ttorus--message-batch-choice
  "Run on circle files [e] Elisp code [c] Elisp command \n\
                    [!] Shell command [&] Async Shell command")

(defvar ttorus--message-layout-choice
  "Layout [m] manual [o] one window [h] horizontal [v] vertical [g] grid\n\
       main window on [l] left [r] right [t] top [b] bottom")

;;; Miscellaneous
;;; ---------------

(defvar ttorus--message-file-does-not-exist
  "File %s does not exist anymore. It will be removed from the ttorus.")

(defvar ttorus--message-existent-location
  "Location %s already exists in circle %s")

(defvar ttorus--message-prefix-circle
  "Prefix for the circle of torus %s (leave blank for none) ? ")

(defvar ttorus--message-circle-name-collision
  "Circle name collision. Please add/adjust prefixes to avoid confusion.")

(defvar ttorus--message-replace-torus
  "This will replace the current torus variables. Continue ? ")

;;; Keymaps & Mouse maps
;;; ------------------------------------------------------------

(defvar ttorus-map)

(define-prefix-command 'ttorus-map)

(defvar ttorus-map-mouse-torus (make-sparse-keymap))
(defvar ttorus-map-mouse-circle (make-sparse-keymap))
(defvar ttorus-map-mouse-location (make-sparse-keymap))

;;; Toolbox
;;; ------------------------------------------------------------

;;; Assoc
;;; ------------------------------

(defsubst ttorus--assoc-delete-all (key alist)
  "Remove all elements with key matching KEY in ALIST."
  (cl-remove key alist :test 'equal :key 'car))

(when (fboundp 'assoc-delete-all)
  (defalias 'ttorus--assoc-delete-all 'assoc-delete-all))

(defsubst ttorus--reverse-assoc-delete-all (value alist)
  "Remove all elements with value matching VALUE in ALIST."
  (cl-remove value alist :test 'equal :key 'cdr))

;;; Strings
;;; ------------------------------

(defun ttorus--eval-string (string)
  "Eval Elisp code in STRING."
  (eval (car (read-from-string (format "(progn %s)" string)))))

;;; Files
;;; ------------------------------

(defun ttorus--directory (object)
  "Return the last directory component of OBJECT."
  (let* ((filename (pcase object
                     (`(,(and (pred stringp) one) . ,(pred integerp)) one)
                     ((pred stringp) object)))
         (grandpa (file-name-directory (directory-file-name
                                        (file-name-directory
                                         (directory-file-name filename)))))
         (relative (file-relative-name filename grandpa)))
    (directory-file-name (file-name-directory relative))))

(defun ttorus--extension-description (object)
  "Return the extension description of OBJECT."
  (let* ((filename (pcase object
                     (`(,(and (pred stringp) one) . ,(pred integerp)) one)
                     ((pred stringp) object)))
         (extension (file-name-extension filename)))
    (when (> ttorus-verbosity 1)
      (message "filename extension : %s %s" filename extension))
    (pcase extension
      ('nil "Nil")
      ('"" "Ends with a dot")
      ('"sh" "Shell POSIX")
      ('"zsh" "Shell Zsh")
      ('"bash" "Shell Bash")
      ('"org" "Org mode")
      ('"el" "Emacs Lisp")
      ('"vim" "Vim Script")
      ('"py" "Python")
      ('"rb" "Ruby")
      (_ extension))))

;;; Private Functions
;;; ------------------------------------------------------------

;;; Predicates
;;; ------------------------------

(defsubst ttorus--empty-tree-p ()
  "Whether `torus-tree' is empty.
It’s empty when nil."
  (not torus-tree))

(defsubst ttorus--empty-torus-p ()
  "Whether current ttorus is empty.
It’s empty when nil or just a name in car
but no circle in it."
  (not (cdr (car torus-current-torus))))

(defsubst ttorus--empty-circle-p ()
  "Whether current circle is empty.
It’s empty when nil or just a name in car
but no location in it."
  (not (cdr (car torus-current-circle))))

(defun ttorus--inside-p (&optional buffer)
  "Whether BUFFER belongs to the ttorus.
Argument BUFFER nil means use current buffer."
  (let ((filename (buffer-file-name  (if buffer
                                         buffer
                                       (current-buffer))))
        (locations (append (mapcar 'cdr ttorus-index))))
    (member filename locations)))

(defun ttorus--equal-concise-p (one two)
  "Whether the concise representations of ONE and TWO are equal."
  (equal (ttorus--concise one)
         (ttorus--concise two)))

;;; Informations
;;; ------------------------------

(defsubst torus--current-torus-name ()
  "Return current torus name."
  (car (car torus-current-torus)))

(defsubst torus--current-circle-name ()
  "Return current torus name."
  (car (car torus-current-circle)))

;;; Tables
;;; ------------------------------

(defun ttorus--build-index (&optional tree)
  "Return index built from TREE.
Argument TREE nil means build index of `torus-tree'"
  (let ((meta (if tree
                  tree
                torus-tree))
        (ttorus-name)
        (circle-name)
        (ttorus-circle)
        (index)
        (entry))
    (dolist (ttorus meta)
      (setq ttorus-name (car ttorus))
      (dolist (circle (cdr ttorus))
        (setq circle-name (car circle))
        (setq ttorus-circle (cons ttorus-name circle-name))
        (dolist (location (cdr circle))
          (when (> ttorus-verbosity 2)
            (message "Table entry %s" entry))
          (setq entry (cons ttorus-circle location))
          (unless (member entry index)
            (push entry index)))))
    (setq index (reverse index))))

(defun ttorus--narrow-to-torus (&optional ttorus-name index)
  "Narrow an index-like table to entries of TTORUS-NAME.
Argument TTORUS-NAME nil means narrow to current ttorus.
Argument INDEX nil means using `ttorus-index'.
Can be used with `ttorus-index' and `ttorus-history'."
  (let ((index (if index
                   index
                 ttorus-index))
        (ttorus-name (if ttorus-name
                        ttorus-name
                      (car (car torus-current-torus)))))
    (seq-filter (lambda (elem) (equal (caar elem) ttorus-name))
                index)))

(defun ttorus--narrow-to-circle (&optional ttorus-name circle-name index)
  "Narrow an index-like table to entries of TTORUS-NAME and CIRCLE-NAME.
Argument TTORUS-NAME nil means narrow using current ttorus.
Argument CIRCLE-NAME nil means narrow to current circle.
Argument INDEX nil means using `ttorus-index'.
Can be used with `ttorus-index' and `ttorus-history'."
  (let ((index (if index
                   index
                 ttorus-index))
        (ttorus-name (if ttorus-name
                        ttorus-name
                      (car (car torus-current-torus))))
        (circle-name (if circle-name
                         circle-name
                       (car (car torus-current-circle)))))
    (seq-filter (lambda (elem) (and (equal (caar elem) ttorus-name)
                               (equal (cdar elem) circle-name)))
                index)))

(defun ttorus--complete-and-clean-layout ()
  "Fill `ttorus-layout' from missing elements. Delete useless ones."
  (let ((paths (mapcar #'car ttorus-index)))
    (delete-dups paths)
    (dolist (elem paths)
      (unless (assoc elem ttorus-layout)
        (push (cons elem ?m) ttorus-layout)))
    (dolist (elem ttorus-layout)
      (unless (member (car elem) paths)
        (setq ttorus-layout (ttorus--assoc-delete-all (car elem) ttorus-layout))))
    (setq ttorus-layout (reverse ttorus-layout))))

(defun ttorus--apply-or-push-layout ()
  "Apply layout of current circle, or add default is not present."
  (let* ((path (cons (car (car torus-current-torus))
                     (car (car torus-current-circle))))
         (entry (assoc path ttorus-layout)))
    (if entry
        (ttorus-layout-menu (cdr entry))
      (push (cons path ?m) ttorus-layout))))

;;; Updates
;;; ------------------------------

(defun ttorus--update-position ()
  "Update position in current location.
Do nothing if file does not match current buffer."
  (unless (ttorus--empty-circle-p)
    (let* ((ttorus-circle (cons (car torus-current-torus)
                               (car torus-current-circle)))
           (old-location (torus-current-location))
           (old-entry torus-current-index)
           (old-here (cdr old-location))
           (file (car old-location))
           (here (point))
           (marker (point-marker))
           (line-col (cons (line-number-at-pos) (current-column)))
           (new-location (cons file here))
           (new-entry (cons ttorus-circle new-location))
           (new-location-line-col (cons new-location line-col))
           (new-location-marker (cons new-location marker)))
      (when (> ttorus-verbosity 2)
        (message "Update position -->")
        (message "here old : %s %s" here old-here)
        (message "old-location : %s" old-location)
        (message "loc history : %s" (caar ttorus-old-history))
        (message "assoc index : %s" (assoc old-location ttorus-table)))
      (when (and (equal file (buffer-file-name (current-buffer)))
                 (equal old-entry (car ttorus-history))
                 (not (equal here old-here)))
        (when (> ttorus-verbosity 2)
          (message "Old location : %s" old-location)
          (message "New location : %s" new-location))
        (setcar (cdr (cadr torus-current-torus)) new-location)
        (if (member old-entry ttorus-index)
            (setcar (member old-entry ttorus-index) new-entry)
          (setq ttorus-index (ttorus--build-index)))
        (if (member old-entry ttorus-history)
            (setcar (member old-entry ttorus-history)
                    new-entry)
          (ttorus--update-meta-history))
        (if (assoc old-location ttorus-line-col)
            (progn
              (setcdr (assoc old-location ttorus-line-col) line-col)
              (setcar (assoc old-location ttorus-line-col) new-location))
          (push new-location-line-col ttorus-line-col))
        (if (assoc old-location ttorus-markers)
            (progn
              (setcdr (assoc old-location ttorus-markers) marker)
              (setcar (assoc old-location ttorus-markers) new-location))
          (push new-location-marker ttorus-markers))))))

(defun ttorus--jump ()
  "Jump to current location (buffer & position) in ttorus.
Add the location to `ttorus-markers' if not already present."
  (when (and torus-current-torus
             (listp torus-current-torus)
             (car torus-current-torus)
             (listp (car torus-current-torus))
             (> (length (car torus-current-torus)) 1))
    (let* ((location (car (cdr (car torus-current-torus))))
           (circle-name (caar torus-current-torus))
           (ttorus-name (caar ttorus-meta))
           (circle-torus (cons circle-name ttorus-name))
           (location-circle (cons location circle-name))
           (location-circle-torus (cons location circle-torus))
           (file (car location))
           (position (cdr location))
           (bookmark (cdr (assoc location ttorus-markers)))
           (buffer (when bookmark
                     (marker-buffer bookmark))))
      (if (and bookmark buffer (buffer-live-p buffer))
          (progn
            (when (> ttorus-verbosity 2)
              (message "Found %s in markers" bookmark))
            (when (not (equal buffer (current-buffer)))
              (switch-to-buffer buffer))
            (goto-char bookmark))
        (when (> ttorus-verbosity 2)
          (message "Found %s in ttorus" location))
        (when bookmark
          (setq ttorus-markers (ttorus--assoc-delete-all location ttorus-markers)))
        (if (file-exists-p file)
            (progn
              (when (> ttorus-verbosity 1)
                (message "Opening file %s at %s" file position))
              (find-file file)
              (goto-char position)
              (push (cons location (point-marker)) ttorus-markers))
          (message (format ttorus--message-file-does-not-exist file))
          (setcdr (car torus-current-torus) (cl-remove location (cdr (car torus-current-torus))))
          (setq ttorus-line-col (ttorus--assoc-delete-all location ttorus-line-col))
          (setq ttorus-markers (ttorus--assoc-delete-all location ttorus-markers))
          (setq ttorus-table (cl-remove location-circle ttorus-table))
          (setq ttorus-index (cl-remove location-circle-torus ttorus-index))
          (setq ttorus-old-history (cl-remove location-circle ttorus-old-history))
          (setq ttorus-history (cl-remove location-circle-torus ttorus-history))))
      (ttorus--update-history)
      (ttorus--update-meta-history)
      (ttorus--tab-bar))
    (recenter)))

;;; Switch
;;; ------------------------------

(defun ttorus--switch (location-circle)
  "Jump to circle and location countained in LOCATION-CIRCLE."
  (unless (and location-circle
               (consp location-circle)
               (consp (car location-circle)))
    (error "Function ttorus--switch : wrong type argument"))
  (ttorus--update-position)
  (let* ((circle-name (cdr location-circle))
         (circle (assoc circle-name torus-current-torus))
         (index (cl-position circle torus-current-torus :test #'equal))
         (before (cl-subseq torus-current-torus 0 index))
         (after (cl-subseq torus-current-torus index)))
    (if index
        (setq torus-current-torus (append after before))
      (message "Circle not found.")))
  (let* ((circle (cdr (car torus-current-torus)))
         (location (car location-circle))
         (index (cl-position location circle :test #'equal))
         (before (cl-subseq circle 0 index))
         (after (cl-subseq circle index)))
    (if index
        (setcdr (car torus-current-torus) (append after before))
      (message "Location not found.")))
  (ttorus--jump)
  (ttorus--apply-or-push-layout))

(defun ttorus--meta-switch (entry)
  "Jump to ttorus, circle and location countained in ENTRY."
  (unless (and entry
               (consp entry)
               (consp (car entry))
               (consp (cdr entry)))
    (error "Function ttorus--switch : wrong type argument"))
  (when (> ttorus-verbosity 2)
    (message "meta switch entry : %s" entry))
  (ttorus--update-meta)
  (let* ((ttorus-name (caar entry))
         (ttorus (assoc ttorus-name ttorus-meta))
         (index (cl-position ttorus ttorus-meta :test #'equal))
         (before (cl-subseq ttorus-meta 0 index))
         (after (cl-subseq ttorus-meta index)))
    (if index
        (setq ttorus-meta (append after before))
      (message "ttorus not found.")))
  (ttorus--update-from-meta)
  (ttorus--build-table)
  (setq ttorus-index (ttorus--build-index))
  (ttorus--complete-and-clean-layout)
  (let* ((circle-name (cdar entry))
         (circle (assoc circle-name torus-current-torus))
         (index (cl-position circle torus-current-torus :test #'equal))
         (before (cl-subseq torus-current-torus 0 index))
         (after (cl-subseq torus-current-torus index)))
    (if index
        (setq torus-current-torus (append after before))
      (message "Circle not found.")))
  (let* ((circle (cdr (car torus-current-torus)))
         (location (cdr entry))
         (index (cl-position location circle :test #'equal))
         (before (cl-subseq circle 0 index))
         (after (cl-subseq circle index)))
    (if index
        (setcdr (car torus-current-torus) (append after before))
      (message "Location not found.")))
  (ttorus--jump)
  (ttorus--apply-or-push-layout))

;;; Modifications
;;; ------------------------------

(defun ttorus--prefix-circles (prefix ttorus-name)
  "Return vars of TTORUS-NAME with PREFIX to the circle names."
  (unless (and (stringp prefix) (stringp ttorus-name))
    (error "Function ttorus--prefix-circles : wrong type argument"))
  (let* ((entry (cdr (assoc ttorus-name ttorus-meta)))
         (ttorus (copy-tree (cdr (assoc "ttorus" entry))))
         (history (copy-tree (cdr (assoc "history" entry)))))
    (if (> (length prefix) 0)
        (progn
          (message "Prefix is %s" prefix)
          (dolist (elem ttorus)
            (setcar elem
                    (concat prefix ttorus-prefix-separator (car elem))))
          (dolist (elem history)
            (setcdr elem
                    (concat prefix ttorus-prefix-separator (cdr elem)))))
      (message "Prefix is blank"))
    (list ttorus history)))

;;; Windows
;;; ------------------------------

(defsubst ttorus--windows ()
  "Windows displaying a ttorus buffer."
  (seq-filter (lambda (elem) (ttorus--inside-p (window-buffer elem)))
              (window-list)))

(defun ttorus--main-windows ()
  "Return main window of layout."
  (let* ((windows (ttorus--windows))
         (columns (mapcar #'window-text-width windows))
         (max-columns (when columns
                    (eval `(max ,@columns))))
         (widest)
         (lines)
         (max-lines)
         (biggest))
    (when windows
      (dolist (index (number-sequence 0 (1- (length windows))))
        (when (equal (nth index columns) max-columns)
          (push (nth index windows) widest)))
      (setq lines (mapcar #'window-text-height widest))
      (setq max-lines (eval `(max ,@lines)))
      (dolist (index (number-sequence 0 (1- (length widest))))
        (when (equal (nth index lines) max-lines)
          (push (nth index widest) biggest)))
      (when (> ttorus-verbosity 2)
        (message "toruw windows : %s" windows)
        (message "columns : %s" columns)
        (message "max-columns : %s" max-columns)
        (message "widest : %s" widest)
        (message "lines : %s" lines)
        (message "max-line : %s" max-lines)
        (message "biggest : %s" biggest))
      biggest)))

(defun ttorus--prefix-argument-split (prefix)
  "Handle prefix argument PREFIX. Used to split."
  (pcase prefix
   ('(4)
    (split-window-below)
    (other-window 1))
   ('(16)
    (split-window-right)
    (other-window 1))))

;;; Strings
;;; ------------------------------

(defun ttorus--buffer-or-filename (location)
  "Return buffer name of LOCATION if existent in `ttorus-markers', file basename otherwise."
  (unless (consp location)
    (error "Function ttorus--buffer-or-filename : wrong type argument"))
  (let* ((bookmark (cdr (assoc location ttorus-markers)))
         (buffer (when bookmark
                   (marker-buffer bookmark))))
    (if buffer
        (buffer-name buffer)
      (file-name-nondirectory (car location)))))

(defun ttorus--position (location)
  "Return position in LOCATION in raw format or in line & column if available.
Line & Columns are stored in `ttorus-line-col'."
  (let ((entry (assoc location ttorus-line-col)))
    (if entry
        (format " at line %s col %s" (cadr entry) (cddr entry))
      (format " at position %s" (cdr location)))))

(defun ttorus--concise (object)
  "Return OBJECT in concise string format.
If OBJECT is a string : simply returns OBJECT.
If OBJECT is :
  \(File . Position) : returns \"File at Position\"
  \(Circle . (File . Pos)) -> \"Circle > File at Pos\"
  \((ttorus . Circle) . (File . Pos)) -> \"ttorus >> Circle > File at Pos\"
  \((File . Pos) . Circle) -> \"Circle > File at Pos\"
  \((File . Pos) . (Circle . ttorus)) -> \"ttorus >> Circle > File at Pos\""
  (let ((location))
    (pcase object
      (`((,(and (pred stringp) ttorus) . ,(and (pred stringp) circle)) .
         (,(and (pred stringp) file) . ,(and (pred integerp) position)))
       (setq location (cons file position))
       (concat ttorus
               ttorus-separator-torus-circle
               circle
               ttorus-separator-circle-location
               (ttorus--buffer-or-filename location)
               (ttorus--position location)))
      (`(,(and (pred stringp) circle) .
         (,(and (pred stringp) file) . ,(and (pred integerp) position)))
       (setq location (cons file position))
       (concat circle
               ttorus-separator-circle-location
               (ttorus--buffer-or-filename location)
               (ttorus--position location)))
      (`((,(and (pred stringp) file) . ,(and (pred integerp) position)) .
         (,(and (pred stringp) circle) . ,(and (pred stringp) ttorus)))
       (setq location (cons file position))
       (concat ttorus
               ttorus-separator-torus-circle
               circle
               ttorus-separator-circle-location
               (ttorus--buffer-or-filename location)
               (ttorus--position location)))
      (`((,(and (pred stringp) file) . ,(and (pred integerp) position)) .
         ,(and (pred stringp) circle))
       (setq location (cons file position))
       (concat circle
               ttorus-separator-circle-location
               (ttorus--buffer-or-filename location)
               (ttorus--position location)))
      (`(,(and (pred stringp) file) . ,(and (pred integerp) position))
       (setq location (cons file position))
       (concat (ttorus--buffer-or-filename location)
               (ttorus--position location)))
      ((pred stringp) object)
      (_ (error "Function ttorus--concise : wrong type argument")))))

(defun ttorus--needle (location)
  "Return LOCATION in short string format.
Shorter than concise. Used for dashboard and tabs."
  (unless (consp location)
    (error "Function ttorus--needle : wrong type argument"))
  (let* ((entry (assoc location ttorus-line-col))
         (position (if entry
                       (format " : %s" (cadr entry))
                     (format " . %s" (cdr location)))))
    (if (equal location (cadar torus-current-torus))
        (concat "[ "
                (ttorus--buffer-or-filename location)
                position
                " ]")
      (concat (ttorus--buffer-or-filename location)
              position))))

(defun ttorus--dashboard ()
  "Display summary of current ttorus, circle and location."
  (if ttorus-meta
      (if (> (length (car torus-current-torus)) 1)
          (let*
              ((locations (string-join (mapcar #'ttorus--needle
                                               (cdar torus-current-torus)) " | ")))
            (format (concat " %s"
                            ttorus-separator-torus-circle
                            "%s"
                            ttorus-separator-circle-location
                            "%s")
                     (caar ttorus-meta)
                     (caar torus-current-torus)
                     locations))
        (message ttorus--message-empty-circle (car (car torus-current-torus))))
    (message ttorus--message-empty-tree)))

;;; Files
;;; ------------------------------

(defun ttorus--roll-backups (filename)
  "Roll backups of FILENAME."
  (unless (stringp filename)
    (error "Function ttorus--roll-backups : wrong type argument"))
  (let ((file-list (list filename))
        (file-src)
        (file-dest))
    (dolist (iter (number-sequence 1 ttorus-backup-number))
      (push (concat filename "." (prin1-to-string iter)) file-list))
    (while (> (length file-list) 1)
      (setq file-dest (pop file-list))
      (setq file-src (car file-list))
      (when (> ttorus-verbosity 2)
        (message "files %s %s" file-src file-dest))
      (when (and file-src (file-exists-p file-src))
        (when (> ttorus-verbosity 2)
          (message "copy %s -> %s" file-src file-dest))
        (copy-file file-src file-dest t)))))

;;; Tab bar
;;; ------------------------------

(defun ttorus--eval-tab ()
  "Build tab bar."
  (when ttorus-meta
      (let*
          ((locations (mapcar #'ttorus--needle (cdar torus-current-torus)))
           (tab-string))
        (setq tab-string
              (propertize (format (concat " %s"
                                          ttorus-separator-torus-circle)
                                  (caar ttorus-meta))
                          'keymap ttorus-map-mouse-torus))
        (setq tab-string
              (concat tab-string
                      (propertize (format (concat "%s"
                                                  ttorus-separator-circle-location)
                                          (caar torus-current-torus))
                                  'keymap ttorus-map-mouse-circle)))
        (dolist (filepos locations)
          (setq tab-string
                (concat tab-string (propertize filepos
                                               'keymap ttorus-map-mouse-location)))
          (setq tab-string (concat tab-string ttorus-location-separator)))
        tab-string)))

(defun ttorus--tab-bar ()
  "Display tab bar."
  (let* ((main-windows (ttorus--main-windows))
         (current-window (selected-window))
         (buffer (current-buffer))
         (original (assoc buffer ttorus-original-header-lines))
         (eval-tab '(:eval (ttorus--eval-tab))))
    (when (> ttorus-verbosity 2)
      (pp ttorus-original-header-lines)
      (message "original : %s" original)
      (message "cdr original : %s" (cdr original)))
    (if (and ttorus-display-tab-bar
             (member current-window main-windows))
        (progn
          (unless original
            (push (cons buffer header-line-format)
                  ttorus-original-header-lines))
          (unless (equal header-line-format eval-tab)
            (when (> ttorus-verbosity 2)
              (message "Set :eval in header-line-format."))
            (setq header-line-format eval-tab)))
      (when original
        (setq header-line-format (cdr original))
        (setq ttorus-original-header-lines
              (ttorus--assoc-delete-all buffer
                                       ttorus-original-header-lines)))
      (message (ttorus--dashboard)))))

;;; Compatibility
;;; ------------------------------------------------------------

(defun ttorus--convert-meta-to-tree ()
  "Convert old `ttorus-meta' format to new `torus-tree'."
  (setq torus-tree
        (mapcar (lambda (elem)
                  (cons (car elem)
                        (ttorus--assoc-value "ttorus" (cdr elem))))
                ttorus-meta)))

(defun ttorus--convert-old-vars ()
  "Convert old variables format to new one."
  (ttorus--convert-meta-to-tree)
  (when (intern-soft "ttorus-meta-index")
    (when ttorus-meta-index
      (setq ttorus-index (ttorus--build-index)))
    (unintern "ttorus-meta-index"))
  (when (intern-soft "ttorus-meta-history")
    (when ttorus-meta-history
      ;;TODO: more checks
      (setq ttorus-history ttorus-meta-history))
    (unintern "ttorus-meta-history")))

;;; Hooks & Advices
;;; ------------------------------------------------------------

;;;###autoload
(defun ttorus-quit ()
  "Write ttorus before quit."
  (when ttorus-save-on-exit
    (if ttorus-autowrite-file
        (ttorus-write ttorus-autowrite-file)
      (when (y-or-n-p "Write ttorus ? ")
        (call-interactively 'ttorus-write))))
  ;; To be sure they will be nil at startup, even if some plugin saved
  ;; global variables
  (ttorus-reset-menu ?a))

;;;###autoload
(defun ttorus-start ()
  "Read ttorus on startup."
  (when ttorus-load-on-startup
    (if ttorus-autoread-file
        (ttorus-read ttorus-autoread-file)
      (message "Set ttorus-autoread-file if you want to load it."))))

;;;###autoload
(defun ttorus-after-save-torus-file ()
  "Ask whether to read ttorus file after edition."
  (let* ((filename (buffer-file-name (current-buffer)))
         (directory (file-name-directory filename))
         (ttorus-dir (expand-file-name (file-name-as-directory ttorus-dirname))))
    (when (> ttorus-verbosity 2)
      (message "filename : %s" filename)
      (message "filename directory : %s" directory)
      (message "ttorus directory : %s" ttorus-dir))
    (when (equal directory ttorus-dir)
      (when (y-or-n-p "Apply changes to current ttorus variables ? ")
        (ttorus-read filename)))))

;;;###autoload
(defun ttorus-advice-switch-buffer (&rest args)
  "Advice to `switch-to-buffer'. ARGS are irrelevant."
  (when (> ttorus-verbosity 2)
    (message "Advice called with args %s" args))
  (when (and torus-current-torus (ttorus--inside-p))
    (ttorus--update-position)))

;;; Commands
;;; ------------------------------------------------------------

;;;###autoload
(defun ttorus-init ()
  "Initialize ttorus. Add hooks and advices.
Create `ttorus-dirname' if needed."
  (interactive)
  (add-hook 'emacs-startup-hook 'ttorus-start)
  (add-hook 'kill-emacs-hook 'ttorus-quit)
  (add-hook 'after-save-hook 'ttorus-after-save-torus-file)
  (advice-add #'switch-to-buffer :before #'ttorus-advice-switch-buffer)
  (unless (file-exists-p ttorus-dirname)
    (make-directory ttorus-dirname)))

;;;###autoload
(defun ttorus-install-default-bindings ()
  "Install default keybindings."
  (interactive)
  ;; Keymap
  (if (stringp ttorus-prefix-key)
      (global-set-key (kbd ttorus-prefix-key) 'ttorus-map)
    (global-set-key ttorus-prefix-key 'ttorus-map))
  (when (>= ttorus-binding-level 0)
    (define-key ttorus-map (kbd "i") 'ttorus-info)
    (define-key ttorus-map (kbd "c") 'ttorus-add-circle)
    (define-key ttorus-map (kbd "l") 'ttorus-add-location)
    (define-key ttorus-map (kbd "f") 'ttorus-add-file)
    (define-key ttorus-map (kbd "+") 'ttorus-add-torus)
    (define-key ttorus-map (kbd "*") 'ttorus-add-copy-of-torus)
    (define-key ttorus-map (kbd "<left>") 'ttorus-previous-circle)
    (define-key ttorus-map (kbd "<right>") 'ttorus-next-circle)
    (define-key ttorus-map (kbd "<up>") 'ttorus-previous-location)
    (define-key ttorus-map (kbd "<down>") 'ttorus-next-location)
    (define-key ttorus-map (kbd "C-p") 'ttorus-previous-torus)
    (define-key ttorus-map (kbd "C-n") 'ttorus-next-torus)
    (define-key ttorus-map (kbd "SPC") 'ttorus-switch-circle)
    (define-key ttorus-map (kbd "=") 'ttorus-switch-location)
    (define-key ttorus-map (kbd "@") 'ttorus-switch-torus)
    (define-key ttorus-map (kbd "s") 'ttorus-search)
    (define-key ttorus-map (kbd "S") 'ttorus-meta-search)
    (define-key ttorus-map (kbd "d") 'ttorus-delete-location)
    (define-key ttorus-map (kbd "D") 'ttorus-delete-circle)
    (define-key ttorus-map (kbd "-") 'ttorus-delete-torus)
    (define-key ttorus-map (kbd "r") 'ttorus-read)
    (define-key ttorus-map (kbd "w") 'ttorus-write)
    (define-key ttorus-map (kbd "e") 'ttorus-edit))
  (when (>= ttorus-binding-level 1)
    (define-key ttorus-map (kbd "<next>") 'ttorus-history-older)
    (define-key ttorus-map (kbd "<prior>") 'ttorus-history-newer)
    (define-key ttorus-map (kbd "h") 'ttorus-search-history)
    (define-key ttorus-map (kbd "H") 'ttorus-search-meta-history)
    (define-key ttorus-map (kbd "a") 'ttorus-alternate-menu)
    (define-key ttorus-map (kbd "^") 'ttorus-alternate-in-same-torus)
    (define-key ttorus-map (kbd "<") 'ttorus-alternate-circles)
    (define-key ttorus-map (kbd ">") 'ttorus-alternate-in-same-circle)
    (define-key ttorus-map (kbd "n") 'ttorus-rename-circle)
    (define-key ttorus-map (kbd "N") 'ttorus-rename-torus)
    (define-key ttorus-map (kbd "m") 'ttorus-move-location)
    (define-key ttorus-map (kbd "M") 'ttorus-move-circle)
    (define-key ttorus-map (kbd "M-m") 'ttorus-move-torus)
    (define-key ttorus-map (kbd "v") 'ttorus-move-location-to-circle)
    (define-key ttorus-map (kbd "V") 'ttorus-move-circle-to-torus)
    (define-key ttorus-map (kbd "y") 'ttorus-copy-location-to-circle)
    (define-key ttorus-map (kbd "Y") 'ttorus-copy-circle-to-torus)
    (define-key ttorus-map (kbd "j") 'ttorus-join-circles)
    (define-key ttorus-map (kbd "J") 'ttorus-join-toruses)
    (define-key ttorus-map (kbd "#") 'ttorus-layout-menu))
  (when (>= ttorus-binding-level 2)
    (define-key ttorus-map (kbd "o") 'ttorus-reverse-menu)
    (define-key ttorus-map (kbd ":") 'ttorus-prefix-circles-of-current-torus)
    (define-key ttorus-map (kbd "g") 'ttorus-autogroup-menu)
    (define-key ttorus-map (kbd "!") 'ttorus-batch-menu))
  (when (>= ttorus-binding-level 3)
    (define-key ttorus-map (kbd "p") 'ttorus-print-menu)
    (define-key ttorus-map (kbd "z") 'ttorus-reset-menu)
    (define-key ttorus-map (kbd "C-d") 'ttorus-delete-current-location)
    (define-key ttorus-map (kbd "M-d") 'ttorus-delete-current-circle))
  ;; Mouse
  (define-key ttorus-map-mouse-torus [header-line mouse-1] 'ttorus-switch-torus)
  (define-key ttorus-map-mouse-torus [header-line mouse-2] 'ttorus-alternate-toruses)
  (define-key ttorus-map-mouse-torus [header-line mouse-3] 'ttorus-meta-search)
  (define-key ttorus-map-mouse-torus [header-line mouse-4] 'ttorus-previous-torus)
  (define-key ttorus-map-mouse-torus [header-line mouse-5] 'ttorus-next-torus)
  (define-key ttorus-map-mouse-circle [header-line mouse-1] 'ttorus-switch-circle)
  (define-key ttorus-map-mouse-circle [header-line mouse-2] 'ttorus-alternate-circles)
  (define-key ttorus-map-mouse-circle [header-line mouse-3] 'ttorus-search)
  (define-key ttorus-map-mouse-circle [header-line mouse-4] 'ttorus-previous-circle)
  (define-key ttorus-map-mouse-circle [header-line mouse-5] 'ttorus-next-circle)
  (define-key ttorus-map-mouse-location [header-line mouse-1] 'ttorus-tab-mouse)
  (define-key ttorus-map-mouse-location [header-line mouse-2] 'ttorus-alternate-in-meta)
  (define-key ttorus-map-mouse-location [header-line mouse-3] 'ttorus-switch-location)
  (define-key ttorus-map-mouse-location [header-line mouse-4] 'ttorus-previous-location)
  (define-key ttorus-map-mouse-location [header-line mouse-5] 'ttorus-next-location))

;;;###autoload
(defun ttorus-reset-menu (choice)
  "Reset CHOICE variables to nil."
  (interactive
   (list (read-key ttorus--message-reset-choice)))
  (let ((varlist))
    (pcase choice
      (?3 (push 'torus-tree varlist))
      (?i (push 'ttorus-index varlist))
      (?h (push 'ttorus-history varlist))
      (?m (push 'torus-minibuffer-history varlist))
      (?l (push 'ttorus-layout varlist))
      (?& (push 'ttorus-line-col varlist))
      (?\^m (push 'ttorus-markers varlist))
      (?o (push 'ttorus-original-header-lines varlist))
      (?a (setq varlist (list 'torus-tree
                              'ttorus-index
                              'ttorus-history
                              'torus-minibuffer-history
                              'ttorus-layout
                              'ttorus-line-col
                              'ttorus-markers
                              'ttorus-original-header-lines)))
      (?\a (message "Reset cancelled by Ctrl-G."))
      (_ (message "Invalid key.")))
    (dolist (var varlist)
      (when (> ttorus-verbosity 1)
        (message "%s -> nil" (symbol-name var)))
      (set var nil))))

;;; Print
;;; ------------------------------

;;;###autoload
(defun ttorus-info ()
  "Print local info : circle name and locations."
  (interactive)
  (message (ttorus--dashboard)))

;;;###autoload
(defun ttorus-print-menu (choice)
  "Print CHOICE variables."
  (interactive
   (list (read-key ttorus--message-print-choice)))
  (let ((varlist)
        (window (view-echo-area-messages)))
    (pcase choice
      (?3 (push 'torus-tree varlist))
      (?i (push 'ttorus-index varlist))
      (?h (push 'ttorus-history varlist))
      (?m (push 'torus-minibuffer-history varlist))
      (?l (push 'ttorus-layout varlist))
      (?& (push 'ttorus-line-col varlist))
      (?\^m (push 'ttorus-markers varlist))
      (?o (push 'ttorus-original-header-lines varlist))
      (?a (setq varlist (list 'torus-tree
                              'ttorus-index
                              'ttorus-history
                              'torus-minibuffer-history
                              'ttorus-layout
                              'ttorus-line-col
                              'ttorus-markers
                              'ttorus-original-header-lines)))
      (?\a (delete-window window)
           (message "Print cancelled by Ctrl-G."))
      (_ (message "Invalid key.")))
    (dolist (var varlist)
      (message "%s" (symbol-name var))
      (pp (symbol-value var)))))

;;; Add
;;; ------------------------------

;;;###autoload
(defun ttorus-add-torus (torus-name)
  "Create a new torus named TORUS-NAME in `torus-tree'."
  (interactive
   (list (read-string "Name of the new torus : "
                      nil
                      'torus-minibuffer-history)))
  (let ((torus (list torus-name))
        (return))
    (setq return (duo-ref-add-new torus
                                  torus-root
                                  torus-last-torus
                                  #'duo-equal-car-p))
    (unless torus-tree
      (setq torus-tree (car torus-root)))
    (if return
        (progn
          (setq torus-last-torus return)
          (setq torus-current-torus return))
      (message "Torus %s already present in Torus Tree." torus-name))))

;;;###autoload
(defun ttorus-add-circle (circle-name)
  "Add a new circle CIRCLE-NAME to current torus."
  (interactive
   (list
    (read-string "Name of the new circle : "
                 nil
                 'torus-minibuffer-history)))
  (unless torus-current-torus
    (call-interactively 'ttorus-add-torus))
  (let ((circle (list circle-name))
        (return))
    (if (ttorus--empty-torus-p)
        (setq torus-current-circle (duo-add circle
                                            (car torus-current-torus)))
      (setq return (duo-add-new circle
                                (cdr (car torus-current-torus))
                                nil #'duo-equal-car-p))
      (if return
          (setq torus-current-circle return)
        (message "Circle %s already present in Torus %s."
                 circle-name (torus--current-torus-name))))))

;;;###autoload
(defun ttorus-add-location (location-arg)
  "Add LOCATION-ARG to current circle."
  (interactive
   (list
    (read-string "New location : "
                 nil
                 'torus-minibuffer-history)))
  (unless torus-current-torus
    (call-interactively 'ttorus-add-torus))
  (unless torus-current-circle
    (call-interactively 'ttorus-add-circle))
  (let* ((location (if (consp location-arg)
                       location-arg
                     (car (read-from-string location-arg))))
         (member (duo-member location (cdr (car torus-current-circle))))
         (entry (cons (cons (torus--current-torus-name)
                            (torus--current-circle-name))
                      location))
         (pair))
    (if member
        (progn
          (message "Location %s is already present in Torus %s Circle %s."
                   location
                   (torus--current-torus-name)
                   (torus--current-circle-name))
          (setq torus-current-location member)
          (setq torus-current-index (duo-member entry ttorus-index))
          (setq torus-current-history (duo-member entry ttorus-history)))
      (setq torus-current-location (duo-add location (car torus-current-circle)))
      (if ttorus-index
          (progn
            (setq pair (duo-insert-at-group-end entry ttorus-index
                                                #'duo-equal-car-p))
            (setq torus-current-index (car pair))
            (setq ttorus-index (cdr pair)))
        (setq ttorus-index (list entry))
        (setq torus-current-index ttorus-index))
      (if ttorus-history
          (progn
            (setq ttorus-history (duo-push-and-truncate
                                  entry
                                  ttorus-history
                                  ttorus-maximum-history-elements)))
        (setq ttorus-history (list entry)))
      (setq torus-current-history ttorus-history)))
  torus-current-location)

;;;###autoload
(defun ttorus-add-here ()
  "Add current file and point to current circle."
  (interactive)
  (if (buffer-file-name)
      (let* ((pointmark (point-marker))
             (location (cons (buffer-file-name)
                             (marker-position pointmark)))
             (location-line-col (cons location
                                      (cons (line-number-at-pos)
                                            (current-column))))
             (location-marker (cons location pointmark)))
        (ttorus-add-location location)
        (duo-add-new location-line-col ttorus-line-col)
        (duo-add-new location-marker ttorus-markers)
        ;; (ttorus--tab-bar)
        )
    (message "Buffer must have a filename to be added to the torus.")))

;;;###autoload
(defun ttorus-add-file (filename)
  "Add FILENAME to the current circle.
The location added will be (file . 1)."
  (interactive (list (read-file-name "File to add : ")))
  (if (file-exists-p filename)
      (progn
        (find-file filename)
        (ttorus-add-here))
    (message "File %s does not exist." filename)))

;;;###autoload
(defun ttorus-add-buffer ())

;;;###autoload
(defun ttorus-add-copy-of-torus (ttorus-name)
  "Create a new ttorus named TTORUS-NAME as copy of the current ttorus."
  (interactive
   (list (read-string "Name of the new ttorus : "
                      nil
                      'torus-minibuffer-history)))
  (setq torus-current-torus (copy-tree torus-current-torus))
  (if torus-current-torus
      (setcar torus-current-torus ttorus-name)
    (setq torus-current-torus (list ttorus-name)))
  (if torus-tree
      (duo-add-new torus-current-torus torus-tree)
    (setq torus-tree (list torus-current-torus))))

;;; Navigate
;;; ------------------------------

;;;###autoload
(defun ttorus-previous-torus ()
  "Jump to the previous ttorus."
  (interactive)
  (if (ttorus--empty-tree-p)
      (message ttorus--message-empty-tree)
    (setq torus-current-torus
          (duo-circ-previous torus-current-torus torus-tree))
    (setq torus-current-circle
          (cdr (car torus-current-torus)))
    (setq torus-current-location
          (cdr (car torus-current-circle))))
  torus-current-torus)

;;;###autoload
(defun ttorus-next-torus ()
  "Jump to the next ttorus."
  (interactive)
  (if (ttorus--empty-tree-p)
      (message ttorus--message-empty-tree)
    (setq torus-current-torus
          (duo-circ-next torus-current-torus torus-tree))
    (setq torus-current-circle
          (cdr (car torus-current-torus)))
    (setq torus-current-location
          (cdr (car torus-current-circle))))
  torus-current-torus)

;;;###autoload
(defun ttorus-previous-circle ()
  "Jump to the previous circle."
  (interactive)
  (if (ttorus--empty-torus-p)
      (message ttorus--message-empty-torus)
    (setq torus-current-circle
          (duo-circ-previous torus-current-circle
                             (cdr (car torus-current-torus))))
    (setq torus-current-location
          (cdr (car torus-current-circle))))
  torus-current-circle)

;;;###autoload
(defun ttorus-next-circle ()
  "Jump to the next circle."
  (interactive)
  (if (ttorus--empty-torus-p)
      (message ttorus--message-empty-torus)
    (setq torus-current-circle
          (duo-circ-next torus-current-circle
                         (cdr (car torus-current-torus))))
    (setq torus-current-location
          (cdr (car torus-current-circle))))
  torus-current-circle)

;;;###autoload
(defun ttorus-previous-location ()
  "Jump to the previous location."
  (interactive)
  (if (ttorus--empty-circle-p)
      (message ttorus--message-empty-circle)
    (setq torus-current-location
          (duo-circ-previous torus-current-location
                             (cdr (car torus-current-circle)))))
  torus-current-location)

;;;###autoload
(defun ttorus-next-location ()
  "Jump to the next location."
  (interactive)
  (if (ttorus--empty-circle-p)
      (message ttorus--message-empty-circle)
    (setq torus-current-location
          (duo-circ-previous torus-current-location
                             (cdr (car torus-current-circle)))))
  torus-current-location)

;;;###autoload
(defun ttorus-switch-circle (circle-name)
  "Jump to CIRCLE-NAME circle.
With prefix argument \\[universal-argument], open the buffer in a
horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument], open the
buffer in a vertical split."
  (interactive
   (list (completing-read
          "Go to circle : "
          (mapcar #'car torus-current-torus) nil t)))
  (ttorus--prefix-argument-split current-prefix-arg)
  (ttorus--update-position)
  (let* ((circle (assoc circle-name torus-current-torus))
         (index (cl-position circle torus-current-torus :test #'equal))
         (before (cl-subseq torus-current-torus 0 index))
         (after (cl-subseq torus-current-torus index)))
    (setq torus-current-torus (append after before)))
  (ttorus--jump)
  (ttorus--apply-or-push-layout))

;;;###autoload
(defun ttorus-switch-location (location-name)
  "Jump to LOCATION-NAME location.
With prefix argument \\[universal-argument], open the buffer in a
horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument], open the
buffer in a vertical split."
  (interactive
   (list
    (completing-read
     "Go to location : "
     (mapcar #'ttorus--concise (cdr (car torus-current-torus))) nil t)))
  (ttorus--prefix-argument-split current-prefix-arg)
  (ttorus--update-position)
  (let* ((circle (cdr (car torus-current-torus)))
         (index (cl-position location-name circle
                          :test #'ttorus--equal-concise-p))
         (before (cl-subseq circle 0 index))
         (after (cl-subseq circle index)))
    (setcdr (car torus-current-torus) (append after before)))
  (ttorus--jump))

;;;###autoload
(defun ttorus-switch-torus (ttorus-name)
  "Jump to TTORUS-NAME ttorus.
With prefix argument \\[universal-argument], open the buffer in a
horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument], open the
buffer in a vertical split."
  (interactive
   (list (completing-read
          "Go to ttorus : "
          (mapcar #'car ttorus-meta) nil t)))
  (ttorus--prefix-argument-split current-prefix-arg)
  (ttorus--update-meta)
  (let* ((ttorus (assoc ttorus-name ttorus-meta))
         (index (cl-position ttorus ttorus-meta :test #'equal))
         (before (cl-subseq ttorus-meta 0 index))
         (after (cl-subseq ttorus-meta index)))
    (if index
        (setq ttorus-meta (append after before))
      (message "ttorus not found.")))
  (ttorus--update-from-meta)
  (ttorus--build-table)
  (setq ttorus-index (ttorus--build-index))
  (ttorus--complete-and-clean-layout)
  (ttorus--jump)
  (ttorus--apply-or-push-layout))

;;; Search
;;; ------------------------------

;;;###autoload
(defun ttorus-search (location-name)
  "Search LOCATION-NAME in the ttorus.
Go to the first matching circle and location."
  (interactive
   (list
    (completing-read
     "Search location in ttorus : "
     (mapcar #'ttorus--concise ttorus-table) nil t)))
  (ttorus--prefix-argument-split current-prefix-arg)
  (let* ((location-circle
          (cl-find
           location-name ttorus-table
           :test #'ttorus--equal-concise-p)))
    (ttorus--switch location-circle)))


;;;###autoload
(defun ttorus-meta-search (name)
  "Search location NAME in all ttoruses.
Go to the first matching ttorus, circle and location."
  (interactive
   (list
    (completing-read
     "Search location in all ttoruses : "
     (mapcar #'ttorus--concise ttorus-index) nil t)))
  (ttorus--prefix-argument-split current-prefix-arg)
  (let* ((entry
          (cl-find
           name ttorus-index
           :test #'ttorus--equal-concise-p)))
    (ttorus--meta-switch entry)))

;;; History
;;; ------------------------------

;;;###autoload
(defun ttorus-history-newer ()
  "Go to newer location in history."
  (interactive)
  (if torus-current-torus
      (progn
        (ttorus--prefix-argument-split current-prefix-arg)
        (if ttorus-old-history
            (progn
              (setq ttorus-old-history (append (last ttorus-old-history) (butlast ttorus-old-history)))
              (ttorus--switch (car ttorus-old-history)))
          (message "History is empty.")))
    (message ttorus--message-empty-torus)))

;;;###autoload
(defun ttorus-history-older ()
  "Go to older location in history."
  (interactive)
  (if torus-current-torus
      (progn
        (ttorus--prefix-argument-split current-prefix-arg)
        (if ttorus-old-history
            (progn
              (setq ttorus-old-history (append (cdr ttorus-old-history) (list (car ttorus-old-history))))
              (ttorus--switch (car ttorus-old-history)))
          (message "History is empty.")))
    (message ttorus--message-empty-torus)))

;;;###autoload
(defun ttorus-search-history (location-name)
  "Search LOCATION-NAME in `ttorus-old-history'."
  (interactive
   (list
    (completing-read
     "Search location in history : "
     (mapcar #'ttorus--concise ttorus-old-history) nil t)))
  (ttorus--prefix-argument-split current-prefix-arg)
  (when ttorus-old-history
    (let* ((index (cl-position location-name ttorus-old-history
                            :test #'ttorus--equal-concise-p))
           (before (cl-subseq ttorus-old-history 0 index))
           (element (nth index ttorus-old-history))
           (after (cl-subseq ttorus-old-history (1+ index))))
      (setq ttorus-old-history (append (list element) before after)))
    (ttorus--switch (car ttorus-old-history))))

;;;###autoload
(defun ttorus-search-meta-history (location-name)
  "Search LOCATION-NAME in `ttorus-history'."
  (interactive
   (list
    (completing-read
     "Search location in history : "
     (mapcar #'ttorus--concise ttorus-history) nil t)))
  (ttorus--prefix-argument-split current-prefix-arg)
  (when ttorus-history
    (let* ((index (cl-position location-name ttorus-history
                            :test #'ttorus--equal-concise-p))
           (before (cl-subseq ttorus-history 0 index))
           (element (nth index ttorus-history))
           (after (cl-subseq ttorus-history (1+ index))))
      (setq ttorus-history (append (list element) before after)))
    (ttorus--meta-switch (car ttorus-history))))

;;; Alternate
;;; ------------------------------

;;;###autoload
(defun ttorus-alternate-in-meta ()
  "Alternate last two locations in meta history.
If outside the ttorus, just return inside, to the last ttorus location."
  (interactive)
  (if ttorus-meta
      (progn
        (ttorus--prefix-argument-split current-prefix-arg)
        (if (ttorus--inside-p)
            (if (and ttorus-history
                     (>= (length ttorus-history) 2))
                (progn
                  (ttorus--update-meta)
                  (setq ttorus-history (append (list (car (cdr ttorus-history)))
                                                   (list (car ttorus-history))
                                                   (nthcdr 2 ttorus-history)))
                  (ttorus--meta-switch (car ttorus-history)))
              (message "Meta history has less than two elements."))
          (ttorus--jump)))
    (message ttorus--message-empty-tree)))

;;;###autoload
(defun ttorus-alternate-in-same-torus ()
  "Alternate last two locations in history belonging to the current circle.
If outside the ttorus, just return inside, to the last ttorus location."
  (interactive)
  (if torus-current-torus
      (progn
        (ttorus--prefix-argument-split current-prefix-arg)
        (if (ttorus--inside-p)
            (if (and ttorus-old-history
                     (>= (length ttorus-old-history) 2))
                (progn
                  (ttorus--update-meta)
                  (setq ttorus-old-history (append (list (car (cdr ttorus-old-history)))
                                              (list (car ttorus-old-history))
                                              (nthcdr 2 ttorus-old-history)))
                  (ttorus--switch (car ttorus-old-history)))
              (message "History has less than two elements."))
          (ttorus--jump)))
    (message ttorus--message-empty-torus)))

;;;###autoload
(defun ttorus-alternate-in-same-circle ()
  "Alternate last two locations in history belonging to the current circle.
If outside the ttorus, just return inside, to the last ttorus location."
  (interactive)
  (if torus-current-torus
      (progn
        (ttorus--prefix-argument-split current-prefix-arg)
        (if (ttorus--inside-p)
            (if (and ttorus-old-history
                     (>= (length ttorus-old-history) 2))
                (progn
                  (ttorus--update-meta)
                  (let ((history ttorus-old-history)
                        (circle (car (car torus-current-torus)))
                        (element)
                        (location-circle))
                    (pop history)
                    (while (and (not location-circle) history)
                      (setq element (pop history))
                      (when (equal circle (cdr element))
                        (setq location-circle element)))
                    (if location-circle
                        (ttorus--switch location-circle)
                      (message "No alternate file in same circle in history."))))
              (message "History has less than two elements."))
          (ttorus--jump)))
    (message ttorus--message-empty-torus)))

;;;###autoload
(defun ttorus-alternate-toruses ()
  "Alternate last two ttoruses in meta history.
If outside the ttorus, just return inside, to the last ttorus location."
  (interactive)
  (if ttorus-meta
      (progn
        (ttorus--prefix-argument-split current-prefix-arg)
        (if (ttorus--inside-p)
            (if (and ttorus-history
                     (>= (length ttorus-history) 2))
                (progn
                  (ttorus--update-meta)
                  (let ((history ttorus-history)
                        (ttorus (car (car ttorus-meta)))
                        (element)
                        (location-circle-torus))
                    (while (and (not location-circle-torus) history)
                      (setq element (pop history))
                      (when (not (equal ttorus (cddr element)))
                        (setq location-circle-torus element)))
                    (if location-circle-torus
                        (ttorus--meta-switch location-circle-torus)
                      (message "No alternate ttorus in history."))))
              (message "Meta History has less than two elements."))
          (ttorus--jump)))
    (message "Meta ttorus is empty.")))

;;;###autoload
(defun ttorus-alternate-circles ()
  "Alternate last two circles in history.
If outside the ttorus, just return inside, to the last ttorus location."
  (interactive)
  (if torus-current-torus
      (progn
        (ttorus--prefix-argument-split current-prefix-arg)
        (if (ttorus--inside-p)
            (if (and ttorus-old-history
                     (>= (length ttorus-old-history) 2))
                (progn
                  (ttorus--update-meta)
                  (let ((history ttorus-old-history)
                        (circle (car (car torus-current-torus)))
                        (element)
                        (location-circle))
                    (while (and (not location-circle) history)
                      (setq element (pop history))
                      (when (not (equal circle (cdr element)))
                        (setq location-circle element)))
                    (if location-circle
                        (ttorus--switch location-circle)
                      (message "No alternate circle in history."))))
              (message "History has less than two elements."))
          (ttorus--jump)))
    (message ttorus--message-empty-torus)))

;;;###autoload
(defun ttorus-alternate-menu (choice)
  "Alternate according to CHOICE."
  (interactive
   (list (read-key ttorus--message-alternate-choice)))
  (pcase choice
    (?m (funcall 'ttorus-alternate-in-meta))
    (?t (funcall 'ttorus-alternate-in-same-torus))
    (?c (funcall 'ttorus-alternate-in-same-circle))
    (?T (funcall 'ttorus-alternate-toruses))
    (?C (funcall 'ttorus-alternate-circles))
    (?\a (message "Alternate operation cancelled by Ctrl-G."))
    (_ (message "Invalid key."))))

;;; Rename
;;; ------------------------------

;;;###autoload
(defun ttorus-rename-circle ()
  "Rename current circle."
  (interactive)
  (if torus-current-torus
      (let*
          ((old-name (car (car torus-current-torus)))
           (prompt (format "New name of circle %s : " old-name))
           (circle-name (read-string prompt nil 'torus-minibuffer-history)))
        (ttorus--update-input-history circle-name)
        (setcar (car torus-current-torus) circle-name)
        (dolist (location-circle ttorus-table)
          (when (equal (cdr location-circle) old-name)
            (setcdr location-circle circle-name)))
        (dolist (location-circle ttorus-old-history)
          (when (equal (cdr location-circle) old-name)
            (setcdr location-circle circle-name)))
        (dolist (location-circle-torus ttorus-history)
          (when (equal (cadr location-circle-torus) old-name)
            (setcar (cdr location-circle-torus) circle-name)))
        (dolist (location-circle-torus ttorus-index)
          (when (equal (cadr location-circle-torus) old-name)
            (setcar (cdr location-circle-torus) circle-name)))
        (message "Renamed circle %s -> %s" old-name circle-name))
    (message "ttorus is empty. Please add a circle first with ttorus-add-circle.")))

;;;###autoload
(defun ttorus-rename-torus ()
  "Rename current ttorus."
  (interactive)
  (if ttorus-meta
      (let*
          ((old-name (car (car ttorus-meta)))
           (prompt (format "New name of ttorus %s : " old-name))
           (ttorus-name (read-string prompt nil 'torus-minibuffer-history)))
        (ttorus--update-input-history ttorus-name)
        (setcar (car ttorus-meta) ttorus-name)
        (message "Renamed ttorus %s -> %s" old-name ttorus-name))
    (message ttorus--message-empty-tree)))

;;; Move
;;; ------------------------------

;;;###autoload
(defun ttorus-move-circle (circle-name)
  "Move current circle after CIRCLE-NAME."
  (interactive
   (list (completing-read
          "Move current circle after : "
          (mapcar #'car torus-current-torus) nil t)))
  (ttorus--update-position)
  (let* ((circle (assoc circle-name torus-current-torus))
         (index (1+ (cl-position circle torus-current-torus :test #'equal)))
         (current (list (car torus-current-torus)))
         (before (cl-subseq torus-current-torus 1 index))
         (after (cl-subseq torus-current-torus index)))
    (setq torus-current-torus (append before current after))
    (ttorus-switch-circle (caar current))))

;;;###autoload
(defun ttorus-move-location (location-name)
  "Move current location after LOCATION-NAME."
  (interactive
   (list
    (completing-read
     "Move current location after : "
     (mapcar #'ttorus--concise (cdr (car torus-current-torus))) nil t)))
  (ttorus--update-position)
  (let* ((circle (cdr (car torus-current-torus)))
         (index (1+ (cl-position location-name circle
                                 :test #'ttorus--equal-concise-p)))
         (current (list (car circle)))
         (before (cl-subseq circle 1 index))
         (after (cl-subseq circle index)))
    (setcdr (car torus-current-torus) (append before current after))
    (ttorus-switch-location (car current))))

;;;###autoload
(defun ttorus-move-torus (ttorus-name)
  "Move current ttorus after TTORUS-NAME."
  (interactive
   (list (completing-read
          "Move current ttorus after : "
          (mapcar #'car ttorus-meta) nil t)))
  (ttorus--update-meta)
  (let* ((ttorus (assoc ttorus-name ttorus-meta))
         (index (1+ (cl-position ttorus ttorus-meta :test #'equal)))
         (current (copy-tree (list (car ttorus-meta))))
         (before (copy-tree (cl-subseq ttorus-meta 1 index)))
         (after (copy-tree (cl-subseq ttorus-meta index))))
    (setq ttorus-meta (append before current after))
    (ttorus--update-from-meta)
    (ttorus-switch-torus (caar current))))

;;;###autoload
(defun ttorus-move-location-to-circle (circle-name)
  "Move current location to CIRCLE-NAME."
  (interactive
   (list (completing-read
          "Move current location to circle : "
          (mapcar #'car torus-current-torus) nil t)))
  (ttorus--update-position)
  (let* ((location (car (cdr (car torus-current-torus))))
         (circle (cdr (assoc circle-name torus-current-torus)))
         (old-name (car (car torus-current-torus)))
         (old-pair (cons location old-name)))
    (if (member location circle)
        (message "Location %s already exists in circle %s."
                 (ttorus--concise location)
                 circle-name)
      (message "Moving location %s to circle %s."
               (ttorus--concise location)
               circle-name)
      (pop (cdar torus-current-torus))
      (setcdr (assoc circle-name torus-current-torus)
              (push location circle))
      (dolist (location-circle ttorus-table)
        (when (equal location-circle old-pair)
          (setcdr location-circle circle-name)))
      (dolist (location-circle ttorus-old-history)
        (when (equal location-circle old-pair)
          (setcdr location-circle circle-name)))
      (ttorus--jump))))


;;;###autoload
(defun ttorus-move-circle-to-torus (ttorus-name)
  "Move current circle to TTORUS-NAME."
  (interactive
   (list (completing-read
          "Move current circle to ttorus : "
          (mapcar #'car ttorus-meta) nil t)))
  (ttorus--update-position)
  (let* ((circle (cl-copy-seq (car torus-current-torus)))
         (ttorus (copy-tree
                 (cdr (assoc "ttorus" (assoc ttorus-name ttorus-meta)))))
         (circle-name (car circle))
         (circle-torus (cons circle-name (caar ttorus-meta))))
    (if (member circle ttorus)
        (message "Circle %s already exists in ttorus %s."
                 circle-name
                 ttorus-name)
      (message "Moving circle %s to ttorus %s."
               circle-name
               ttorus-name)
      (when (> ttorus-verbosity 2)
        (message "circle-torus %s" circle-torus))
      (setcdr (assoc "ttorus" (assoc ttorus-name ttorus-meta))
              (push circle ttorus))
      (setq torus-current-torus (ttorus--assoc-delete-all circle-name torus-current-torus))
      (setq ttorus-table
            (ttorus--reverse-assoc-delete-all circle-name ttorus-table))
      (setq ttorus-old-history
            (ttorus--reverse-assoc-delete-all circle-name ttorus-old-history))
      (setq ttorus-markers
            (ttorus--reverse-assoc-delete-all circle-name ttorus-markers))
      (setq ttorus-index
            (ttorus--reverse-assoc-delete-all circle-torus ttorus-index))
      (setq ttorus-history
            (ttorus--reverse-assoc-delete-all circle-torus ttorus-history))
      (ttorus--build-table)
      (setq ttorus-index (ttorus--build-index))
      (ttorus--jump))))

;;;###autoload
(defun ttorus-copy-location-to-circle (circle-name)
  "Copy current location to CIRCLE-NAME."
  (interactive
   (list (completing-read
          "Copy current location to circle : "
          (mapcar #'car torus-current-torus) nil t)))
  (ttorus--update-position)
  (let* ((location (car (cdr (car torus-current-torus))))
         (circle (cdr (assoc circle-name torus-current-torus))))
    (if (member location circle)
        (message "Location %s already exists in circle %s."
                 (ttorus--concise location)
                 circle-name)
      (message "Copying location %s to circle %s."
                 (ttorus--concise location)
                 circle-name)
      (setcdr (assoc circle-name torus-current-torus) (push location circle))
      (ttorus--build-table)
      (setq ttorus-index (ttorus--build-index)))))

;;;###autoload
(defun ttorus-copy-circle-to-torus (ttorus-name)
  "Copy current circle to TTORUS-NAME."
  (interactive
   (list (completing-read
          "Copy current circle to ttorus : "
          (mapcar #'car ttorus-meta) nil t)))
  (ttorus--update-position)
  (let* ((circle (cl-copy-seq (car torus-current-torus)))
         (ttorus (copy-tree
                 (cdr (assoc "ttorus" (assoc ttorus-name ttorus-meta))))))
    (if (member circle ttorus)
        (message "Circle %s already exists in ttorus %s."
                 (car circle)
                 ttorus-name)
      (message "Copying circle %s to ttorus %s."
               (car circle)
               ttorus-name)
      (setcdr (assoc "ttorus" (assoc ttorus-name ttorus-meta))
              (push circle ttorus)))
    (ttorus--build-table)
    (setq ttorus-index (ttorus--build-index))))

;;; Reverse
;;; ------------------------------

;;;###autoload
(defun ttorus-reverse-circles ()
  "Reverse order of the circles."
  (interactive)
  (ttorus--update-position)
  (setq torus-current-torus (reverse torus-current-torus))
  (ttorus--jump))

;;;###autoload
(defun ttorus-reverse-locations ()
  "Reverse order of the locations in the current circles."
  (interactive)
  (ttorus--update-position)
  (setcdr (car torus-current-torus) (reverse (cdr (car torus-current-torus))))
  (ttorus--jump))

;;;###autoload
(defun ttorus-deep-reverse ()
  "Reverse order of the locations in each circle."
  (interactive)
  (ttorus--update-position)
  (setq torus-current-torus (reverse torus-current-torus))
  (dolist (circle torus-current-torus)
    (setcdr circle (reverse (cdr circle))))
  (ttorus--jump))


;;;###autoload
(defun ttorus-reverse-menu (choice)
  "Split according to CHOICE."
  (interactive
   (list (read-key ttorus--message-reverse-choice)))
  (pcase choice
    (?c (funcall 'ttorus-reverse-circles))
    (?l (funcall 'ttorus-reverse-locations))
    (?d (funcall 'ttorus-deep-reverse))
    (?\a (message "Reverse operation cancelled by Ctrl-G."))
    (_ (message "Invalid key."))))

;;; Join
;;; ------------------------------

;;;###autoload
(defun ttorus-prefix-circles-of-current-torus (prefix)
  "Add PREFIX to circle names of `torus-current-torus'."
  (interactive
   (list
    (read-string (format ttorus--message-prefix-circle
                         (car (car ttorus-meta)))
                 nil
                 'torus-minibuffer-history)))
  (let ((varlist))
    (setq varlist (ttorus--prefix-circles prefix (car (car ttorus-meta))))
    (setq torus-current-torus (car varlist))
    (setq ttorus-old-history (car (cdr varlist))))
  (ttorus--build-table)
  (setq ttorus-index (ttorus--build-index)))

;;;###autoload
(defun ttorus-join-circles (circle-name)
  "Join current circle with CIRCLE-NAME."
  (interactive
   (list
    (completing-read "Join current circle with circle : "
                     (mapcar #'car torus-current-torus) nil t)))
  (let* ((current-name (car (car torus-current-torus)))
         (join-name (concat current-name ttorus-join-separator circle-name))
         (user-choice
          (read-string (format "Name of the joined ttorus [%s] : " join-name))))
    (when (> (length user-choice) 0)
      (setq join-name user-choice))
    (ttorus-add-circle join-name)
    (setcdr (car torus-current-torus)
            (append (cdr (assoc current-name torus-current-torus))
                    (cdr (assoc circle-name torus-current-torus))))
    (delete-dups (cdr (car torus-current-torus))))
  (ttorus--update-meta)
  (ttorus--build-table)
  (setq ttorus-index (ttorus--build-index))
  (ttorus--jump))

;;;###autoload
(defun ttorus-join-toruses (ttorus-name)
  "Join current ttorus with TTORUS-NAME in `ttorus-meta'."
  (interactive
   (list
    (completing-read "Join current ttorus with ttorus : "
                     (mapcar #'car ttorus-meta) nil t)))
  (ttorus--prefix-argument-split current-prefix-arg)
  (ttorus--update-meta)
  (let* ((current-name (car (car ttorus-meta)))
         (join-name (concat current-name ttorus-join-separator ttorus-name))
         (user-choice
          (read-string (format "Name of the joined ttorus [%s] : " join-name)))
         (prompt-current
          (format ttorus--message-prefix-circle current-name))
         (prompt-added
          (format ttorus--message-prefix-circle ttorus-name))
         (prefix-current
          (read-string prompt-current nil 'torus-minibuffer-history))
         (prefix-added
          (read-string prompt-added nil 'torus-minibuffer-history))
         (varlist)
         (ttorus-added)
         (history-added)
         (input-added))
    (when (> (length user-choice) 0)
      (setq join-name user-choice))
    (ttorus--update-input-history prefix-current)
    (ttorus--update-input-history prefix-added)
    (ttorus-add-copy-of-torus join-name)
    (ttorus-prefix-circles-of-current-torus prefix-current)
    (setq varlist (ttorus--prefix-circles prefix-added ttorus-name))
    (setq ttorus-added (car varlist))
    (setq history-added (car (cdr varlist)))
    (setq input-added (car (cdr (cdr varlist))))
    (if (seq-intersection torus-current-torus ttorus-added #'ttorus--equal-car-p)
        (message ttorus--message-circle-name-collision)
      (setq torus-current-torus (append torus-current-torus ttorus-added))
      (setq ttorus-old-history (append ttorus-old-history history-added))
      (setq torus-minibuffer-history (append torus-minibuffer-history input-added))))
  (ttorus--update-meta)
  (ttorus--build-table)
  (setq ttorus-index (ttorus--build-index))
  (ttorus--jump))

;;; Autogroup
;;; ------------------------------

;;;###autoload
(defun ttorus-autogroup (quoted-function)
  "Autogroup all ttorus locations according to the values of QUOTED-FUNCTION.
A new ttorus is created on `ttorus-meta' to contain the new circles.
The function must return the names of the new circles as strings."
  (interactive)
  (let ((ttorus-name
         (read-string "Name of the autogroup ttorus : "
                      nil
                      'torus-minibuffer-history))
        (all-locations))
    (if (assoc ttorus-name ttorus-meta)
        (message "ttorus %s already exists in ttorus-meta" ttorus-name)
      (ttorus-add-copy-of-torus ttorus-name)
      (dolist (circle torus-current-torus)
        (dolist (location (cdr circle))
          (push location all-locations)))
      (setq torus-current-torus (seq-group-by quoted-function all-locations))))
  (setq ttorus-old-history nil)
  (setq ttorus-markers nil)
  (setq torus-minibuffer-history nil)
  (ttorus--build-table)
  (setq ttorus-index (ttorus--build-index))
  (ttorus--update-meta)
  (ttorus--jump))

;;;###autoload
(defun ttorus-autogroup-by-path ()
  "Autogroup all location of the ttorus by directories.
A new ttorus is created to contain the new circles."
  (interactive)
  (ttorus-autogroup (lambda (elem) (directory-file-name (file-name-directory (car elem))))))

;;;###autoload
(defun ttorus-autogroup-by-directory ()
  "Autogroup all location of the ttorus by directories.
A new ttorus is created to contain the new circles."
  (interactive)
  (ttorus-autogroup #'ttorus--directory))

;;;###autoload
(defun ttorus-autogroup-by-extension ()
  "Autogroup all location of the ttorus by extension.
A new ttorus is created to contain the new circles."
  (interactive)
  (ttorus-autogroup #'ttorus--extension-description))

;;;###autoload
(defun ttorus-autogroup-by-git-repo ()
  "Autogroup all location of the ttorus by git repositories.
A new ttorus is created to contain the new circles."
  ;; TODO
  )

;;;###autoload
(defun ttorus-autogroup-menu (choice)
  "Autogroup according to CHOICE."
  (interactive
   (list (read-key ttorus--message-autogroup-choice)))
    (pcase choice
      (?p (funcall 'ttorus-autogroup-by-path))
      (?d (funcall 'ttorus-autogroup-by-directory))
      (?e (funcall 'ttorus-autogroup-by-extension))
      (?\a (message "Autogroup cancelled by Ctrl-G."))
      (_ (message "Invalid key."))))

;;; Batch
;;; ------------------------------


;;;###autoload
(defun ttorus-run-elisp-code-on-circle (elisp-code)
  "Run ELISP-CODE to all files of the circle."
  (interactive (list (read-string
                      "Elisp code to run to all files of the circle : ")))
  (dolist (iter (number-sequence 1 (length (cdar torus-current-torus))))
    (when (> ttorus-verbosity 1)
      (message "%d. Applying %s to %s" iter elisp-code (cadar torus-current-torus))
      (message "Evaluated : %s"
               (car (read-from-string (format "(progn %s)" elisp-code)))))
    (ttorus--eval-string elisp-code)
    (ttorus-next-location)))

;;;###autoload
(defun ttorus-run-elisp-command-on-circle (command)
  "Run an Emacs Lisp COMMAND to all files of the circle."
  (interactive (list (read-command
                      "Elisp command to run to all files of the circle : ")))
  (dolist (iter (number-sequence 1 (length (cdar torus-current-torus))))
    (when (> ttorus-verbosity 1)
      (message "%d. Applying %s to %s" iter command (cadar torus-current-torus)))
    (funcall command)
    (ttorus-next-location)))

;;;###autoload
(defun ttorus-run-shell-command-on-circle (command)
  "Run a shell COMMAND to all files of the circle."
  (interactive (list (read-string
                      "Shell command to run to all files of the circle : ")))
  (let ((keep-value shell-command-dont-erase-buffer))
    (setq shell-command-dont-erase-buffer t)
    (dolist (iter (number-sequence 1 (length (cdar torus-current-torus))))
      (when (> ttorus-verbosity 1)
        (message "%d. Applying %s to %s" iter command (cadar torus-current-torus)))
      (shell-command (format "%s %s"
                             command
                             (shell-quote-argument (buffer-file-name))))
      (ttorus-next-location))
    (setq shell-command-dont-erase-buffer keep-value)))

;;;###autoload
(defun ttorus-run-async-shell-command-on-circle (command)
  "Run a shell COMMAND to all files of the circle."
  (interactive (list (read-string
                      "Shell command to run to all files of the circle : ")))
  (let ((keep-value async-shell-command-buffer))
    (setq async-shell-command-buffer 'new-buffer)
    (dolist (iter (number-sequence 1 (length (cdar torus-current-torus))))
      (when (> ttorus-verbosity 1)
        (message "%d. Applying %s to %s" iter command (cadar torus-current-torus)))
      (async-shell-command (format "%s %s"
                             command
                             (shell-quote-argument (buffer-file-name))))
      (ttorus-next-location))
    (setq async-shell-command-buffer keep-value)))

;;;###autoload
(defun ttorus-batch-menu (choice)
  "Split according to CHOICE."
  (interactive
   (list (read-key ttorus--message-batch-choice)))
  (pcase choice
    (?e (call-interactively 'ttorus-run-elisp-code-on-circle))
    (?c (call-interactively 'ttorus-run-elisp-command-on-circle))
    (?! (call-interactively 'ttorus-run-shell-command-on-circle))
    (?& (call-interactively 'ttorus-run-async-shell-command-on-circle))
    (?\a (message "Batch operation cancelled by Ctrl-G."))
    (_ (message "Invalid key."))))

;;; Split
;;; ------------------------------

;;;###autoload
(defun ttorus-split-horizontally ()
  "Split horizontally to view all buffers in current circle.
Split until `ttorus-maximum-horizontal-split' is reached."
  (interactive)
  (let* ((circle (cdr (car torus-current-torus)))
         (numsplit (1- (length circle))))
    (when (> ttorus-verbosity 1)
      (message "numsplit = %d" numsplit))
    (if (> numsplit (1- ttorus-maximum-horizontal-split))
        (message "Too many files to split.")
      (delete-other-windows)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> ttorus-verbosity 2)
          (message "iter = %d" iter))
        (split-window-below)
        (balance-windows)
        (other-window 1)
        (ttorus-next-location))
      (other-window 1)
      (ttorus-next-location))))

;;;###autoload
(defun ttorus-split-vertically ()
  "Split vertically to view all buffers in current circle.
Split until `ttorus-maximum-vertical-split' is reached."
  (interactive)
  (let* ((circle (cdr (car torus-current-torus)))
         (numsplit (1- (length circle))))
    (when (> ttorus-verbosity 1)
      (message "numsplit = %d" numsplit))
    (if (> numsplit (1- ttorus-maximum-vertical-split))
        (message "Too many files to split.")
      (delete-other-windows)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> ttorus-verbosity 2)
          (message "iter = %d" iter))
        (split-window-right)
        (balance-windows)
        (other-window 1)
        (ttorus-next-location))
      (other-window 1)
      (ttorus-next-location))))

;;;###autoload
(defun ttorus-split-main-left ()
  "Split with left main window to view all buffers in current circle."
  (interactive)
  (let* ((circle (cdr (car torus-current-torus)))
         (numsplit (- (length circle) 2)))
    (when (> ttorus-verbosity 1)
      (message "numsplit = %d" numsplit))
    (if (> numsplit (1- ttorus-maximum-horizontal-split))
        (message "Too many files to split.")
      (delete-other-windows)
      (split-window-right)
      (other-window 1)
      (ttorus-next-location)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> ttorus-verbosity 2)
          (message "iter = %d" iter))
        (split-window-below)
        (balance-windows)
        (other-window 1)
        (ttorus-next-location))
      (other-window 1)
      (ttorus-next-location))))

;;;###autoload
(defun ttorus-split-main-right ()
  "Split with right main window to view all buffers in current circle."
  (interactive)
  (let* ((circle (cdr (car torus-current-torus)))
         (numsplit (- (length circle) 2)))
    (when (> ttorus-verbosity 1)
      (message "numsplit = %d" numsplit))
    (if (> numsplit (1- ttorus-maximum-horizontal-split))
        (message "Too many files to split.")
      (delete-other-windows)
      (split-window-right)
      (ttorus-next-location)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> ttorus-verbosity 2)
          (message "iter = %d" iter))
        (split-window-below)
        (balance-windows)
        (other-window 1)
        (ttorus-next-location))
      (other-window 1)
      (ttorus-next-location))))

;;;###autoload
(defun ttorus-split-main-top ()
  "Split with main top window to view all buffers in current circle."
  (interactive)
  (let* ((circle (cdr (car torus-current-torus)))
         (numsplit (- (length circle) 2)))
    (when (> ttorus-verbosity 1)
      (message "numsplit = %d" numsplit))
    (if (> numsplit (1- ttorus-maximum-vertical-split))
        (message "Too many files to split.")
      (delete-other-windows)
      (split-window-below)
      (other-window 1)
      (ttorus-next-location)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> ttorus-verbosity 2)
          (message "iter = %d" iter))
        (split-window-right)
        (balance-windows)
        (other-window 1)
        (ttorus-next-location))
      (other-window 1)
      (ttorus-next-location))))

;;;###autoload
(defun ttorus-split-main-bottom ()
  "Split with main bottom window to view all buffers in current circle."
  (interactive)
  (let* ((circle (cdr (car torus-current-torus)))
         (numsplit (- (length circle) 2)))
    (when (> ttorus-verbosity 1)
      (message "numsplit = %d" numsplit))
    (if (> numsplit (1- ttorus-maximum-vertical-split))
        (message "Too many files to split.")
      (delete-other-windows)
      (split-window-below)
      (ttorus-next-location)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> ttorus-verbosity 2)
          (message "iter = %d" iter))
        (split-window-right)
        (balance-windows)
        (other-window 1)
        (ttorus-next-location))
      (other-window 1)
      (ttorus-next-location))))

;;;###autoload
(defun ttorus-split-grid ()
  "Split horizontally & vertically to view all current circle buffers in a grid."
  (interactive)
  (let* ((circle (cdr (car torus-current-torus)))
         (len-circle (length circle))
         (max-iter (1- len-circle))
         (ratio (/ (float (frame-text-width))
                   (float (frame-text-height))))
         (horizontal (sqrt (/ (float len-circle) ratio)))
         (vertical (* ratio horizontal))
         (int-hor (min (ceiling horizontal)
                       ttorus-maximum-horizontal-split))
         (int-ver (min (ceiling vertical)
                       ttorus-maximum-vertical-split))
         (getout)
         (num-hor-minus)
         (num-hor)
         (num-ver-minus)
         (total 0))
    (if (< (* int-hor int-ver) len-circle)
        (message "Too many files to split.")
      (let ((dist-dec-hor)
            (dist-dec-ver))
        (when (> ttorus-verbosity 2)
          (message "ratio = %f" ratio)
          (message "horizontal = %f" horizontal)
          (message "vertical = %f" vertical)
          (message "int-hor int-ver = %d %d" int-hor  int-ver))
        (while (not getout)
          (setq dist-dec-hor (abs (- (* (1- int-hor) int-ver) len-circle)))
          (setq dist-dec-ver (abs (- (* int-hor (1- int-ver)) len-circle)))
          (when (> ttorus-verbosity 2)
            (message "Distance hor ver = %f %f" dist-dec-hor dist-dec-ver))
          (cond ((and (<= dist-dec-hor dist-dec-ver)
                      (>= (* (1- int-hor) int-ver) len-circle))
                 (setq int-hor (1- int-hor))
                 (when (> ttorus-verbosity 2)
                   (message "Decrease int-hor : int-hor int-ver = %d %d"
                            int-hor  int-ver)))
                ((and (>= dist-dec-hor dist-dec-ver)
                      (>= (* int-hor (1- int-ver)) len-circle))
                 (setq int-ver (1- int-ver))
                 (when (> ttorus-verbosity 2)
                   (message "Decrease int-ver : int-hor int-ver = %d %d"
                            int-hor  int-ver)))
                (t (setq getout t)
                   (when (> ttorus-verbosity 2)
                     (message "Getout : %s" getout)
                     (message "int-hor int-ver = %d %d" int-hor int-ver))))))
      (setq num-hor-minus (number-sequence 1 (1- int-hor)))
      (setq num-hor (number-sequence 1 int-hor))
      (setq num-ver-minus (number-sequence 1 (1- int-ver)))
      (when (> ttorus-verbosity 2)
        (message "num-hor-minus = %s" num-hor-minus)
        (message "num-hor = %s" num-hor)
        (message "num-ver-minus = %s" num-ver-minus))
      (delete-other-windows)
      (dolist (iter-hor num-hor-minus)
        (when (> ttorus-verbosity 2)
          (message "iter hor = %d" iter-hor))
        (setq max-iter (1- max-iter))
        (split-window-below)
        (balance-windows)
        (other-window 1))
      (other-window 1)
      (dolist (iter-hor num-hor)
        (dolist (iter-ver num-ver-minus)
          (when (> ttorus-verbosity 2)
            (message "iter hor ver = %d %d" iter-hor iter-ver)
            (message "total max-iter = %d %d" total max-iter))
          (when (< total max-iter)
            (setq total (1+ total))
            (split-window-right)
            (balance-windows)
            (other-window 1)
            (ttorus-next-location)))
        (when (< total max-iter)
          (other-window 1)
          (ttorus-next-location)))
    (other-window 1)
    (ttorus-next-location))))

;;;###autoload
(defun ttorus-layout-menu (choice)
  "Split according to CHOICE."
  (interactive
   (list (read-key ttorus--message-layout-choice)))
  (ttorus--complete-and-clean-layout)
  (let ((circle (caar torus-current-torus)))
    (when (member choice '(?m ?o ?h ?v ?l ?r ?t ?b ?g))
      (setcdr (assoc circle ttorus-layout) choice))
    (pcase choice
      (?m nil)
      (?o (delete-other-windows))
      (?h (funcall 'ttorus-split-horizontally))
      (?v (funcall 'ttorus-split-vertically))
      (?l (funcall 'ttorus-split-main-left))
      (?r (funcall 'ttorus-split-main-right))
      (?t (funcall 'ttorus-split-main-top))
      (?b (funcall 'ttorus-split-main-bottom))
      (?g (funcall 'ttorus-split-grid))
      (?\a (message "Layout cancelled by Ctrl-G."))
      (_ (message "Invalid key.")))))

;;; Tabs
;;; ------------------------------

(defun ttorus-tab-mouse (event)
  "Manage click EVENT on locations part of tab line."
  (interactive "@e")
  (let* ((index (cdar (nthcdr 4 (cadr event))))
        (before (substring-no-properties
                    (caar (nthcdr 4 (cadr event))) 0 index))
        (pipes (seq-filter (lambda (elem) (equal elem ?|)) before))
        (len-pipes (length pipes)))
    (if (equal len-pipes 0)
        (ttorus-alternate-in-same-circle)
      (ttorus-switch-location (nth (length pipes) (cdar torus-current-torus))))))

;;; Delete
;;; ------------------------------

;;;###autoload
(defun ttorus-delete-circle (circle-name)
  "Delete circle given by CIRCLE-NAME."
  (interactive
   (list
    (completing-read "Delete circle : "
                     (mapcar #'car torus-current-torus) nil t)))
  (when (y-or-n-p (format "Delete circle %s ? " circle-name))
    (setq torus-current-torus (ttorus--assoc-delete-all circle-name torus-current-torus))
    (setq ttorus-table
          (ttorus--reverse-assoc-delete-all circle-name ttorus-table))
    (setq ttorus-old-history
          (ttorus--reverse-assoc-delete-all circle-name ttorus-old-history))
    (setq ttorus-markers
          (ttorus--reverse-assoc-delete-all circle-name ttorus-markers))
    (let ((circle-torus (cons (caar torus-current-torus) (caar ttorus-meta))))
      (setq ttorus-index
            (ttorus--reverse-assoc-delete-all circle-torus ttorus-index))
      (setq ttorus-history
            (ttorus--reverse-assoc-delete-all circle-torus ttorus-history)))
    (ttorus--build-table)
    (setq ttorus-index (ttorus--build-index))
    (ttorus--jump)))

;;;###autoload
(defun ttorus-delete-location (location-name)
  "Delete location given by LOCATION-NAME."
  (interactive
   (list
    (completing-read
     "Delete location : "
     (mapcar #'ttorus--concise (cdr (car torus-current-torus))) nil t)))
  (if (and
       (> (length (car torus-current-torus)) 1)
       (y-or-n-p
        (format
         "Delete %s from circle %s ? "
         location-name
         (car (car torus-current-torus)))))
      (let* ((circle (cdr (car torus-current-torus)))
             (index (cl-position location-name circle
                                 :test #'ttorus--equal-concise-p))
             (location (nth index circle))
             (location-circle (cons location (caar torus-current-torus)))
             (location-circle-torus (cons location (cons (caar torus-current-torus)
                                                         (caar ttorus-meta)))))
        (setcdr (car torus-current-torus) (cl-remove location circle))
        (setq ttorus-table (cl-remove location-circle ttorus-table))
        (setq ttorus-old-history (cl-remove location-circle ttorus-old-history))
        (setq ttorus-markers (cl-remove location-circle ttorus-markers))
        (setq ttorus-index (cl-remove location-circle-torus ttorus-index))
        (setq ttorus-history (cl-remove location-circle-torus ttorus-history))
        (ttorus--jump))
    (message "No location in current circle.")))

;;;###autoload
(defun ttorus-delete-current-circle ()
  "Delete current circle."
  (interactive)
  (ttorus-delete-circle (ttorus--concise (car (car torus-current-torus)))))

;;;###autoload
(defun ttorus-delete-current-location ()
  "Remove current location from current circle."
  (interactive)
  (ttorus-delete-location (ttorus--concise (car (cdr (car torus-current-torus))))))

;;;###autoload
(defun ttorus-delete-torus (ttorus-name)
  "Delete ttorus given by TTORUS-NAME."
  (interactive
   (list
    (completing-read "Delete ttorus : "
                     (mapcar #'car ttorus-meta) nil t)))
  (when (y-or-n-p (format "Delete ttorus %s ? " ttorus-name))
    (when (equal ttorus-name (car (car ttorus-meta)))
      (ttorus-switch-torus (car (car (cdr ttorus-meta)))))
    (setq ttorus-meta (ttorus--assoc-delete-all ttorus-name ttorus-meta))))

;;; File R/W
;;; ------------------------------

;;;###autoload
(defun ttorus-write (filename)
  "Write main ttorus variables to FILENAME as Lisp code.
An adequate extension is added if needed.
If called interactively, ask for the variables to save (default : all)."
  (interactive
   (list
    (read-file-name
     "ttorus file : "
     (file-name-as-directory ttorus-dirname))))
  ;; We surely don’t want to load a file we’ve just written
  (remove-hook 'after-save-hook 'ttorus-after-save-torus-file)
  (if ttorus-meta
      (let*
          ((file-basename (file-name-nondirectory filename))
           (minus-len-ext (- (min (length ttorus-file-extension)
                                  (length filename))))
           (buffer)
           (varlist '(torus-tree
                      ttorus-index
                      ttorus-history
                      torus-minibuffer-history
                      ttorus-layout
                      ttorus-line-col)))
        (ttorus--update-position)
        (ttorus--update-input-history file-basename)
        (unless (equal (cl-subseq filename minus-len-ext) ttorus-file-extension)
          (setq filename (concat filename ttorus-file-extension)))
        (unless ttorus-table
          (ttorus--build-table))
        (unless ttorus-index
          (setq ttorus-index (ttorus--build-index)))
        (ttorus--complete-and-clean-layout)
        (ttorus--update-meta)
        (if varlist
            (progn
              (ttorus--roll-backups filename)
              (setq buffer (find-file-noselect filename))
              (with-current-buffer buffer
                (erase-buffer)
                (dolist (var varlist)
                  (when var
                    (insert (concat
                             "(setq "
                             (symbol-name var)
                             " (quote\n"))
                    (pp (symbol-value var) buffer)
                    (insert "))\n\n")))
                (save-buffer)
                (kill-buffer)))
          (message "Write cancelled : empty variables.")))
    (message "Write cancelled : empty ttorus."))
  ;; Restore the hook
  (add-hook 'after-save-hook 'ttorus-after-save-torus-file))

;;;###autoload
(defun ttorus-read (filename)
  "Read main ttorus variables from FILENAME as Lisp code."
  (interactive
   (list
    (read-file-name
     "ttorus file : "
     (file-name-as-directory ttorus-dirname))))
  (let*
      ((file-basename (file-name-nondirectory filename))
       (minus-len-ext (- (min (length ttorus-file-extension)
                              (length filename))))
       (buffer))
    (unless (equal (cl-subseq filename minus-len-ext) ttorus-file-extension)
      (setq filename (concat filename ttorus-file-extension)))
    (when (or (not torus-tree)
              (y-or-n-p ttorus--message-replace-torus))
      (ttorus--update-input-history file-basename)
      (if (file-exists-p filename)
          (progn
            (setq buffer (find-file-noselect filename))
            (eval-buffer buffer)
            (kill-buffer buffer))
        (message "File %s does not exist." filename))))
  ;; For old files
  (ttorus--convert-old-vars)
  ;; Also saved in file
  ;; (ttorus--update-meta)
  ;; (ttorus--build-table)
  ;; (setq ttorus-index (ttorus--build-index))
  (ttorus--jump))

;;;###autoload
(defun ttorus-edit (filename)
  "Edit ttorus file FILENAME in the ttorus files dir.
Be sure to understand what you’re doing, and not leave some variables
in inconsistent state, or you might encounter strange undesired effects."
  (interactive
   (list
    (read-file-name
     "ttorus file : "
     (file-name-as-directory ttorus-dirname))))
  (find-file filename))

;;; End
;;; ------------------------------------------------------------

(provide 'ttorus)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; ttorus.el ends here
