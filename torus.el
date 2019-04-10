;;; torus.el --- A buffer groups manager             -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Chimay

;; Author : Chimay
;; Name: Torus
;; Package-Version: pre 2.0
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

;;                          wheel
;;                        +---+---+      +---------------------+--------------+
;;                  +-----+   |   +------+ current torus index | wheel length |
;;                  |     +---+---+      +---------------------+--------------+
;;                  |
;;                  |
;;             +----+----+---------+---------+-------+---------+
;;             | torus 1 | torus 2 | torus 3 | ...   | torus M |
;;             +---------+----+----+---------+-------+---------+
;;                            |
;;                  +---------+
;;                  |
;;              +---+---+ torus root
;;         +----+   |   +----+
;;         |    +---+---+    |
;;         |                 |
;;         |                 |
;; +-------+------+      +---+---+     +----------------------+--------------+
;; | "torus name" |      |   |   +-----+ current circle index | torus length |
;; +--------------+      +-+-+---+     +----------------------+--------------+
;;                         |
;;               +---------+
;;               |
;;         +-----+----+----------+----------+-------+----------+
;;         | circle 1 | circle 2 | circle 3 | ...   | circle N |
;;         +----------+----------+-----+----+-------+----------+
;;                                     |
;;                  +------------------+
;;                  |
;;              +---+---+ circle root
;;         +----+   |   +---+
;;         |    +---+---+   |
;;         |                |
;;         |                |
;; +-------+-------+    +---+---+   +------------------------+---------------+
;; | "circle name" |    |   |   +---+ current location index | circle length |
;; +---------------+    +-+-+---+   +------------------------+---------------+
;;                        |
;;          +-------------+
;;          |
;;    +-----+------+------------+------------+-------+------------+
;;    | location 1 | location 2 | location 3 | ...   | location P |
;;    +------------+------+-----+------------+-------+------------+
;;                        |
;;                        |
;;                        |
;;               +--------+----------+
;;               | "file" | position |
;;               +--------+----------+

;;; Code:
;;; ----------------------------------------------------------------------

;;; Requires
;;; ------------------------------------------------------------

(eval-when-compile
  (require 'duo-referen))

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

(defcustom torus-prefix-key "s-t"
  "Prefix key for the ttorus key mappings.
Will be processed by `kbd'."
  :type 'string
  :group 'ttorus)

(defcustom torus-binding-level 1
  "Binding level : the higher it is, the more bindings you have.
Level 0 : Basic
Level 1 : Common
Level 2 : Advanced
Level 3 : Debug"
  :type 'integer
  :group 'ttorus)

(defcustom torus-verbosity 1
  "Level of verbosity.
1 = normal
2 = light debug
3 = heavy debug."
  :type 'integer
  :group 'ttorus)

(defcustom torus-dirname user-emacs-directory
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

(defcustom torus-autoread-file "auto.el"
  "The file to load on startup when `ttorus-load-on-startup' is t."
  :type 'string
  :group 'ttorus)

(defcustom torus-autowrite-file "auto.el"
  "The file to write before quitting Emacs when `ttorus-save-on-exit' is t."
  :type 'string
  :group 'ttorus)

(defcustom torus-backup-number 3
  "Number of backups of ttorus files."
  :type 'integer
  :group 'ttorus)

(defcustom torus-maximum-history-elements 50
  "Maximum number of elements in history variables.
See `ttorus-history' and `torus-user-input-history'."
  :type 'integer
  :group 'ttorus)

(defcustom torus-maximum-horizontal-split 3
  "Maximum number of horizontal split, see `ttorus-split-horizontally'."
  :type 'integer
  :group 'ttorus)

(defcustom torus-maximum-vertical-split 4
  "Maximum number of vertical split, see `ttorus-split-vertically'."
  :type 'integer
  :group 'ttorus)

(defcustom ttorus-display-tab-bar nil
  "Whether to display a tab bar in `header-line-format'."
  :type 'boolean
  :group 'ttorus)

(defcustom torus-separator-torus-circle " >> "
  "String between ttorus and circle in the dashboard."
  :type 'string
  :group 'ttorus)

(defcustom torus-separator-circle-location " > "
  "String between circle and location(s) in the dashboard."
  :type 'string
  :group 'ttorus)

(defcustom torus-location-separator " | "
  "String between location(s) in the dashboard."
  :type 'string
  :group 'ttorus)

(defcustom torus-prefix-separator "/"
  "String between the prefix and the circle names.
The name of the new circles will be of the form :
\"User_input_prefix `torus-prefix-separator' Name_of_the_added_circle\"
without the spaces. If the user enter a blank prefix,
the added circle names remain untouched."
  :type 'string
  :group 'ttorus)

(defcustom torus-join-separator " & "
  "String between the names when joining.
The name of the new object will be of the form :
\"Object-1 `torus-join-separator' Object-2\"
without the spaces."
  :type 'string
  :group 'ttorus)

;;; Variables
;;; ------------------------------------------------------------

(defvar torus-wheel (list nil)
  "Roughly speaking, the wheel is a reference to a list of toruses.
More precisely, it’s a cons whose car is a list of toruses.
The cdr of the wheel points to a cons which contains :
- the index of the current torus in car
- the length of the list of toruses in cdr
Each torus has a name, a list of circles, the index of current
circle and the length of the circle list (torus length)
Each circle has a name, a list of locations, the index of current
location and the length of the location list (circle length).
Each location contains a file name and a position :
\(file . position)")

(defvar torus-helix (list nil)
  "Reference to an alist containing toruses, circles and their locations.
More precisely, it’s a cons whose car is a list of entries.
Each entry is a cons :
\((torus-name . circle-name) . (file . position))
or :
\(path . location)
where :
path = (torus-name . circle-name)
location = (file . position)")

(defvar torus-grid (list nil)
  "Reference to an alist containing toruses and circles.
More precisely, it’s a cons whose car is a list of entries.
Each entry is a cons :
\(torus-name . circle-name)")

(defvar ttorus-history (list nil)
  "Reference to an alist containing history of locations in all toruses.
More precisely, it’s a cons whose car is a list of entries.
Each entry is a nested cons :
\((torus-name . circle-name) . (file . position))")

(defvar torus-user-input-history (list nil)
  "Reference to history of user input in minibuffer.
More precisely, it’s a cons whose car is a list of entries.")

(defvar torus-split-layout (list nil)
  "Reference to a list containing split layout of all circles in all toruses.
More precisely, it’s a cons whose car is a list of entries.
Each entry is a cons :
\((torus-name . circle-name) . layout)
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

(defvar ttorus-line-col (list nil)
  "Reference to an alist storing locations and lines & columns in files.
More precisely, it’s a cons whose car is a list of entries.
Each entry is a cons :
\((file . position) . (line . column))
Allows to display lines & columns.")

;;; Transient
;;; ------------------------------

(defvar ttorus-markers (list nil)
  "Reference to an alist containing markers to opened files.
More precisely, it’s a cons whose car is a list of entries.
Each entry is a cons :
\((file . position) . marker)
Contain only the files opened in buffers.")

(defvar ttorus-original-header-lines (list nil)
  "Reference to an alist containing header lines before the tab bar changed it.
More precisely, it’s a cons whose car is a list of entries.
Each entry is a cons :
\(buffer . original-header-line)")

;;; Current cons in list
;;; ------------------------------

(defvar torus-cur-torus nil
  "Cons of current torus in `torus-wheel'.")

(defvar torus-cur-circle nil
  "Cons of current circle in `torus-cur-torus'.")

(defvar torus-cur-location nil
  "Cons of current location in `torus-cur-circle'.")

(defvar torus-cur-helix nil
  "Cons of current entry in `torus-helix'.")

(defvar torus-cur-history nil
  "Cons of current entry in `ttorus-history'.")

(defvar torus-cur-user-input nil
  "Cons of current entry in `torus-user-input-history'.")

;;; Last cons in list
;;; ------------------------------

(defvar torus-last-torus nil
  "Last torus in `torus-wheel'. Just for speed.")

(defvar torus-last-circle nil
  "Last circle in `torus-cur-torus'. Just for speed.")

(defvar torus-last-location nil
  "Last location in `torus-cur-circle'. Just for speed.")

;;; Files
;;; ------------------------------

(defvar torus-file-extension ".el"
  "Extension of torus files.")

;;; Prompts
;;; ------------------------------

;;; Empty
;;; ---------------

(defvar ttorus--msg-empty-wheel
  "Torus Wheel is empty. Please add a location with torus-add-location.")

(defvar ttorus--msg-empty-torus
  "Torus %s is empty. Please add a location with torus-add-location.")

(defvar ttorus--msg-empty-circle
  "Circle %s in Torus %s is empty. Please use torus-add-location.")

;;; Menus
;;; ---------------

(defvar ttorus--msg-add-menu
  "Add [h] here [f] file [b] buffer [l] location [c] circle [t] torus")

(defvar ttorus--msg-reset-menu
  "Reset [a] all [w] wheel
      [t] current torus [C-t] last torus
      [c] current circle [C-c] last circle
      [l] current location [C-l] last location
      [x] helix [C-x] current helix [g] grid [G] current grid
      [h] history [C-h] current history
      [u] user input history [C-u] current user input
      [s] split layout [&] line & col [m] markers [o] orig header line")

(defvar ttorus--msg-print-menu
  "Print [a] all [w] wheel
      [t] current torus [C-t] last torus
      [c] current circle [C-c] last circle
      [l] current location [C-l] last location
      [x] helix [C-x] current helix [g] grid [G] current grid
      [h] history [C-h] current history
      [u] user input history [C-u] current user input
      [s] split layout [&] line & col [m] markers [o] orig header line")

(defvar ttorus--msg-alternate-menu
  "Alternate [m] in meta ttorus [t] in ttorus [c] in circle [T] ttoruses [C] circles")

(defvar ttorus--msg-reverse-menu
  "Reverse [l] locations [c] circle [d] deep : locations & circles")

(defvar ttorus--msg-autogroup-menu
  "Autogroup by [p] path [d] directory [e] extension")

(defvar ttorus--msg-batch-menu
  "Run on circle files [e] Elisp code [c] Elisp command \n\
                    [!] Shell command [&] Async Shell command")

(defvar ttorus--msg-layout-menu
  "Layout [m] manual [o] one window [h] horizontal [v] vertical [g] grid\n\
       main window on [l] left [r] right [t] top [b] bottom")

;;; Miscellaneous
;;; ---------------

(defvar ttorus--msg-file-does-not-exist
  "File %s does not exist anymore. It will be removed from the ttorus.")

(defvar ttorus--msg-existent-location
  "Location %s already exists in circle %s")

(defvar ttorus--msg-prefix-circle
  "Prefix for the circle of torus %s (leave blank for none) ? ")

(defvar ttorus--msg-circle-name-collision
  "Circle name collision. Please add/adjust prefixes to avoid confusion.")

(defvar ttorus--msg-replace-torus
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

;;; Predicates
;;; ------------------------------

(defun torus--generic-< (one two)
  "Return t if ONE is less than TWO.
Don’t use this on circular list, or it’ll probably loop forever."
  (cond ((and (number-or-marker-p one)
              (number-or-marker-p two))
         (< one two))
        ((and (stringp one)
              (stringp two))
         (string< one two))
        ((and (consp one)
              (consp two))
         (let ((car-one (car one))
               (car-two (car two)))
           (or (torus--generic-< car-one car-two)
               (and (equal car-one car-two)
                    (torus--generic-< (cdr one) (cdr two))))))
        (t (error "Function torus--generic-< : wrong type argument"))))

;;; Strings
;;; ------------------------------

(defun torus--eval-string (string)
  "Eval Elisp code in STRING."
  (eval (car (read-from-string (format "(progn %s)" string)))))

;;; Files
;;; ------------------------------

(defun torus--directory (object)
  "Return the last directory component of OBJECT."
  (let* ((filename (pcase object
                     (`(,(and (pred stringp) one) . ,(pred integerp)) one)
                     ((pred stringp) object)))
         (grandpa (file-name-directory (directory-file-name
                                        (file-name-directory
                                         (directory-file-name filename)))))
         (relative (file-relative-name filename grandpa)))
    (directory-file-name (file-name-directory relative))))

(defun torus--extension-description (object)
  "Return the extension description of OBJECT."
  (let* ((filename (pcase object
                     (`(,(and (pred stringp) one) . ,(pred integerp)) one)
                     ((pred stringp) object)))
         (extension (file-name-extension filename)))
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

;;; Template
;;; ------------------------------

(defsubst torus--tree-template (name)
  "Minimal tree template for data structure with a NAME."
  (cons name (cons nil nil)))

;;; State
;;; ------------------------------

;;; Wheel
;;; ---------------

(defsubst torus--ref-torus-list ()
  "Return reference to the torus list."
  torus-wheel)

(defsubst torus--torus-list ()
  "Return the torus list."
  (car (torus--ref-torus-list)))

(defsubst torus--torus-index (&optional index)
  "Return current torus index in wheel. Change it to INDEX if non nil."
  (if index
      (setcar (cdr (torus--ref-torus-list)) index)
    (car (cdr (torus--ref-torus-list)))))

(defsubst torus--wheel-length (&optional length)
  "Return wheel length. Change it to LENGTH if non nil."
  (if length
      (setcdr (cdr (torus--ref-torus-list)) length)
    (cdr (cdr (torus--ref-torus-list)))))

;;; Torus
;;; ---------------

(defsubst torus--root-torus ()
  "Return root of current torus."
  (car torus-cur-torus))

(defsubst torus--torus-name (&optional name)
  "Return current torus name. Change it to NAME if non nil."
  (if name
      (setcar (torus--root-torus) name)
    (car (torus--root-torus))))

(defsubst torus--ref-circle-list ()
  "Return reference to current circle list."
  (cdr (torus--root-torus)))

(defsubst torus--circle-list ()
  "Return current circle list."
  (car (torus--ref-circle-list)))

(defsubst torus--circle-index (&optional index)
  "Return current circle index in torus. Change it to INDEX if non nil."
  (if index
      (setcar (cdr (torus--ref-circle-list)) index)
    (car (cdr (torus--ref-circle-list)))))

(defsubst torus--torus-length (&optional length)
  "Return torus length. Change it to LENGTH if non nil."
  (if length
      (setcdr (cdr (torus--ref-circle-list)) length)
    (cdr (cdr (torus--ref-circle-list)))))

;;; Circle
;;; ---------------

(defsubst torus--root-circle ()
  "Return root of current circle."
  (car torus-cur-circle))

(defsubst torus--circle-name (&optional name)
  "Return current torus name. Change it to NAME if non nil."
  (if name
      (setcar (torus--root-circle) name)
    (car (torus--root-circle))))

(defsubst torus--ref-location-list ()
  "Return reference to current location list."
  (cdr (torus--root-circle)))

(defsubst torus--location-list ()
  "Return current location list."
  (car (torus--ref-location-list)))

(defsubst torus--location-index (&optional index)
  "Return current location index in circle. Change it to INDEX if non nil."
  (if index
      (setcar (cdr (torus--ref-location-list)) index)
    (car (cdr (torus--ref-location-list)))))

(defsubst torus--circle-length (&optional length)
  "Return circle length. Change it to LENGTH if non nil."
  (if length
      (setcdr (cdr (torus--ref-location-list)) length)
    (cdr (cdr (torus--ref-location-list)))))

;;; In / Out
;;; ---------------

(defun ttorus--inside-p (&optional buffer)
  "Whether BUFFER belongs to the torus.
Argument BUFFER nil means use current buffer."
  (let* ((buffer (if buffer
                     buffer
                   (current-buffer)))
         (filename (buffer-file-name buffer))
         (wheel-files (mapcar 'cadr (duo-deref torus-helix))))
    (duo-member filename wheel-files)))

;;; Empty ?
;;; ---------------

(defsubst torus--empty-wheel-p ()
  "Whether the torus list is empty."
  (null (torus--torus-list)))

(defsubst torus--empty-torus-p ()
  "Whether current torus is empty.
It’s empty when nil or just a name in car
but no circle in it."
  (null (torus--circle-list)))

(defsubst torus--empty-circle-p ()
  "Whether current circle is empty.
It’s empty when nil or just a name in car
but no location in it."
  (null (torus--location-list)))

;;; Enter the Void
;;; ---------------

(defsubst torus--set-nil-circle ()
  "Set current circle variables to nil."
  (setq torus-cur-circle nil)
  (setq torus-last-circle nil))

(defsubst torus--set-nil-location ()
  "Set current location variables to nil."
  (setq torus-cur-location nil)
  (setq torus-last-location nil))

;;; Alter index
;;; ---------------

(defsubst torus--add-index (ref)
  "Update index, length in cdr of REF when an element is added in car of REF."
  (let* ((index-length (cdr ref))
         (length (cdr index-length)))
    (if index-length
        (progn
          (setcar index-length length)
          (setcdr index-length (1+ length)))
      (setcdr ref (cons 0 1)))))

(defsubst torus--remove-index (ref)
  "Update index, length in cdr of REF when an element is removed from car."
  (let* ((index-length (cdr ref))
         (length (cdr index-length)))
    (when index-length
      (setcar index-length 0)
      (setcdr index-length (1- length)))))

(defsubst torus--increase-index (ref &optional num)
  "Increase current index in cdr of REF by NUM. Circular.
NUM defaults to 1."
  (let* ((num (if num
                  num
                1))
         (index-length (cdr ref))
         (index (car index-length))
         (length (cdr index-length)))
    (if index-length
        (setcar index-length (mod (+ index num) length))
      (setcdr ref (cons 0 1)))))

(defsubst torus--decrease-index (ref &optional num)
  "Decrease current index in cdr of REF by NUM. Circular.
NUM defaults to 1."
  (let* ((num (if num
                  num
                1))
         (index-length (cdr ref))
         (index (car index-length))
         (length (cdr index-length)))
    (if index-length
        (setcar index-length (mod (- index num) length))
      (setcdr ref (cons 0 1)))))

;;; Seek to index
;;; ---------------

(defsubst torus--seek-torus (&optional index)
  "Set current torus to the one given by INDEX.
INDEX defaults to current torus index."
  (when index
    (torus--torus-index index))
  (let* ((ref (torus--ref-torus-list))
         (content (car ref))
         (index-length (cdr ref))
         (index (car index-length))
         (length (cdr index-length))
         (tail-length (- length index 1)))
    (if (and index content)
        (progn
          (setq torus-cur-torus (duo-at-index index content))
          (setq torus-last-torus (nthcdr tail-length torus-cur-torus)))
      (setq torus-cur-torus content))))

(defsubst torus--seek-circle (&optional index)
  "Set current circle to the one given by INDEX.
INDEX defaults to current circle index."
  (when index
    (torus--circle-index index))
  (let* ((ref (torus--ref-circle-list))
         (content (car ref))
         (index-length (cdr ref))
         (index (car index-length))
         (length (cdr index-length))
         (tail-length (- length index 1)))
    (if (and index content)
        (progn
          (setq torus-cur-circle (duo-at-index index content))
          (setq torus-last-circle (nthcdr tail-length torus-cur-circle)))
      (setq torus-cur-circle content))))

(defsubst torus--seek-location (&optional index)
  "Set current location to the one given by INDEX.
INDEX defaults to current location index."
  (when index
    (torus--location-index index))
  (let* ((ref (torus--ref-location-list))
         (content (car ref))
         (index-length (cdr ref))
         (index (car index-length))
         (length (cdr index-length))
         (tail-length (- length index 1)))
    (if (and index content)
        (progn
          (setq torus-cur-location (duo-at-index index content))
          (setq torus-last-location (nthcdr tail-length torus-cur-location)))
      (setq torus-cur-location content))))

;;; Rewind
;;; ---------------

(defsubst torus--rewind-torus ()
  "Set torus variables to first torus in wheel."
  (setq torus-cur-torus (torus--torus-list))
  (setq torus-last-torus (duo-last (torus--torus-list)))
  (torus--torus-index 0))

(defsubst torus--rewind-circle ()
  "Set circle variables to first circle in torus."
  (setq torus-cur-circle (torus--circle-list))
  (setq torus-last-circle (duo-last (torus--circle-list)))
  (torus--circle-index 0))

(defsubst torus--rewind-location ()
  "Set location variables to first location in circle."
  (setq torus-cur-location (torus--location-list))
  (setq torus-last-location (duo-last (torus--location-list)))
  (torus--location-index 0))

;;; Entry
;;; ------------------------------

(defun torus--make-entry (&optional object)
  "Return an entry ((torus-name . circle-name) . (file . position)) from OBJECT.
Use current torus, circle and location if not given."
  (pcase object
    ('nil
     (let ((torus-name (torus--torus-name))
           (circle-name (torus--circle-name))
           (location (car torus-cur-location)))
       (cons (cons torus-name circle-name) location)))
    (`(,(pred stringp) . ,(pred integerp))
     (let ((torus-name (torus--torus-name))
           (circle-name (torus--circle-name)))
       (cons (cons torus-name circle-name) object)))
    (`(,(pred stringp) . (,(pred stringp) . ,(pred integerp)))
     (let ((torus-name (torus--torus-name)))
       (cons (cons torus-name (car object)) (cdr object))))
    (`((,(pred stringp) . ,(pred stringp)) .
       (,(pred stringp) . ,(pred integerp)))
     object)
    (_ (error "Function torus--make-entry : wrong type argument"))))

;;; Helix
;;; ------------------------------

(defun torus--add-to-helix (&optional object)
  "Add an entry built from OBJECT to `torus-helix'."
  (let* ((entry (torus--make-entry object))
         (helix (duo-deref torus-helix))
         (member (duo-member entry helix)))
    (when (and entry (not member))
      (setq torus-cur-helix
            (duo-ref-insert-in-sorted-list entry
                                           torus-helix
                                           #'torus--generic-<)))))

(defun torus--add-to-grid (&optional object)
  "Add an entry built from OBJECT to `torus-grid'."
  (let* ((entry (if object
                    object
                  (cons (torus--torus-name)
                        (torus--circle-name))))
         (grid (duo-deref torus-grid))
         (member (duo-member entry grid)))
    (when (and entry (not member))
      (setq torus-cur-grid
            (duo-ref-insert-in-sorted-list entry
                                           torus-grid
                                           #'torus--generic-<)))))

(defun torus--build-helix ()
  "Build helix from `torus-wheel'."
  (setq torus-helix (list nil))
  (let ((torus-name)
        (circle-name)
        (path)
        (entry))
    (dolist (torus (torus--torus-list))
      (setq torus-name (car torus))
      (dolist (circle (car (cdr torus)))
        (setq circle-name (car circle))
        (setq path (cons torus-name circle-name))
        (dolist (location (car (cdr circle)))
          (setq entry (cons path location))
          (setq torus-cur-helix
                (duo-ref-insert-in-sorted-list entry
                                               torus-helix
                                               #'torus--generic-<))
          (when (> torus-verbosity 1)
            (message "Helix entry %s" entry)))))))

(defun torus--build-grid ()
  "Build grid from `torus-wheel'."
  (setq torus-grid (list nil))
  (let ((torus-name)
        (circle-name)
        (path)
        (entry))
    (dolist (torus (torus--torus-list))
      (setq torus-name (car torus))
      (dolist (circle (car (cdr torus)))
        (setq circle-name (car circle))
        (setq entry (cons torus-name circle-name))
        (setq torus-cur-grid
              (duo-ref-insert-in-sorted-list entry
                                             torus-grid
                                             #'torus--generic-<))
        (when (> torus-verbosity 1)
          (message "Grid entry %s" entry))))))

;;; History
;;; ------------------------------

(defun torus--add-to-history (&optional object)
  "Add an entry built from OBJECT to `ttorus-history'."
  (let* ((entry (torus--make-entry object))
         (history)
         (member))
    (unless (eq torus-cur-history history)
      (duo-ref-reverse-previous torus-cur-history ttorus-history))
    (setq history (duo-deref ttorus-history))
    (setq member (duo-member entry history))
    (when entry
      (if member
          (setq torus-cur-history
                (duo-ref-teleport-cons-previous history member ttorus-history))
        (setq torus-cur-history
              (duo-ref-push-and-truncate entry
                                         ttorus-history
                                         torus-maximum-history-elements))))))

;;; User Input History
;;; ------------------------------

(defun torus--add-user-input (string)
  "Add an entry built from OBJECT to `torus-user-input-history'."
  (let* ((history (duo-deref torus-user-input-history))
         (member (duo-member string history)))
    (if member
        (setq torus-cur-user-input
              (duo-ref-teleport-cons-previous history
                                              member
                                              torus-user-input-history))
      (setq torus-cur-user-input
            (duo-ref-push-and-truncate string
                                       torus-user-input-history
                                       torus-maximum-history-elements)))))

;;; Tables
;;; ------------------------------

(defun torus--add-entry-to-table (entry ref-table)
  "Add ENTRY to table referenced in REF-TABLE."
  (let* ((table (duo-deref ref-table))
         (member (duo-member entry table)))
    (when (and entry (not member))
      (duo-ref-insert-in-sorted-list entry
                                     ref-table
                                     #'torus--generic-<))))

(defun torus--replace-entries (old-entry new-entry)
  "Replace entries of table variables.
Affected variables : `torus-helix', `torus-history'."
  (duo-replace old-entry new-entry (duo-deref torus-helix))
  (duo-replace old-entry new-entry (duo-deref ttorus-history)))

(defun torus--delete-file-entries (filename)
  "Delete entries matching FILENAME from table variables.
Affected variables : `torus-helix', `torus-history',
`torus-line-col', `torus-markers'."
  (let ((match-caar (lambda (duo arg) (equal (car (car duo)) arg)))
        (match-cadr (lambda (duo arg) (equal (car (cdr duo)) arg))))
    (duo-ref-delete-all filename torus-helix match-cadr)
    (duo-ref-delete-all filename ttorus-history match-cadr)
    (duo-ref-delete-all filename ttorus-line-col match-caar)
    (duo-ref-delete-all filename ttorus-markers match-caar)))

;;; Sync
;;; ------------------------------

(defun ttorus--update-position ()
  "Update position in current location.
Do nothing if file does not match current buffer.
Sync Emacs buffer state -> Torus state."
  (if (torus--empty-circle-p)
      (message "Can’t update location on an empty circle.")
    (let* ((old-location (car torus-cur-location))
           (file (car old-location))
           (old-position (cdr old-location))
           (new-position (point)))
      (when (and (not (equal new-position old-position))
                 (equal file (buffer-file-name (current-buffer))))
        (let* ((old-entry (torus--make-entry))
               (old-location-line-col (car (duo-assoc
                                            old-location
                                            (duo-deref ttorus-line-col))))
               (old-location-marker (car (duo-assoc
                                          old-location
                                          (duo-deref ttorus-markers))))
               (new-location (cons file new-position))
               (new-entry (torus--make-entry new-location))
               (new-marker (point-marker))
               (new-line-col (cons (line-number-at-pos) (current-column)))
               (new-location-line-col (cons new-location new-line-col))
               (new-location-marker (cons new-location new-marker)))
          (when (> torus-verbosity 1)
            (message "Updating position %s -> %s in file %s"
                     old-position
                     new-position
                     file))
          (torus--replace-entries old-entry new-entry)
          (duo-replace old-location-line-col
                       new-location-line-col
                       (duo-deref ttorus-line-col))
          (duo-replace old-location-marker
                       new-location-marker
                       (duo-deref ttorus-markers))
          ;; Do it in the end, otherwise it will not be found in helix & history
          (setcdr old-location new-position))))))

(defun ttorus--jump ()
  "Jump to current location (buffer & position) in torus.
Sync Torus state -> Emacs buffer state.
Add the location to `ttorus-markers' if not already present."
  (if (torus--empty-circle-p)
      (message "Can’t jump on an empty circle.")
    (let* ((location (car torus-cur-location))
           (location-marker (car (duo-assoc location
                                            (duo-deref ttorus-markers))))
           (marker (cdr location-marker))
           (buffer (when marker (marker-buffer marker))))
      (when (> torus-verbosity 2)
        (message "location %s location-marker %s" location location-marker)
        (message "marker %s buffer %s" marker buffer))
      (unless (buffer-live-p buffer)
        (duo-ref-delete location-marker ttorus-markers)
        (setq buffer nil))
      (if buffer
          (progn
            (when (> torus-verbosity 0)
              (message "Jumping to marker %s" marker))
            (unless (equal buffer (current-buffer))
              (switch-to-buffer buffer))
            (goto-char marker)
            (recenter))
        (pcase-let ((`(,filename . ,position) location)
                    (location-point-marker))
          (if (file-exists-p filename)
              (progn
                (when (> torus-verbosity 0)
                  (message "Opening file %s at %s" filename position))
                (find-file filename)
                (goto-char position)
                (recenter)
                (setq location-point-marker (cons location (point-marker)))
                (duo-ref-push-new location-point-marker ttorus-markers))
            (when (> torus-verbosity 0)
              (message "File %s does not exist. Deleting it from Torus." filename))
            (duo-ref-delete location (torus--ref-location-list))
            (torus--rewind-location)
            (torus--remove-index (torus--ref-circle))
            (torus--delete-file-entries filename)))))
    (torus--add-to-history)
    (torus--status-bar)))

;;; Window
;;; ------------------------------

(defsubst ttorus--windows ()
  "Windows displaying a ttorus buffer."
  (duo-filter (window-list) (lambda (elem) (ttorus--inside-p (window-buffer elem)))))

(defun ttorus--main-windows ()
  "Return main window of layout."
  (let ((windows (ttorus--windows)))
    (when windows
      (let* ((columns (mapcar #'window-text-width windows))
             (max-columns (eval `(max ,@columns)))
             (widest)
             (lines)
             (max-lines)
             (biggest))
        (dotimes (index (length windows))
          (when (equal (nth index columns) max-columns)
            (push (nth index windows) widest)))
        (setq lines (mapcar #'window-text-height widest))
        (setq max-lines (eval `(max ,@lines)))
        (dotimes (index (length widest))
          (when (equal (nth index lines) max-lines)
            (push (nth index widest) biggest)))
        biggest))))

;;; Split
;;; ------------------------------

(defun ttorus--prefix-argument-split (prefix)
  "Handle prefix argument PREFIX. Used to split."
  (pcase prefix
   ('(4)
    (split-window-below)
    (other-window 1))
   ('(16)
    (split-window-right)
    (other-window 1))))

;;; String
;;; ------------------------------

(defun torus--buffer-or-file-name (location)
  "Return buffer name of LOCATION if existent in `ttorus-markers', file basename otherwise."
  (unless (consp location)
    (error "Function torus--buffer-or-file-name : wrong type argument"))
  (let* ((bookmark (cdr (assoc location ttorus-markers)))
         (buffer (when bookmark
                   (marker-buffer bookmark))))
    (if buffer
        (buffer-name buffer)
      (file-name-nondirectory (car location)))))

(defun torus--position-string (location)
  "Return position in LOCATION in raw format or in line & column if available.
Line & Columns are stored in `ttorus-line-col'."
  (let ((entry (assoc location ttorus-line-col)))
    (if entry
        (format " at line %s col %s" (cadr entry) (cddr entry))
      (format " at position %s" (cdr location)))))

;;; String & Entry
;;; ---------------

(defun torus--entry-to-string (object)
  "Return OBJECT in concise string format.
Here are the returned strings, depending of the nature
of OBJECT :
string                             -> string
\((torus . circle) . (file . pos)) -> torus >> circle > file at pos
\(circle . (file . pos))           -> circle > file at Pos
\(file . position)                 -> file at position"
  (let ((location))
    (pcase object
      (`((,(and (pred stringp) ttorus) . ,(and (pred stringp) circle)) .
         (,(and (pred stringp) file) . ,(and (pred integerp) position)))
       (setq location (cons file position))
       (concat ttorus
               torus-separator-torus-circle
               circle
               torus-separator-circle-location
               (torus--buffer-or-file-name location)
               (torus--position-string location)))
      (`(,(and (pred stringp) circle) .
         (,(and (pred stringp) file) . ,(and (pred integerp) position)))
       (setq location (cons file position))
       (concat circle
               torus-separator-circle-location
               (torus--buffer-or-file-name location)
               (torus--position-string location)))
      (`(,(and (pred stringp) file) . ,(and (pred integerp) position))
       (setq location (cons file position))
       (concat (torus--buffer-or-file-name location)
               (torus--position-string location)))
      ((pred stringp) object)
      (_ (error "Function ttorus--concise : wrong type argument")))))

(defun torus--equal-string-entry-p (one two)
  "Whether the string representations of entries ONE and TWO are equal."
  (equal (torus--entry-to-string (torus--make-entry one))
         (torus--entry-to-string (torus--make-entry two))))

;;; Status bar
;;; ------------------------------

(defun torus--needle (&optional location)
  "Return LOCATION in short string format.
Shorter than concise. Used for dashboard and tabs."
  (let* ((cur-location (car torus-cur-location))
         (location (if location
                       location
                     cur-location))
         (entry (assoc location ttorus-line-col))
         (position (if entry
                       (format " : %s" (car (cdr entry)))
                     (format " . %s" (cdr location))))
         (needle (concat (torus--buffer-or-file-name location) position)))
    (when (equal location cur-location)
      (setq needle (concat "[*" needle "*]")))
    needle))

(defun ttorus--dashboard ()
  "Display summary of current torus, circle and location."
  (let ((torus (propertize (format (concat " %s"
                                           torus-separator-torus-circle)
                                   (torus--torus-name))
                           'keymap ttorus-map-mouse-torus))
        (circle (propertize (format (concat "%s"
                                            torus-separator-circle-location)
                                    (torus--circle-name))
                            'keymap ttorus-map-mouse-circle))
        (needles (mapcar #'torus--needle (torus--location-list)))
        (locations))
    (dolist (filepos needles)
      (setq locations (concat locations filepos torus-location-separator)))
    (setq locations (propertize locations 'keymap ttorus-map-mouse-location))
    (concat torus circle locations)))

(defun torus--status-bar ()
  "Display status bar, as tab bar or as info in echo area."
  (let* ((main-windows (ttorus--main-windows))
         (current-window (selected-window))
         (buffer (current-buffer))
         (original (assoc buffer ttorus-original-header-lines))
         (eval-tab '(:eval (ttorus--dashboard))))
    (if ttorus-display-tab-bar
        (when (member current-window main-windows)
          (unless original
            (push (cons buffer header-line-format)
                  ttorus-original-header-lines))
          (unless (equal header-line-format eval-tab)
            (setq header-line-format eval-tab)))
      (when original
        (setq header-line-format (cdr original))
        (setq ttorus-original-header-lines
              (ttorus--assoc-delete-all buffer
                                        ttorus-original-header-lines)))
      (message (substring-no-properties (ttorus--dashboard))))))

;;; Compatibility
;;; ------------------------------------------------------------

(defun torus--convert-version-1-variables ()
  "Convert version 1 variables format to new one."
  (when (and torus-meta
             torus-meta-history
             torus-input-history
             torus-layout )
    (torus-reset-menu ?a)
    ;; --- torus-meta -> torus-wheel ----
    (let ((meta (mapcar (lambda (elem)
                          (cons (car elem)
                                (cdr (car (duo-assoc "torus" (cdr elem))))))
                        torus-meta))
          (torus-name)
          (circle-name))
      (dolist (torus meta)
        (setq torus-name (car torus))
        (ttorus-add-torus torus-name)
        (dolist (circle (cdr torus))
          (setq circle-name (car circle))
          (ttorus-add-circle circle-name)
          (dolist (location (cdr circle))
            (ttorus-add-location location))
          (torus--rewind-location))
        (torus--rewind-circle)
        (torus--rewind-location))
      (torus--rewind-torus)
      (torus--rewind-circle)
      (torus--rewind-location))
    ;; --- Rebuild helix & grid ----
    ;; Not necessary : done by torus-add-{circle,location}
    ;; (torus--build-helix)
    ;; (torus--build-grid)
    ;; --- torus-meta-history -> torus-history ----
    (setq ttorus-history (list nil))
    (setq torus-cur-history nil)
    (let ((entry))
      (pcase-dolist (`(,location . (,circle-name . ,torus-name)) torus-meta-history)
        (setq entry (cons (cons torus-name circle-name) location))
        (torus--add-to-history entry)))
    (setq torus-cur-history (duo-ref-reverse ttorus-history))
    ;; --- torus-input-history -> torus-user-input-history ----
    (setq torus-user-input-history
          (list (apply #'append
                       (mapcar (lambda (elem)
                                 (cdr (assoc "input history" elem)))
                               torus-meta))))
    (setq torus-cur-user-input (duo-deref torus-user-input-history))
    ;; --- torus-layout -> torus-split-layout ----
    (let ((meta-layout (mapcar (lambda (elem)
                                 (cons (car elem)
                                       (cdr (car (duo-assoc "layout" (cdr elem))))))
                               torus-meta))
          (entry))
      (pcase-dolist (`(,torus-name . ,circle-layout-list) meta-layout)
        (pcase-dolist (`(,circle-name . ,layout) circle-layout-list)
          (unless (equal layout ?m)
            (setq entry (cons (cons torus-name circle-name) layout))
            (torus--add-entry-to-table entry torus-split-layout)))))
    ;; --- torus-line-col ----
    ;; Nothing to do
    ;; --- Unintern useless vars ----
    ;; (unintern "torus-meta")
    ;; (unintern "torus-meta-index")
    ;; (unintern "torus-meta-history")
    ;; (unintern "torus-torus")
    ;; (unintern "torus-history")
    ;; (unintern "torus-layout")
    ))

;;; Commands
;;; ------------------------------------------------------------

;;; Bindings
;;; ------------------------------

;;;###autoload
(defun ttorus-install-default-bindings ()
  "Install default keybindings."
  (interactive)
  ;; Keyboard
  (if (stringp torus-prefix-key)
      (global-set-key (kbd torus-prefix-key) 'ttorus-map)
    (global-set-key torus-prefix-key 'ttorus-map))
  (when (>= torus-binding-level 0)
    (define-key ttorus-map (kbd "a") 'ttorus-add-here)
    (define-key ttorus-map (kbd "C-a") 'ttorus-add-circle)
    (define-key ttorus-map (kbd "A") 'ttorus-add-torus)
    (define-key ttorus-map (kbd "s-a") 'ttorus-add-menu)
    (define-key ttorus-map (kbd "<left>") 'ttorus-previous-location)
    (define-key ttorus-map (kbd "<right>") 'ttorus-next-location)
    (define-key ttorus-map (kbd "<up>") 'ttorus-previous-circle)
    (define-key ttorus-map (kbd "<down>") 'ttorus-next-circle)
    (define-key ttorus-map (kbd "<S-up>") 'ttorus-previous-torus)
    (define-key ttorus-map (kbd "<S-down>") 'ttorus-next-torus)
    "Basic")
  (when (>= torus-binding-level 1)
    "Common")
  (when (>= torus-binding-level 2)
    "Advanced")
  (when (>= torus-binding-level 3)
    (define-key ttorus-map (kbd "p") 'torus-print-menu)
    (define-key ttorus-map (kbd "z") 'torus-reset-menu)
    "Debug")
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
  (define-key ttorus-map-mouse-location [header-line mouse-1] 'torus-mouse-on-tab)
  (define-key ttorus-map-mouse-location [header-line mouse-2] 'ttorus-alternate-in-meta)
  (define-key ttorus-map-mouse-location [header-line mouse-3] 'ttorus-switch-location)
  (define-key ttorus-map-mouse-location [header-line mouse-4] 'ttorus-previous-location)
  (define-key ttorus-map-mouse-location [header-line mouse-5] 'ttorus-next-location))

;;;###autoload
(defun torus-mouse-on-tab (event)
  "Manage click EVENT on locations part of tab line."
  (interactive "@e")
  (let* ((index (cdar (nthcdr 4 (cadr event))))
        (before (substring-no-properties
                 (caar (nthcdr 4 (cadr event))) 0 index))
        (pipes 0))
    (dotimes (index (length before))
      (when (equal (elt before index) ?|)
        (setq pipes (1+ pipes))))
    (if (equal pipes (torus--location-index))
        (ttorus-alternate-in-same-circle)
      (ttorus-switch-location (nth (length pipes) (cdar torus-cur-torus))))))

;;; Menus
;;; ------------------------------

;;;###autoload
(defun ttorus-add-menu (choice)
  "Autogroup according to CHOICE."
  (interactive
   (list (read-key ttorus--msg-add-menu)))
    (pcase choice
      (?h (call-interactively 'ttorus-add-here))
      (?f (call-interactively 'ttorus-add-file))
      (?b (call-interactively 'ttorus-add-buffer))
      (?l (call-interactively 'ttorus-add-location))
      (?c (call-interactively 'ttorus-add-circle))
      (?t (call-interactively 'ttorus-add-torus))
      (?\a (message "Add cancelled by Ctrl-G."))
      (_ (message "Invalid key."))))

;;;###autoload
(defun torus-print-menu (choice)
  "Print CHOICE variables."
  (interactive
   (list (read-key ttorus--msg-print-menu)))
  (let ((varlist)
        (window (view-echo-area-messages)))
    (pcase choice
      (?w (push 'torus-wheel varlist))
      (?t (push 'torus-cur-torus varlist))
      (?\^t (push 'torus-last-torus varlist))
      (?c (push 'torus-cur-circle varlist))
      (?\^c (push 'torus-last-circle varlist))
      (?l (push 'torus-cur-location varlist))
      (?\^l (push 'torus-last-location varlist))
      (?x (push 'torus-helix varlist))
      (?\^x (push 'torus-cur-helix varlist))
      (?g (push 'torus-grid varlist))
      (?G (push 'torus-cur-grid varlist))
      (?h (push 'ttorus-history varlist))
      (?\^h (push 'torus-cur-history varlist))
      (?u (push 'torus-user-input-history varlist))
      (?\^u (push 'torus-cur-user-input varlist))
      (?s (push 'torus-split-layout varlist))
      (?& (push 'ttorus-line-col varlist))
      (?m (push 'ttorus-markers varlist))
      (?o (push 'ttorus-original-header-lines varlist))
      (?a (setq varlist (list 'torus-wheel
                              'torus-cur-torus
                              'torus-last-torus
                              'torus-cur-circle
                              'torus-last-circle
                              'torus-cur-location
                              'torus-last-location
                              'torus-helix
                              'torus-cur-helix
                              'torus-grid
                              'torus-cur-grid
                              'ttorus-history
                              'torus-cur-history
                              'torus-user-input-history
                              'torus-cur-user-input
                              'torus-split-layout
                              'ttorus-line-col
                              'ttorus-markers
                              'ttorus-original-header-lines)))
      (?\a (delete-window window)
           (message "Print cancelled by Ctrl-G."))
      (_ (message "Invalid key.")))
    (dolist (var varlist)
      (message "%s" (symbol-name var))
      (pp (symbol-value var)))))

;;;###autoload
(defun torus-reset-menu (choice)
  "Reset CHOICE variables to nil."
  (interactive
   (list (read-key ttorus--msg-reset-menu)))
  (let ((list-nil-vars)
        (nil-vars))
    (pcase choice
      (?w (push 'torus-wheel list-nil-vars))
      (?t (push 'torus-cur-torus nil-vars))
      (?\^t (push 'torus-last-torus nil-vars))
      (?c (push 'torus-cur-circle nil-vars))
      (?\^c (push 'torus-last-circle nil-vars))
      (?l (push 'torus-cur-location nil-vars))
      (?\^l (push 'torus-last-location nil-vars))
      (?x (push 'torus-helix list-nil-vars))
      (?\^x (push 'torus-cur-helix nil-vars))
      (?g (push 'torus-grid list-nil-vars))
      (?G (push 'torus-cur-grid nil-vars))
      (?h (push 'ttorus-history list-nil-vars))
      (?\^h (push 'torus-cur-history nil-vars))
      (?u (push 'torus-user-input-history list-nil-vars))
      (?\^u (push 'torus-cur-user-input nil-vars))
      (?s (push 'torus-split-layout list-nil-vars))
      (?& (push 'ttorus-line-col list-nil-vars))
      (?m (push 'ttorus-markers list-nil-vars))
      (?o (push 'ttorus-original-header-lines list-nil-vars))
      (?a (setq list-nil-vars (list 'torus-wheel
                                    'torus-helix
                                    'torus-grid
                                    'ttorus-history
                                    'torus-user-input-history
                                    'torus-split-layout
                                    'ttorus-line-col
                                    'ttorus-markers
                                    'ttorus-original-header-lines))
          (setq nil-vars (list 'torus-cur-torus
                               'torus-last-torus
                               'torus-cur-circle
                               'torus-last-circle
                               'torus-cur-location
                               'torus-last-location
                               'torus-cur-helix
                               'torus-cur-grid
                               'torus-cur-history
                               'torus-cur-user-input)))
      (?\a (message "Reset cancelled by Ctrl-G."))
      (_ (message "Invalid key.")))
    (dolist (var list-nil-vars)
      (message "%s -> (list nil)" (symbol-name var))
      (set var (list nil)))
    (dolist (var nil-vars)
      (message "%s -> nil" (symbol-name var))
      (set var nil))))

;;; Read & Write
;;; ------------------------------



;;; Add
;;; ------------------------------

;;;###autoload
(defun ttorus-add-torus (torus-name)
  "Create a new torus named TORUS-NAME in `torus-wheel'."
  (interactive
   (list (read-string "Name of the new torus : "
                      nil
                      'torus-cur-user-input)))
  (torus--add-user-input torus-name)
  (let* ((torus (torus--tree-template torus-name))
         (return (duo-ref-add-new torus
                                  (torus--ref-torus-list)
                                  torus-last-torus
                                  #'duo-equal-car-p)))
    (if return
        (progn
          (setq torus-cur-torus return)
          (setq torus-last-torus return)
          (torus--add-index (torus--ref-torus-list))
          (torus--set-nil-circle)
          (torus--set-nil-location)
          torus-cur-torus)
      (message "Torus %s is already present in Torus Wheel." torus-name)
      nil)))

;;;###autoload
(defun ttorus-add-circle (circle-name)
  "Add a new circle CIRCLE-NAME to current torus."
  (interactive
   (list
    (read-string "Name of the new circle : "
                 nil
                 'torus-cur-user-input)))
  (unless torus-cur-torus
    (call-interactively 'ttorus-add-torus))
  (torus--add-user-input circle-name)
  (let ((circle (torus--tree-template circle-name))
        (torus-name (torus--torus-name))
        (return))
    (setq return (duo-ref-add-new circle
                                  (torus--ref-circle-list)
                                  torus-last-circle
                                  #'duo-equal-car-p))
    (if return
        (progn
          (setq torus-cur-circle return)
          (setq torus-last-circle return)
          (torus--add-index (torus--ref-circle-list))
          (torus--add-to-grid)
          (torus--set-nil-location)
          torus-cur-circle)
      (message "Circle %s is already present in Torus %s."
               circle-name
               torus-name)
      nil)))

;;;###autoload
(defun ttorus-add-location (location)
  "Add LOCATION to current circle."
  (interactive
   (list
    (read-string "New location : "
                 nil
                 'torus-cur-user-input)))
  (unless torus-cur-torus
    (call-interactively 'ttorus-add-torus))
  (unless torus-cur-circle
    (call-interactively 'ttorus-add-circle))
  (torus--add-user-input (prin1-to-string location))
  (let* ((location (if (consp location)
                       location
                     (car (read-from-string location))))
         (member (duo-member location (torus--location-list))))
    (if member
        (progn
          (message "Location %s is already present in Torus %s Circle %s."
                   location
                   (torus--torus-name)
                   (torus--circle-name))
          nil)
      (setq torus-last-location (duo-ref-add location
                                             (torus--ref-location-list)
                                             torus-last-location))
      (setq torus-cur-location torus-last-location)
      (torus--add-index (torus--ref-location-list))
      (torus--add-to-helix)
      (torus--add-to-history)
      torus-cur-location)))

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
        (torus--add-entry-to-table location-line-col ttorus-line-col)
        (duo-ref-push-new location-marker ttorus-markers)
        (torus--status-bar)
        torus-cur-location)
    (message "Buffer must have a filename to be added to the torus.")
    nil))

;;;###autoload
(defun ttorus-add-file (file-name)
  "Add FILE-NAME to the current circle."
  (interactive (list (read-file-name "File to add : ")))
  (if (file-exists-p file-name)
      (progn
        (find-file file-name)
        (ttorus-add-here))
    (message "File %s does not exist." file-name)
    nil))

;;;###autoload
(defun ttorus-add-buffer (buffer-name)
  "Add BUFFER-NAME at current position to the current circle."
  (interactive (list (read-buffer "File to add : ")))
  (switch-to-buffer buffer-name)
  (ttorus-add-here))

;;; Previous / Next
;;; ------------------------------

;;;###autoload
(defun ttorus-previous-torus ()
  "Jump to the previous ttorus."
  (interactive)
  (if (torus--empty-wheel-p)
      (message ttorus--msg-empty-wheel)
    (setq torus-cur-torus
          (duo-circ-previous torus-cur-torus (torus--torus-list)))
    (ttorus--update-position)
    (torus--decrease-index (torus--ref-torus-list))
    (torus--seek-circle)
    (torus--seek-location)
    (ttorus--jump))
  torus-cur-torus)

;;;###autoload
(defun ttorus-next-torus ()
  "Jump to the next ttorus."
  (interactive)
  (if (torus--empty-wheel-p)
      (message ttorus--msg-empty-wheel)
    (ttorus--update-position)
    (setq torus-cur-torus
          (duo-circ-next torus-cur-torus (torus--torus-list)))
    (torus--increase-index (torus--ref-torus-list))
    (torus--seek-circle)
    (torus--seek-location)
    (ttorus--jump))
  torus-cur-torus)

;;;###autoload
(defun ttorus-previous-circle ()
  "Jump to the previous circle."
  (interactive)
  (if (torus--empty-torus-p)
      (message ttorus--msg-empty-torus (torus--torus-name))
    (ttorus--update-position)
    (setq torus-cur-circle
          (duo-circ-previous torus-cur-circle
                             (torus--circle-list)))
    (torus--decrease-index (torus--ref-circle-list))
    (torus--seek-location)
    (ttorus--jump))
  torus-cur-circle)

;;;###autoload
(defun ttorus-next-circle ()
  "Jump to the next circle."
  (interactive)
  (if (torus--empty-torus-p)
      (message ttorus--msg-empty-torus (torus--torus-name))
    (ttorus--update-position)
    (setq torus-cur-circle
          (duo-circ-next torus-cur-circle
                         (torus--circle-list)))
    (torus--increase-index (torus--ref-circle-list))
    (torus--seek-location)
    (ttorus--jump))
  torus-cur-circle)

;;;###autoload
(defun ttorus-previous-location ()
  "Jump to the previous location."
  (interactive)
  (if (torus--empty-circle-p)
      (message ttorus--msg-empty-circle
               (torus--circle-name)
               (torus--torus-name))
    (ttorus--update-position)
    (setq torus-cur-location
          (duo-circ-previous torus-cur-location
                             (torus--location-list)))
    (torus--decrease-index (torus--ref-location-list))
    (ttorus--jump))
  torus-cur-location)

;;;###autoload
(defun ttorus-next-location ()
  "Jump to the next location."
  (interactive)
  (if (torus--empty-circle-p)
      (message ttorus--msg-empty-circle
               (torus--circle-name)
               (torus--torus-name))
    (ttorus--update-position)
    (setq torus-cur-location
          (duo-circ-next torus-cur-location
                         (torus--location-list)))
    (torus--increase-index (torus--ref-location-list))
    (ttorus--jump))
  torus-cur-location)

;;; ============================================================
;;; From here, it’s a mess
;;; ============================================================

;;; Switch
;;; ------------------------------

;;;###autoload
(defun ttorus-switch-location (location-name)
  "Jump to LOCATION-NAME location in current circle and torus.
With prefix argument \\[universal-argument], open the buffer in a
horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument], open the
buffer in a vertical split."
  (interactive
   (list
    (completing-read
     "Go to location : "
     (mapcar #'ttorus--concise (cdr (car torus-cur-torus))) nil t)))
  (ttorus--prefix-argument-split current-prefix-arg)
  (ttorus--update-position)
  (let* ((circle (cdr (car torus-cur-torus)))
         (index (cl-position location-name circle
                          :test #'ttorus--equal-concise-p))
         (before (cl-subseq circle 0 index))
         (after (cl-subseq circle index)))
    (setcdr (car torus-cur-torus) (append after before)))
  (ttorus--jump))

;;; Tables
;;; ------------------------------

(defun ttorus--narrow-to-torus (&optional ttorus-name index)
  "Narrow an index-like table to entries of TTORUS-NAME.
Argument TTORUS-NAME nil means narrow to current ttorus.
Argument INDEX nil means using `torus-helix'.
Can be used with `torus-helix' and `ttorus-history'."
  (let ((index (if index
                   index
                 torus-helix))
        (ttorus-name (if ttorus-name
                        ttorus-name
                      (car (car torus-cur-torus)))))
    (seq-filter (lambda (elem) (equal (caar elem) ttorus-name))
                index)))

(defun ttorus--narrow-to-circle (&optional ttorus-name circle-name index)
  "Narrow an index-like table to entries of TTORUS-NAME and CIRCLE-NAME.
Argument TTORUS-NAME nil means narrow using current ttorus.
Argument CIRCLE-NAME nil means narrow to current circle.
Argument INDEX nil means using `torus-helix'.
Can be used with `torus-helix' and `ttorus-history'."
  (let ((index (if index
                   index
                 torus-helix))
        (ttorus-name (if ttorus-name
                        ttorus-name
                      (car (car torus-cur-torus))))
        (circle-name (if circle-name
                         circle-name
                       (car (car torus-cur-circle)))))
    (seq-filter (lambda (elem) (and (equal (caar elem) ttorus-name)
                               (equal (cdar elem) circle-name)))
                index)))

(defun ttorus--complete-and-clean-layout ()
  "Fill `torus-split-layout' from missing elements. Delete useless ones."
  (let ((paths (mapcar #'car torus-helix)))
    (delete-dups paths)
    (dolist (elem paths)
      (unless (assoc elem torus-split-layout)
        (push (cons elem ?m) torus-split-layout)))
    (dolist (elem torus-split-layout)
      (unless (member (car elem) paths)
        (setq torus-split-layout (ttorus--assoc-delete-all (car elem) torus-split-layout))))
    (setq torus-split-layout (reverse torus-split-layout))))

(defun ttorus--apply-or-push-layout ()
  "Apply layout of current circle, or add default is not present."
  (let* ((path (cons (car (car torus-cur-torus))
                     (car (car torus-cur-circle))))
         (entry (assoc path torus-split-layout)))
    (if entry
        (torus-split-layout-menu (cdr entry))
      (push (cons path ?m) torus-split-layout))))

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
         (circle (assoc circle-name torus-cur-torus))
         (index (cl-position circle torus-cur-torus :test #'equal))
         (before (cl-subseq torus-cur-torus 0 index))
         (after (cl-subseq torus-cur-torus index)))
    (if index
        (setq torus-cur-torus (append after before))
      (message "Circle not found.")))
  (let* ((circle (cdr (car torus-cur-torus)))
         (location (car location-circle))
         (index (cl-position location circle :test #'equal))
         (before (cl-subseq circle 0 index))
         (after (cl-subseq circle index)))
    (if index
        (setcdr (car torus-cur-torus) (append after before))
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
  (when (> torus-verbosity 2)
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
  (setq torus-helix (ttorus--build-helix))
  (ttorus--complete-and-clean-layout)
  (let* ((circle-name (cdar entry))
         (circle (assoc circle-name torus-cur-torus))
         (index (cl-position circle torus-cur-torus :test #'equal))
         (before (cl-subseq torus-cur-torus 0 index))
         (after (cl-subseq torus-cur-torus index)))
    (if index
        (setq torus-cur-torus (append after before))
      (message "Circle not found.")))
  (let* ((circle (cdr (car torus-cur-torus)))
         (location (cdr entry))
         (index (cl-position location circle :test #'equal))
         (before (cl-subseq circle 0 index))
         (after (cl-subseq circle index)))
    (if index
        (setcdr (car torus-cur-torus) (append after before))
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
                    (concat prefix torus-prefix-separator (car elem))))
          (dolist (elem history)
            (setcdr elem
                    (concat prefix torus-prefix-separator (cdr elem)))))
      (message "Prefix is blank"))
    (list ttorus history)))

;;; Files
;;; ------------------------------

(defun ttorus--roll-backups (filename)
  "Roll backups of FILENAME."
  (unless (stringp filename)
    (error "Function ttorus--roll-backups : wrong type argument"))
  (let ((file-list (list filename))
        (file-src)
        (file-dest))
    (dolist (iter (number-sequence 1 torus-backup-number))
      (push (concat filename "." (prin1-to-string iter)) file-list))
    (while (> (length file-list) 1)
      (setq file-dest (pop file-list))
      (setq file-src (car file-list))
      (when (> torus-verbosity 2)
        (message "files %s %s" file-src file-dest))
      (when (and file-src (file-exists-p file-src))
        (when (> torus-verbosity 2)
          (message "copy %s -> %s" file-src file-dest))
        (copy-file file-src file-dest t)))))

;;; Hooks & Advices
;;; ------------------------------------------------------------

;;;###autoload
(defun ttorus-quit ()
  "Write ttorus before quit."
  (when ttorus-save-on-exit
    (if torus-autowrite-file
        ;; TODO : complete path by torus-dirname if necessary
        (ttorus-write torus-autowrite-file)
      (when (y-or-n-p "Write ttorus ? ")
        (call-interactively 'ttorus-write))))
  ;; To be sure they will be nil at startup, even if some plugin saved
  ;; global variables
  (torus-reset-menu ?a))

;;;###autoload
(defun ttorus-start ()
  "Read ttorus on startup."
  (when ttorus-load-on-startup
    (if torus-autoread-file
        ;; TODO : complete path by torus-dirname if necessary
        (ttorus-read torus-autoread-file)
      (message "Set torus-autoread-file if you want to load it."))))

;;;###autoload
(defun ttorus-after-save-torus-file ()
  "Ask whether to read ttorus file after edition."
  (let* ((filename (buffer-file-name (current-buffer)))
         (directory (file-name-directory filename))
         (ttorus-dir (expand-file-name (file-name-as-directory torus-dirname))))
    (when (> torus-verbosity 2)
      (message "filename : %s" filename)
      (message "filename directory : %s" directory)
      (message "ttorus directory : %s" ttorus-dir))
    (when (equal directory ttorus-dir)
      (when (y-or-n-p "Apply changes to current ttorus variables ? ")
        (ttorus-read filename)))))

;;;###autoload
(defun ttorus-advice-switch-buffer (&rest args)
  "Advice to `switch-to-buffer'. ARGS are irrelevant."
  (when (> torus-verbosity 2)
    (message "Advice called with args %s" args))
  (when (and torus-cur-torus (ttorus--inside-p))
    (ttorus--update-position)))

;;; Commands
;;; ------------------------------------------------------------

;;;###autoload
(defun ttorus-init ()
  "Initialize ttorus. Add hooks and advices.
Create `torus-dirname' if needed."
  (interactive)
  (add-hook 'emacs-startup-hook 'ttorus-start)
  (add-hook 'kill-emacs-hook 'ttorus-quit)
  (add-hook 'after-save-hook 'ttorus-after-save-torus-file)
  (advice-add #'switch-to-buffer :before #'ttorus-advice-switch-buffer)
  (unless (file-exists-p torus-dirname)
    (make-directory torus-dirname)))

;;; Print
;;; ------------------------------

;;;###autoload
(defun ttorus-info ()
  "Print local info : circle name and locations."
  (interactive)
  (message (ttorus--dashboard)))

;;; Add
;;; ------------------------------

;;;###autoload
(defun ttorus-add-copy-of-torus (ttorus-name)
  "Create a new ttorus named TTORUS-NAME as copy of the current ttorus."
  (interactive
   (list (read-string "Name of the new ttorus : "
                      nil
                      'torus-user-input-history)))
  (setq torus-cur-torus (copy-tree torus-cur-torus))
  (if torus-cur-torus
      (setcar torus-cur-torus ttorus-name)
    (setq torus-cur-torus (list ttorus-name)))
  (if torus-wheel
      (duo-add-new torus-cur-torus torus-wheel)
    (setq torus-wheel (list torus-cur-torus))))

;;; Navigate
;;; ------------------------------

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
          (mapcar #'car torus-cur-torus) nil t)))
  (ttorus--prefix-argument-split current-prefix-arg)
  (ttorus--update-position)
  (let* ((circle (assoc circle-name torus-cur-torus))
         (index (cl-position circle torus-cur-torus :test #'equal))
         (before (cl-subseq torus-cur-torus 0 index))
         (after (cl-subseq torus-cur-torus index)))
    (setq torus-cur-torus (append after before)))
  (ttorus--jump)
  (ttorus--apply-or-push-layout))

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
  (setq torus-helix (ttorus--build-helix))
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
     (mapcar #'ttorus--concise torus-helix) nil t)))
  (ttorus--prefix-argument-split current-prefix-arg)
  (let* ((entry
          (cl-find
           name torus-helix
           :test #'ttorus--equal-concise-p)))
    (ttorus--meta-switch entry)))

;;; History
;;; ------------------------------

;;;###autoload
(defun ttorus-history-newer ()
  "Go to newer location in history."
  (interactive)
  (if torus-cur-torus
      (progn
        (ttorus--prefix-argument-split current-prefix-arg)
        (if ttorus-old-history
            (progn
              (setq ttorus-old-history (append (last ttorus-old-history) (butlast ttorus-old-history)))
              (ttorus--switch (car ttorus-old-history)))
          (message "History is empty.")))
    (message ttorus--msg-empty-torus)))

;;;###autoload
(defun ttorus-history-older ()
  "Go to older location in history."
  (interactive)
  (if torus-cur-torus
      (progn
        (ttorus--prefix-argument-split current-prefix-arg)
        (if ttorus-old-history
            (progn
              (setq ttorus-old-history (append (cdr ttorus-old-history) (list (car ttorus-old-history))))
              (ttorus--switch (car ttorus-old-history)))
          (message "History is empty.")))
    (message ttorus--msg-empty-torus)))

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
    (message ttorus--msg-empty-wheel)))

;;;###autoload
(defun ttorus-alternate-in-same-torus ()
  "Alternate last two locations in history belonging to the current circle.
If outside the ttorus, just return inside, to the last ttorus location."
  (interactive)
  (if torus-cur-torus
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
    (message ttorus--msg-empty-torus)))

;;;###autoload
(defun ttorus-alternate-in-same-circle ()
  "Alternate last two locations in history belonging to the current circle.
If outside the ttorus, just return inside, to the last ttorus location."
  (interactive)
  (if torus-cur-torus
      (progn
        (ttorus--prefix-argument-split current-prefix-arg)
        (if (ttorus--inside-p)
            (if (and ttorus-old-history
                     (>= (length ttorus-old-history) 2))
                (progn
                  (ttorus--update-meta)
                  (let ((history ttorus-old-history)
                        (circle (car (car torus-cur-torus)))
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
    (message ttorus--msg-empty-torus)))

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
  (if torus-cur-torus
      (progn
        (ttorus--prefix-argument-split current-prefix-arg)
        (if (ttorus--inside-p)
            (if (and ttorus-old-history
                     (>= (length ttorus-old-history) 2))
                (progn
                  (ttorus--update-meta)
                  (let ((history ttorus-old-history)
                        (circle (car (car torus-cur-torus)))
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
    (message ttorus--msg-empty-torus)))

;;;###autoload
(defun ttorus-alternate-menu (choice)
  "Alternate according to CHOICE."
  (interactive
   (list (read-key ttorus--msg-alternate-menu)))
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
  (if torus-cur-torus
      (let*
          ((old-name (car (car torus-cur-torus)))
           (prompt (format "New name of circle %s : " old-name))
           (circle-name (read-string prompt nil 'torus-user-input-history)))
        (ttorus--update-input-history circle-name)
        (setcar (car torus-cur-torus) circle-name)
        (dolist (location-circle ttorus-table)
          (when (equal (cdr location-circle) old-name)
            (setcdr location-circle circle-name)))
        (dolist (location-circle ttorus-old-history)
          (when (equal (cdr location-circle) old-name)
            (setcdr location-circle circle-name)))
        (dolist (location-circle-torus ttorus-history)
          (when (equal (cadr location-circle-torus) old-name)
            (setcar (cdr location-circle-torus) circle-name)))
        (dolist (location-circle-torus torus-helix)
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
           (ttorus-name (read-string prompt nil 'torus-user-input-history)))
        (ttorus--update-input-history ttorus-name)
        (setcar (car ttorus-meta) ttorus-name)
        (message "Renamed ttorus %s -> %s" old-name ttorus-name))
    (message ttorus--msg-empty-wheel)))

;;; Move
;;; ------------------------------

;;;###autoload
(defun ttorus-move-circle (circle-name)
  "Move current circle after CIRCLE-NAME."
  (interactive
   (list (completing-read
          "Move current circle after : "
          (mapcar #'car torus-cur-torus) nil t)))
  (ttorus--update-position)
  (let* ((circle (assoc circle-name torus-cur-torus))
         (index (1+ (cl-position circle torus-cur-torus :test #'equal)))
         (current (list (car torus-cur-torus)))
         (before (cl-subseq torus-cur-torus 1 index))
         (after (cl-subseq torus-cur-torus index)))
    (setq torus-cur-torus (append before current after))
    (ttorus-switch-circle (caar current))))

;;;###autoload
(defun ttorus-move-location (location-name)
  "Move current location after LOCATION-NAME."
  (interactive
   (list
    (completing-read
     "Move current location after : "
     (mapcar #'ttorus--concise (cdr (car torus-cur-torus))) nil t)))
  (ttorus--update-position)
  (let* ((circle (cdr (car torus-cur-torus)))
         (index (1+ (cl-position location-name circle
                                 :test #'ttorus--equal-concise-p)))
         (current (list (car circle)))
         (before (cl-subseq circle 1 index))
         (after (cl-subseq circle index)))
    (setcdr (car torus-cur-torus) (append before current after))
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
          (mapcar #'car torus-cur-torus) nil t)))
  (ttorus--update-position)
  (let* ((location (car (cdr (car torus-cur-torus))))
         (circle (cdr (assoc circle-name torus-cur-torus)))
         (old-name (car (car torus-cur-torus)))
         (old-pair (cons location old-name)))
    (if (member location circle)
        (message "Location %s already exists in circle %s."
                 (ttorus--concise location)
                 circle-name)
      (message "Moving location %s to circle %s."
               (ttorus--concise location)
               circle-name)
      (pop (cdar torus-cur-torus))
      (setcdr (assoc circle-name torus-cur-torus)
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
  (let* ((circle (cl-copy-seq (car torus-cur-torus)))
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
      (when (> torus-verbosity 2)
        (message "circle-torus %s" circle-torus))
      (setcdr (assoc "ttorus" (assoc ttorus-name ttorus-meta))
              (push circle ttorus))
      (setq torus-cur-torus (ttorus--assoc-delete-all circle-name torus-cur-torus))
      (setq ttorus-table
            (ttorus--reverse-assoc-delete-all circle-name ttorus-table))
      (setq ttorus-old-history
            (ttorus--reverse-assoc-delete-all circle-name ttorus-old-history))
      (setq ttorus-markers
            (ttorus--reverse-assoc-delete-all circle-name ttorus-markers))
      (setq torus-helix
            (ttorus--reverse-assoc-delete-all circle-torus torus-helix))
      (setq ttorus-history
            (ttorus--reverse-assoc-delete-all circle-torus ttorus-history))
      (ttorus--build-table)
      (setq torus-helix (ttorus--build-helix))
      (ttorus--jump))))

;;;###autoload
(defun ttorus-copy-location-to-circle (circle-name)
  "Copy current location to CIRCLE-NAME."
  (interactive
   (list (completing-read
          "Copy current location to circle : "
          (mapcar #'car torus-cur-torus) nil t)))
  (ttorus--update-position)
  (let* ((location (car (cdr (car torus-cur-torus))))
         (circle (cdr (assoc circle-name torus-cur-torus))))
    (if (member location circle)
        (message "Location %s already exists in circle %s."
                 (ttorus--concise location)
                 circle-name)
      (message "Copying location %s to circle %s."
                 (ttorus--concise location)
                 circle-name)
      (setcdr (assoc circle-name torus-cur-torus) (push location circle))
      (ttorus--build-table)
      (setq torus-helix (ttorus--build-helix)))))

;;;###autoload
(defun ttorus-copy-circle-to-torus (ttorus-name)
  "Copy current circle to TTORUS-NAME."
  (interactive
   (list (completing-read
          "Copy current circle to ttorus : "
          (mapcar #'car ttorus-meta) nil t)))
  (ttorus--update-position)
  (let* ((circle (cl-copy-seq (car torus-cur-torus)))
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
    (setq torus-helix (ttorus--build-helix))))

;;; Reverse
;;; ------------------------------

;;;###autoload
(defun ttorus-reverse-circles ()
  "Reverse order of the circles."
  (interactive)
  (ttorus--update-position)
  (setq torus-cur-torus (reverse torus-cur-torus))
  (ttorus--jump))

;;;###autoload
(defun ttorus-reverse-locations ()
  "Reverse order of the locations in the current circles."
  (interactive)
  (ttorus--update-position)
  (setcdr (car torus-cur-torus) (reverse (cdr (car torus-cur-torus))))
  (ttorus--jump))

;;;###autoload
(defun ttorus-deep-reverse ()
  "Reverse order of the locations in each circle."
  (interactive)
  (ttorus--update-position)
  (setq torus-cur-torus (reverse torus-cur-torus))
  (dolist (circle torus-cur-torus)
    (setcdr circle (reverse (cdr circle))))
  (ttorus--jump))


;;;###autoload
(defun ttorus-reverse-menu (choice)
  "Split according to CHOICE."
  (interactive
   (list (read-key ttorus--msg-reverse-menu)))
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
  "Add PREFIX to circle names of `torus-cur-torus'."
  (interactive
   (list
    (read-string (format ttorus--msg-prefix-circle
                         (car (car ttorus-meta)))
                 nil
                 'torus-user-input-history)))
  (let ((varlist))
    (setq varlist (ttorus--prefix-circles prefix (car (car ttorus-meta))))
    (setq torus-cur-torus (car varlist))
    (setq ttorus-old-history (car (cdr varlist))))
  (ttorus--build-table)
  (setq torus-helix (ttorus--build-helix)))

;;;###autoload
(defun ttorus-join-circles (circle-name)
  "Join current circle with CIRCLE-NAME."
  (interactive
   (list
    (completing-read "Join current circle with circle : "
                     (mapcar #'car torus-cur-torus) nil t)))
  (let* ((current-name (car (car torus-cur-torus)))
         (join-name (concat current-name torus-join-separator circle-name))
         (user-choice
          (read-string (format "Name of the joined ttorus [%s] : " join-name))))
    (when (> (length user-choice) 0)
      (setq join-name user-choice))
    (ttorus-add-circle join-name)
    (setcdr (car torus-cur-torus)
            (append (cdr (assoc current-name torus-cur-torus))
                    (cdr (assoc circle-name torus-cur-torus))))
    (delete-dups (cdr (car torus-cur-torus))))
  (ttorus--update-meta)
  (ttorus--build-table)
  (setq torus-helix (ttorus--build-helix))
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
         (join-name (concat current-name torus-join-separator ttorus-name))
         (user-choice
          (read-string (format "Name of the joined ttorus [%s] : " join-name)))
         (prompt-current
          (format ttorus--msg-prefix-circle current-name))
         (prompt-added
          (format ttorus--msg-prefix-circle ttorus-name))
         (prefix-current
          (read-string prompt-current nil 'torus-user-input-history))
         (prefix-added
          (read-string prompt-added nil 'torus-user-input-history))
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
    (if (seq-intersection torus-cur-torus ttorus-added #'ttorus--equal-car-p)
        (message ttorus--msg-circle-name-collision)
      (setq torus-cur-torus (append torus-cur-torus ttorus-added))
      (setq ttorus-old-history (append ttorus-old-history history-added))
      (setq torus-user-input-history (append torus-user-input-history input-added))))
  (ttorus--update-meta)
  (ttorus--build-table)
  (setq torus-helix (ttorus--build-helix))
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
                      'torus-user-input-history))
        (all-locations))
    (if (assoc ttorus-name ttorus-meta)
        (message "ttorus %s already exists in ttorus-meta" ttorus-name)
      (ttorus-add-copy-of-torus ttorus-name)
      (dolist (circle torus-cur-torus)
        (dolist (location (cdr circle))
          (push location all-locations)))
      (setq torus-cur-torus (seq-group-by quoted-function all-locations))))
  (setq ttorus-old-history nil)
  (setq ttorus-markers nil)
  (setq torus-user-input-history nil)
  (ttorus--build-table)
  (setq torus-helix (ttorus--build-helix))
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
  (ttorus-autogroup #'torus--directory))

;;;###autoload
(defun ttorus-autogroup-by-extension ()
  "Autogroup all location of the ttorus by extension.
A new ttorus is created to contain the new circles."
  (interactive)
  (ttorus-autogroup #'torus--extension-description))

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
   (list (read-key ttorus--msg-autogroup-menu)))
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
  (dolist (iter (number-sequence 1 (length (cdar torus-cur-torus))))
    (when (> torus-verbosity 1)
      (message "%d. Applying %s to %s" iter elisp-code (cadar torus-cur-torus))
      (message "Evaluated : %s"
               (car (read-from-string (format "(progn %s)" elisp-code)))))
    (torus--eval-string elisp-code)
    (ttorus-next-location)))

;;;###autoload
(defun ttorus-run-elisp-command-on-circle (command)
  "Run an Emacs Lisp COMMAND to all files of the circle."
  (interactive (list (read-command
                      "Elisp command to run to all files of the circle : ")))
  (dolist (iter (number-sequence 1 (length (cdar torus-cur-torus))))
    (when (> torus-verbosity 1)
      (message "%d. Applying %s to %s" iter command (cadar torus-cur-torus)))
    (funcall command)
    (ttorus-next-location)))

;;;###autoload
(defun ttorus-run-shell-command-on-circle (command)
  "Run a shell COMMAND to all files of the circle."
  (interactive (list (read-string
                      "Shell command to run to all files of the circle : ")))
  (let ((keep-value shell-command-dont-erase-buffer))
    (setq shell-command-dont-erase-buffer t)
    (dolist (iter (number-sequence 1 (length (cdar torus-cur-torus))))
      (when (> torus-verbosity 1)
        (message "%d. Applying %s to %s" iter command (cadar torus-cur-torus)))
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
    (dolist (iter (number-sequence 1 (length (cdar torus-cur-torus))))
      (when (> torus-verbosity 1)
        (message "%d. Applying %s to %s" iter command (cadar torus-cur-torus)))
      (async-shell-command (format "%s %s"
                             command
                             (shell-quote-argument (buffer-file-name))))
      (ttorus-next-location))
    (setq async-shell-command-buffer keep-value)))

;;;###autoload
(defun ttorus-batch-menu (choice)
  "Split according to CHOICE."
  (interactive
   (list (read-key ttorus--msg-batch-menu)))
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
Split until `torus-maximum-horizontal-split' is reached."
  (interactive)
  (let* ((circle (cdr (car torus-cur-torus)))
         (numsplit (1- (length circle))))
    (when (> torus-verbosity 1)
      (message "numsplit = %d" numsplit))
    (if (> numsplit (1- torus-maximum-horizontal-split))
        (message "Too many files to split.")
      (delete-other-windows)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> torus-verbosity 2)
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
Split until `torus-maximum-vertical-split' is reached."
  (interactive)
  (let* ((circle (cdr (car torus-cur-torus)))
         (numsplit (1- (length circle))))
    (when (> torus-verbosity 1)
      (message "numsplit = %d" numsplit))
    (if (> numsplit (1- torus-maximum-vertical-split))
        (message "Too many files to split.")
      (delete-other-windows)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> torus-verbosity 2)
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
  (let* ((circle (cdr (car torus-cur-torus)))
         (numsplit (- (length circle) 2)))
    (when (> torus-verbosity 1)
      (message "numsplit = %d" numsplit))
    (if (> numsplit (1- torus-maximum-horizontal-split))
        (message "Too many files to split.")
      (delete-other-windows)
      (split-window-right)
      (other-window 1)
      (ttorus-next-location)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> torus-verbosity 2)
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
  (let* ((circle (cdr (car torus-cur-torus)))
         (numsplit (- (length circle) 2)))
    (when (> torus-verbosity 1)
      (message "numsplit = %d" numsplit))
    (if (> numsplit (1- torus-maximum-horizontal-split))
        (message "Too many files to split.")
      (delete-other-windows)
      (split-window-right)
      (ttorus-next-location)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> torus-verbosity 2)
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
  (let* ((circle (cdr (car torus-cur-torus)))
         (numsplit (- (length circle) 2)))
    (when (> torus-verbosity 1)
      (message "numsplit = %d" numsplit))
    (if (> numsplit (1- torus-maximum-vertical-split))
        (message "Too many files to split.")
      (delete-other-windows)
      (split-window-below)
      (other-window 1)
      (ttorus-next-location)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> torus-verbosity 2)
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
  (let* ((circle (cdr (car torus-cur-torus)))
         (numsplit (- (length circle) 2)))
    (when (> torus-verbosity 1)
      (message "numsplit = %d" numsplit))
    (if (> numsplit (1- torus-maximum-vertical-split))
        (message "Too many files to split.")
      (delete-other-windows)
      (split-window-below)
      (ttorus-next-location)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> torus-verbosity 2)
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
  (let* ((circle (cdr (car torus-cur-torus)))
         (len-circle (length circle))
         (max-iter (1- len-circle))
         (ratio (/ (float (frame-text-width))
                   (float (frame-text-height))))
         (horizontal (sqrt (/ (float len-circle) ratio)))
         (vertical (* ratio horizontal))
         (int-hor (min (ceiling horizontal)
                       torus-maximum-horizontal-split))
         (int-ver (min (ceiling vertical)
                       torus-maximum-vertical-split))
         (getout)
         (num-hor-minus)
         (num-hor)
         (num-ver-minus)
         (total 0))
    (if (< (* int-hor int-ver) len-circle)
        (message "Too many files to split.")
      (let ((dist-dec-hor)
            (dist-dec-ver))
        (when (> torus-verbosity 2)
          (message "ratio = %f" ratio)
          (message "horizontal = %f" horizontal)
          (message "vertical = %f" vertical)
          (message "int-hor int-ver = %d %d" int-hor  int-ver))
        (while (not getout)
          (setq dist-dec-hor (abs (- (* (1- int-hor) int-ver) len-circle)))
          (setq dist-dec-ver (abs (- (* int-hor (1- int-ver)) len-circle)))
          (when (> torus-verbosity 2)
            (message "Distance hor ver = %f %f" dist-dec-hor dist-dec-ver))
          (cond ((and (<= dist-dec-hor dist-dec-ver)
                      (>= (* (1- int-hor) int-ver) len-circle))
                 (setq int-hor (1- int-hor))
                 (when (> torus-verbosity 2)
                   (message "Decrease int-hor : int-hor int-ver = %d %d"
                            int-hor  int-ver)))
                ((and (>= dist-dec-hor dist-dec-ver)
                      (>= (* int-hor (1- int-ver)) len-circle))
                 (setq int-ver (1- int-ver))
                 (when (> torus-verbosity 2)
                   (message "Decrease int-ver : int-hor int-ver = %d %d"
                            int-hor  int-ver)))
                (t (setq getout t)
                   (when (> torus-verbosity 2)
                     (message "Getout : %s" getout)
                     (message "int-hor int-ver = %d %d" int-hor int-ver))))))
      (setq num-hor-minus (number-sequence 1 (1- int-hor)))
      (setq num-hor (number-sequence 1 int-hor))
      (setq num-ver-minus (number-sequence 1 (1- int-ver)))
      (when (> torus-verbosity 2)
        (message "num-hor-minus = %s" num-hor-minus)
        (message "num-hor = %s" num-hor)
        (message "num-ver-minus = %s" num-ver-minus))
      (delete-other-windows)
      (dolist (iter-hor num-hor-minus)
        (when (> torus-verbosity 2)
          (message "iter hor = %d" iter-hor))
        (setq max-iter (1- max-iter))
        (split-window-below)
        (balance-windows)
        (other-window 1))
      (other-window 1)
      (dolist (iter-hor num-hor)
        (dolist (iter-ver num-ver-minus)
          (when (> torus-verbosity 2)
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
(defun torus-split-layout-menu (choice)
  "Split according to CHOICE."
  (interactive
   (list (read-key ttorus--msg-layout-menu)))
  (ttorus--complete-and-clean-layout)
  (let ((circle (caar torus-cur-torus)))
    (when (member choice '(?m ?o ?h ?v ?l ?r ?t ?b ?g))
      (setcdr (assoc circle torus-split-layout) choice))
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

;;; Delete
;;; ------------------------------

;;;###autoload
(defun ttorus-delete-circle (circle-name)
  "Delete circle given by CIRCLE-NAME."
  (interactive
   (list
    (completing-read "Delete circle : "
                     (mapcar #'car torus-cur-torus) nil t)))
  (when (y-or-n-p (format "Delete circle %s ? " circle-name))
    (setq torus-cur-torus (ttorus--assoc-delete-all circle-name torus-cur-torus))
    (setq ttorus-table
          (ttorus--reverse-assoc-delete-all circle-name ttorus-table))
    (setq ttorus-old-history
          (ttorus--reverse-assoc-delete-all circle-name ttorus-old-history))
    (setq ttorus-markers
          (ttorus--reverse-assoc-delete-all circle-name ttorus-markers))
    (let ((circle-torus (cons (caar torus-cur-torus) (caar ttorus-meta))))
      (setq torus-helix
            (ttorus--reverse-assoc-delete-all circle-torus torus-helix))
      (setq ttorus-history
            (ttorus--reverse-assoc-delete-all circle-torus ttorus-history)))
    (ttorus--build-table)
    (setq torus-helix (ttorus--build-helix))
    (ttorus--jump)))

;;;###autoload
(defun ttorus-delete-location (location-name)
  "Delete location given by LOCATION-NAME."
  (interactive
   (list
    (completing-read
     "Delete location : "
     (mapcar #'ttorus--concise (cdr (car torus-cur-torus))) nil t)))
  (if (and
       (> (length (car torus-cur-torus)) 1)
       (y-or-n-p
        (format
         "Delete %s from circle %s ? "
         location-name
         (car (car torus-cur-torus)))))
      (let* ((circle (cdr (car torus-cur-torus)))
             (index (cl-position location-name circle
                                 :test #'ttorus--equal-concise-p))
             (location (nth index circle))
             (location-circle (cons location (caar torus-cur-torus)))
             (location-circle-torus (cons location (cons (caar torus-cur-torus)
                                                         (caar ttorus-meta)))))
        (setcdr (car torus-cur-torus) (cl-remove location circle))
        (setq ttorus-table (cl-remove location-circle ttorus-table))
        (setq ttorus-old-history (cl-remove location-circle ttorus-old-history))
        (setq ttorus-markers (cl-remove location-circle ttorus-markers))
        (setq torus-helix (cl-remove location-circle-torus torus-helix))
        (setq ttorus-history (cl-remove location-circle-torus ttorus-history))
        (ttorus--jump))
    (message "No location in current circle.")))

;;;###autoload
(defun ttorus-delete-current-circle ()
  "Delete current circle."
  (interactive)
  (ttorus-delete-circle (ttorus--concise (car (car torus-cur-torus)))))

;;;###autoload
(defun ttorus-delete-current-location ()
  "Remove current location from current circle."
  (interactive)
  (ttorus-delete-location (ttorus--concise (car (cdr (car torus-cur-torus))))))

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
(defun ttorus-read (filename)
  "Read main ttorus variables from FILENAME as Lisp code."
  (interactive
   (list
    (read-file-name
     "ttorus file : "
     (file-name-as-directory torus-dirname))))
  (let*
      ((file-basename (file-name-nondirectory filename))
       (minus-len-ext (- (min (length torus-file-extension)
                              (length filename))))
       (buffer))
    (unless (equal (cl-subseq filename minus-len-ext) torus-file-extension)
      (setq filename (concat filename torus-file-extension)))
    (when (or (not torus-wheel)
              (y-or-n-p ttorus--msg-replace-torus))
      (ttorus--update-input-history file-basename)
      (if (file-exists-p filename)
          (progn
            (setq buffer (find-file-noselect filename))
            (eval-buffer buffer)
            (kill-buffer buffer))
        (message "File %s does not exist." filename))))
  (ttorus--convert-old-vars)
  (ttorus--jump))

;;;###autoload
(defun ttorus-write (filename)
  "Write main ttorus variables to FILENAME as Lisp code.
An adequate extension is added if needed.
If called interactively, ask for the variables to save (default : all)."
  (interactive
   (list
    (read-file-name
     "ttorus file : "
     (file-name-as-directory torus-dirname))))
  ;; We surely don’t want to load a file we’ve just written
  (remove-hook 'after-save-hook 'ttorus-after-save-torus-file)
  (if ttorus-meta
      (let*
          ((file-basename (file-name-nondirectory filename))
           (minus-len-ext (- (min (length torus-file-extension)
                                  (length filename))))
           (buffer)
           (varlist '(torus-wheel
                      torus-helix
                      ttorus-history
                      torus-user-input-history
                      torus-split-layout
                      ttorus-line-col)))
        (ttorus--update-position)
        (ttorus--update-input-history file-basename)
        (unless (equal (cl-subseq filename minus-len-ext) torus-file-extension)
          (setq filename (concat filename torus-file-extension)))
        (unless ttorus-table
          (ttorus--build-table))
        (unless torus-helix
          (setq torus-helix (ttorus--build-helix)))
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
(defun ttorus-edit (filename)
  "Edit ttorus file FILENAME in the ttorus files dir.
Be sure to understand what you’re doing, and not leave some variables
in inconsistent state, or you might encounter strange undesired effects."
  (interactive
   (list
    (read-file-name
     "ttorus file : "
     (file-name-as-directory torus-dirname))))
  (find-file filename))

;;; End
;;; ------------------------------------------------------------

(provide 'ttorus)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; ttorus.el ends here
