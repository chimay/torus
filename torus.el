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
;;; ----------------------------------------------------------------------

(eval-when-compile
  (require 'duo-common)
  (require 'duo-referen))

(declare-function duo-equal-car-p "duo-common")
(declare-function duo-equal-caar-p "duo-common")
(declare-function duo-x-match-car-p "duo-common")
(declare-function duo-x-match-cdr-p "duo-common")
(declare-function duo-x-match-cadr-p "duo-common")
(declare-function duo-x-match-caar-p "duo-common")
(declare-function duo-at-index "duo-common")
(declare-function duo-member "duo-common")
(declare-function duo-last "duo-common")
(declare-function duo-assoc "duo-common")
(declare-function duo-replace "duo-common")
(declare-function duo-replace-car "duo-common")
(declare-function duo-replace-cdr "duo-common")
(declare-function duo-replace-cdar "duo-common")
(declare-function duo-replace-all-car "duo-common")
(declare-function duo-replace-all-caar "duo-common")
(declare-function duo-replace-all-cdar "duo-common")
(declare-function duo-replace-all-cadr "duo-common")
(declare-function duo-assoc-index-member "duo-common")
(declare-function duo-index-of "duo-common")
(declare-function duo-index-member "duo-common")
(declare-function duo-circ-next "duo-common")
(declare-function duo-circ-previous "duo-common")
(declare-function duo-circ-next-in-group "duo-common")
(declare-function duo-circ-next-not-in-group "duo-common")
(declare-function duo-filter "duo-common")

(declare-function duo-deref "duo-referen")
(declare-function duo-ref-push "duo-referen")
(declare-function duo-ref-insert-in-sorted-list "duo-referen")
(declare-function duo-ref-rotate-left "duo-referen")
(declare-function duo-ref-rotate-right "duo-referen")
(declare-function duo-ref-roll-cons-to-beg "duo-referen")
(declare-function duo-ref-teleport-cons-previous "duo-referen")
(declare-function duo-ref-push-and-truncate "duo-referen")
(declare-function duo-ref-delete-all "duo-referen")
(declare-function duo-ref-delete "duo-referen")
(declare-function duo-ref-reverse "duo-referen")
(declare-function duo-ref-add-new "duo-referen")
(declare-function duo-ref-add "duo-referen")
(declare-function duo-ref-push-new "duo-referen")
(declare-function duo-ref-teleport-cons-after "duo-referen")
(declare-function duo-ref-circ-move-previous "duo-referen")
(declare-function duo-ref-circ-move-next "duo-referen")

;;; Custom
;;; ----------------------------------------------------------------------

(defgroup torus nil
  "An interface to navigating groups of buffers."
  :tag "torus"
  :link '(url-link :tag "Home Page"
                   "https://github.com/chimay/torus")
  :link '(emacs-commentary-link
                  :tag "Commentary in torus.el" "torus.el")
  :prefix "torus-"
  :group 'environment
  :group 'extensions
  :group 'convenience)

(defcustom torus-prefix-key "s-t"
  "Prefix key for the torus key mappings.
Will be processed by `kbd'."
  :type 'string
  :group 'torus)

(defcustom torus-binding-level 1
  "Binding level : the higher it is, the more bindings you have.
Level 0 : Basic
Level 1 : Common
Level 2 : Advanced
Level 3 : Debug"
  :type 'integer
  :group 'torus)

(defcustom torus-verbosity 1
  "Level of verbosity.
1 = normal
2 = light debug
3 = heavy debug."
  :type 'integer
  :group 'torus)

(defcustom torus-dirname (concat user-emacs-directory "torus/")
  "The directory where the torus are read and written."
  :type 'string
  :group 'torus)

(defcustom torus-load-on-startup nil
  "Whether to load torus on startup of Emacs."
  :type 'boolean
  :group 'torus)

(defcustom torus-save-on-exit nil
  "Whether to save torus on exit of Emacs."
  :type 'boolean
  :group 'torus)

(defcustom torus-autoread-file "auto"
  "The file to load on startup when `torus-load-on-startup' is not nil."
  :type 'string
  :group 'torus)

(defcustom torus-autowrite-file "auto"
  "The file to write before quitting Emacs when `torus-save-on-exit' is not nil."
  :type 'string
  :group 'torus)

(defcustom torus-backup-number 3
  "Number of backups of torus files."
  :type 'integer
  :group 'torus)

(defcustom torus-maximum-history-elements 50
  "Maximum number of elements in history variables.
See `torus-history' and `torus-user-input-history'."
  :type 'integer
  :group 'torus)

(defcustom torus-maximum-horizontal-split 3
  "Maximum number of horizontal split, see `torus-split-horizontally'."
  :type 'integer
  :group 'torus)

(defcustom torus-maximum-vertical-split 4
  "Maximum number of vertical split, see `torus-split-vertically'."
  :type 'integer
  :group 'torus)

(defcustom torus-display-tab-bar nil
  "Whether to display a tab bar in `header-line-format'."
  :type 'boolean
  :group 'torus)

(defcustom torus-separator-torus-circle " >> "
  "String between torus and circle in the dashboard."
  :type 'string
  :group 'torus)

(defcustom torus-separator-circle-location " > "
  "String between circle and location(s) in the dashboard."
  :type 'string
  :group 'torus)

(defcustom torus-location-separator " | "
  "String between location(s) in the dashboard."
  :type 'string
  :group 'torus)

(defcustom torus-prefix-separator "/"
  "String between the prefix and the circle names.
The name of the new circles will be of the form :
\"User_input_prefix `torus-prefix-separator' Name_of_the_added_circle\"
without the spaces. If the user enter a blank prefix,
the added circle names remain untouched."
  :type 'string
  :group 'torus)

(defcustom torus-join-separator " & "
  "String between the names when joining.
The name of the new object will be of the form :
\"Object-1 `torus-join-separator' Object-2\"
without the spaces."
  :type 'string
  :group 'torus)

;;; Variables
;;; ----------------------------------------------------------------------

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

(defvar torus-history (list nil)
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

(defvar torus-line-col (list nil)
  "Reference to an alist storing locations and lines & columns in files.
More precisely, it’s a cons whose car is a list of entries.
Each entry is a cons :
\((file . position) . (line . column))
Allows to display lines & columns.")

;;; Transient
;;; ------------------------------------------------------------

(defvar torus-buffers (list nil)
  "Reference to an alist containing buffers to opened files.
More precisely, it’s a cons whose car is a list of entries.
Each entry is a cons :
\(file . buffer)
Contain only the files opened in buffers.")

(defvar torus-markers (list nil)
  "Reference to an alist containing markers to opened files.
More precisely, it’s a cons whose car is a list of entries.
Each entry is a cons :
\((file . position) . marker)
Contain only the files opened in buffers.")

(defvar torus-original-header-lines (list nil)
  "Reference to an alist containing header lines before the tab bar changed it.
More precisely, it’s a cons whose car is a list of entries.
Each entry is a cons :
\(buffer . original-header-line)")

;;; Current cons in list
;;; ------------------------------------------------------------

(defvar torus-cur-torus nil
  "Cons of current torus in `torus-wheel'.")

(defvar torus-cur-circle nil
  "Cons of current circle in `torus-cur-torus'.")

(defvar torus-cur-location nil
  "Cons of current location in `torus-cur-circle'.")

(defvar torus-cur-helix nil
  "Cons of current entry in `torus-helix'.")

(defvar torus-cur-grid nil
  "Cons of current entry in `torus-grid'.")

(defvar torus-cur-history nil
  "Cons of current entry in `torus-history'.")

(defvar torus-cur-user-input nil
  "Cons of current entry in `torus-user-input-history'.")

;;; Last cons in list
;;; ------------------------------------------------------------

(defvar torus-last-torus nil
  "Last torus in `torus-wheel'. Just for speed.")

(defvar torus-last-circle nil
  "Last circle in `torus-cur-torus'. Just for speed.")

(defvar torus-last-location nil
  "Last location in `torus-cur-circle'. Just for speed.")

;;; Files
;;; ------------------------------------------------------------

(defvar torus-file-extension ".el"
  "Extension of torus files.")

;;; Prompts
;;; ------------------------------------------------------------

;;; Empty
;;; ------------------------------

(defvar torus--msg-empty-wheel
  "Wheel is empty. Please add a location with torus-add-location.")

(defvar torus--msg-empty-torus
  "Torus %s is empty. Please add a location with torus-add-location.")

(defvar torus--msg-empty-circle
  "Circle %s in Torus %s is empty. Please use torus-add-location.")

;;; Menus
;;; ------------------------------

(defvar torus--msg-add-menu
  "Add [h] here [f] file [b] buffer [l] location [c] circle [t] torus")

(defvar torus--msg-rename-menu
  "Rename [f] file [c] circle [t] torus")

(defvar torus--msg-delete-menu
  "Delete [l] location [c] circle [t] torus")

(defvar torus--msg-switch-menu
  "Switch [t] torus [c] circle [l] location")

(defvar torus--msg-reset-menu
  "Reset [a] all [w] wheel
      [t] current torus [C-t] last torus
      [c] current circle [C-c] last circle
      [l] current location [C-l] last location
      [x] helix [C-x] current helix [g] grid [G] current grid
      [h] history [C-h] current history
      [u] user input history [C-u] current user input
      [s] split layout [&] line & col
      [b] buffers [m] markers [o] orig header line")

(defvar torus--msg-print-menu
  "Print [a] all [w] wheel
      [t] current torus [C-t] last torus
      [c] current circle [C-c] last circle
      [l] current location [C-l] last location
      [x] helix [C-x] current helix [g] grid [G] current grid
      [h] history [C-h] current history
      [u] user input history [C-u] current user input
      [s] split layout [&] line & col
      [b] buffers [m] markers [o] orig header line")

(defvar torus--msg-alternate-menu
  "Alternate [^] anywhere
          [c] in same circle [i] in other circle
          [t] in same torus [o] in other torus")

(defvar torus--msg-reverse-menu
  "Reverse [l] locations [c] circle [d] deep : locations & circles")

(defvar torus--msg-autogroup-menu
  "Autogroup by [p] path [d] directory [e] extension")

(defvar torus--msg-batch-menu
  "Run on circle files [e] Elisp code [c] Elisp command \n\
                    [!] Shell command [&] Async Shell command")

(defvar torus--msg-split-menu
  "Layout [m] manual [o] one window [h] horizontal [v] vertical [g] grid\n\
       main window on [l] left [r] right [t] top [b] bottom")

;;; Miscellaneous
;;; ------------------------------

(defvar torus--msg-file-does-not-exist
  "File %s does not exist anymore. It will be removed from the torus.")

(defvar torus--msg-existent-location
  "Location %s already exists in circle %s")

(defvar torus--msg-prefix-circle
  "Prefix for the circle of torus %s (leave blank for none) ? ")

(defvar torus--msg-circle-name-collision
  "Circle name collision. Please add/adjust prefixes to avoid confusion.")

(defvar torus--msg-replace-variables
  "This will replace the current torus variables. Continue ? ")

;;; Keymaps & Mouse maps
;;; ----------------------------------------------------------------------

(defvar torus-map)

(define-prefix-command 'torus-map)

(defvar torus-map-mouse-torus (make-sparse-keymap))
(defvar torus-map-mouse-circle (make-sparse-keymap))
(defvar torus-map-mouse-location (make-sparse-keymap))

;;; Toolbox
;;; ----------------------------------------------------------------------

;;; Strings
;;; ------------------------------------------------------------

(defun torus--eval-string (string)
  "Eval Elisp code in STRING."
  (eval (car (read-from-string (format "(progn %s)" string)))))

;;; Files
;;; ------------------------------------------------------------

(defun torus--directory (object)
  "Return the last directory component of OBJECT.
OBJECT can be a filename or a location."
  (let* ((filename
          (pcase object
            (`(,(and (pred stringp) one) . ,(pred integerp)) one)
            ((pred stringp) object)))
         (names (split-string filename "/" t)))
    (car (duo-at-index -2 names))))

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
;;; ----------------------------------------------------------------------

;;; Template
;;; ------------------------------------------------------------

(defsubst torus--tree-template (name)
  "Minimal tree template for data structure with a NAME."
  (cons name (cons nil nil)))

;;; State
;;; ------------------------------------------------------------

;;; Wheel
;;; ------------------------------

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
;;; ------------------------------

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
;;; ------------------------------

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

;;; Location
;;; ------------------------------

(defsubst torus--root-location ()
  "Return root of current location."
  (car torus-cur-location))

(defsubst torus--file-name (&optional name)
  "Return current file name in torus. Change it to NAME if non nil."
  (if name
      (setcar (torus--root-location) name)
    (car (torus--root-location))))

(defsubst torus--file-position (&optional position)
  "Return current file position in torus. Change it to POSITION if non nil."
  (if position
      (setcdr (torus--root-location) position)
    (cdr (torus--root-location))))

;;; In / Out
;;; ------------------------------

(defun torus--inside-p (&optional buffer)
  "Whether BUFFER belongs to the torus.
Argument BUFFER nil means use current buffer."
  (let* ((buffer (or buffer (current-buffer)))
         (filename (buffer-file-name buffer))
         (wheel-files (mapcar 'cadr (duo-deref torus-helix))))
    (duo-member filename wheel-files)))

;;; Empty ?
;;; ------------------------------

(defsubst torus--empty-wheel-p ()
  "Whether the torus list is empty."
  (or (null (torus--ref-torus-list))
      (null (torus--torus-list))))

(defsubst torus--empty-torus-p ()
  "Whether current torus is empty.
It’s empty when nil or just a name in car
but no circle in it."
  (or (null (torus--ref-circle-list))
      (null (torus--circle-list))))

(defsubst torus--empty-circle-p ()
  "Whether current circle is empty.
It’s empty when nil or just a name in car
but no location in it."
  (or (null (torus--ref-location-list))
      (null (torus--location-list))))

(defsubst torus--empty-helix-p ()
  "Whether `torus-helix' is empty."
  (or (null torus-helix)
      (null (duo-deref torus-helix))))

(defsubst torus--empty-grid-p ()
  "Whether `torus-grid' is empty."
  (or (null torus-grid)
      (null (duo-deref torus-grid))))

(defsubst torus--empty-history-p ()
  "Whether `torus-history' is empty."
  (or (null torus-history)
      (null (duo-deref torus-history))))

;;; Enter the Void
;;; ------------------------------

(defsubst torus--set-nil-circle ()
  "Set current circle variables to nil."
  (setq torus-cur-circle nil)
  (setq torus-last-circle nil))

(defsubst torus--set-nil-location ()
  "Set current location variables to nil."
  (setq torus-cur-location nil)
  (setq torus-last-location nil))

;;; Alter index
;;; ------------------------------

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
         (index (car index-length))
         (length (1- (cdr index-length))))
    (when index-length
      (if (< length 1)
          (progn
            (message "Setting (index . length) to nil.")
            (setcdr ref nil))
        (setcar index-length (min index (1- length)))
        (setcdr index-length length)))))

(defsubst torus--increase-index (ref &optional num)
  "Increase current index in cdr of REF by NUM. Circular.
NUM defaults to 1."
  (let* ((num (or num 1))
         (index-length (cdr ref))
         (index (car index-length))
         (length (cdr index-length)))
    (if index-length
        (setcar index-length (mod (+ index num) length))
      (setcdr ref (cons 0 1)))))

(defsubst torus--decrease-index (ref &optional num)
  "Decrease current index in cdr of REF by NUM. Circular.
NUM defaults to 1."
  (let* ((num (or num 1))
         (index-length (cdr ref))
         (index (car index-length))
         (length (cdr index-length)))
    (if index-length
        (setcar index-length (mod (- index num) length))
      (setcdr ref (cons 0 1)))))

;;; Seek to index
;;; ------------------------------

(defsubst torus--seek-torus (&optional index)
  "Set current torus to the one given by INDEX.
INDEX defaults to current torus index."
  (if (torus--empty-wheel-p)
      (message "Can’t seek on empty wheel.")
    (when index
      (torus--torus-index index))
    (let* ((ref (torus--ref-torus-list))
           (content (car ref))
           (index-length (cdr ref))
           (index (car index-length))
           (length (cdr index-length))
           (tail-length (- length index 1)))
      (if content
          (progn
            (setq torus-cur-torus (duo-at-index index content))
            (setq torus-last-torus (nthcdr tail-length torus-cur-torus)))
        (setq torus-cur-torus nil)
        (setq torus-last-torus nil)))))

(defsubst torus--seek-circle (&optional index)
  "Set current circle to the one given by INDEX.
INDEX defaults to current circle index."
  (if (torus--empty-torus-p)
      (message "Can’t seek on empty torus.")
    (when index
      (torus--circle-index index))
    (let* ((ref (torus--ref-circle-list))
           (content (car ref))
           (index-length (cdr ref))
           (index (car index-length))
           (length (cdr index-length))
           (tail-length (- length index 1)))
      (if content
          (progn
            (setq torus-cur-circle (duo-at-index index content))
            (setq torus-last-circle (nthcdr tail-length torus-cur-circle)))
        (setq torus-cur-circle nil)
        (setq torus-last-circle nil)))))

(defsubst torus--seek-location (&optional index)
  "Set current location to the one given by INDEX.
INDEX defaults to current location index."
  (if (torus--empty-circle-p)
      (message "Can’t seek on empty circle.")
    (when index
      (torus--location-index index))
    (let* ((ref (torus--ref-location-list))
           (content (car ref))
           (index-length (cdr ref))
           (index (car index-length))
           (length (cdr index-length))
           (tail-length (- length index 1)))
      (if content
          (progn
            (setq torus-cur-location (duo-at-index index content))
            (setq torus-last-location (nthcdr tail-length torus-cur-location)))
        (setq torus-cur-location nil)
        (setq torus-last-location nil)))))

;;; Rewind
;;; ------------------------------

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

;;; String representation
;;; ------------------------------------------------------------

;;; String & Location
;;; ------------------------------

(defun torus--buffer-or-file-name (location)
  "Return buffer name of LOCATION if found in torus variables.
Return file basename otherwise."
  (unless (consp location)
    (error "In torus--buffer-or-file-name : wrong type argument"))
  (let* ((file-buffer (car (duo-assoc (car location)
                                            (duo-deref torus-buffers))))
         (location-marker (car (duo-assoc location
                                          (duo-deref torus-markers))))
         (marker (cdr location-marker))
         (buffer (cond (file-buffer (cdr file-buffer))
                         (marker (marker-buffer marker)))))
    (if buffer
        (buffer-name buffer)
      (file-name-nondirectory (car location)))))

(defun torus--position-string (location)
  "Return position in LOCATION in raw format or in line & column if available.
Line & Columns are stored in `torus-line-col'."
  (let ((entry (car (duo-assoc location (duo-deref torus-line-col)))))
    (if entry
        (format " at line %s col %s" (cadr entry) (cddr entry))
      (format " at position %s" (cdr location)))))

;;; String & Entry
;;; ------------------------------

(defun torus--entry-to-string (object)
  "Return OBJECT in concise string format.
Here are the returned strings, depending of the nature
of OBJECT :
\((torus . circle) . (file . pos)) -> torus >> circle > file at pos
\(torus . circle)                  -> torus >> circle
\(circle . (file . pos))           -> circle > file at Pos
\(file . position)                 -> file at position
string                             -> string"
  (let ((location))
    (pcase object
      (`((,(and (pred stringp) torus) . ,(and (pred stringp) circle)) .
         (,(and (pred stringp) file) . ,(and (pred integerp) position)))
       (setq location (cons file position))
       (concat torus
               torus-separator-torus-circle
               circle
               torus-separator-circle-location
               (torus--buffer-or-file-name location)
               (torus--position-string location)))
      (`(,(and (pred stringp) torus) . ,(and (pred stringp) circle))
       (concat torus
               torus-separator-torus-circle
               circle))
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
      (_ (error "In torus--entry-to-string : wrong type argument")))))

(defun torus--equal-string-entry-p (one two)
  "Whether the string representations of entries ONE and TWO are equal."
  (equal (torus--entry-to-string (torus--make-entry one))
         (torus--entry-to-string (torus--make-entry two))))

;;; Status bar
;;; ------------------------------------------------------------

(defun torus--needle (&optional location)
  "Return LOCATION in short string format.
Shorter than concise. Used for dashboard and tabs."
  (let* ((cur-location (torus--root-location))
         (location (or location cur-location))
         (entry (car (duo-assoc location
                                (duo-deref torus-line-col))))
         (position (if entry
                       (format " : %s" (car (cdr entry)))
                     (format " . %s" (cdr location))))
         (needle (concat (torus--buffer-or-file-name location) position)))
    (when (equal location cur-location)
      (setq needle (concat "[* " needle " *]")))
    needle))

(defun torus--dashboard ()
  "Display summary of current torus, circle and location."
  (let ((torus (propertize (format (concat " %s"
                                           torus-separator-torus-circle)
                                   (torus--torus-name))
                           'keymap torus-map-mouse-torus))
        (circle (propertize (format (concat "%s"
                                            torus-separator-circle-location)
                                    (torus--circle-name))
                            'keymap torus-map-mouse-circle))
        (needles (mapcar #'torus--needle (torus--location-list)))
        (locations))
    (dolist (filepos needles)
      (setq locations (concat locations filepos torus-location-separator)))
    (setq locations (propertize locations 'keymap torus-map-mouse-location))
    (concat torus circle locations)))

(defun torus--status-bar ()
  "Display status bar, as tab bar or as info in echo area."
  (let* ((main-windows (torus--main-windows))
         (current-window (selected-window))
         (buffer (current-buffer))
         (original (car (duo-assoc buffer torus-original-header-lines)))
         (eval-tab '(:eval (torus--dashboard))))
    (if torus-display-tab-bar
        (when (member current-window main-windows)
          (unless original
            (duo-ref-push (cons buffer header-line-format)
                          torus-original-header-lines))
          (unless (equal header-line-format eval-tab)
            (setq header-line-format eval-tab)))
      (when original
        (setq header-line-format (cdr original))
        (duo-ref-delete-all original
                            torus-original-header-lines))
      (message (substring-no-properties (torus--dashboard))))))

(defun torus--wheel-status ()
  "Display torus names of wheel in echo area."
  (let ((string "Toruses : ")
        (elem))
    (dolist (name (mapcar #'car (torus--torus-list)))
      (if (equal name (torus--torus-name))
          (setq elem (concat "[* " name " *]"))
        (setq elem name))
      (setq string (concat string elem " | ")))
    (message string)))

(defun torus--torus-status ()
  "Display circle names of current torus in echo area."
  (let ((string "Circles : ")
        (elem))
    (dolist (name (mapcar #'car (torus--circle-list)))
      (if (equal name (torus--circle-name))
          (setq elem (concat "[* " name " *]"))
        (setq elem name))
      (setq string (concat string elem " | ")))
    (message string)))

;;; Files
;;; ------------------------------------------------------------

(defun torus--complete-filename (filename)
  "Return complete version of FILENAME.
If FILENAME is a relative path, it’s assumed to be relative to `torus-dirname'.
If FILENAME is an absolute path, do nothing."
  (let ((absolute (if (file-name-absolute-p filename)
                      filename
                    (concat (file-name-as-directory torus-dirname) filename))))
    (unless (string-suffix-p torus-file-extension absolute)
      (setq absolute (concat absolute torus-file-extension)))
    absolute))

(defsubst torus--full-directory (&optional directory)
  "Return full path of DIRECTORY.
DIRECTORY defaults to `torus-dirname'."
  (let ((directory (or directory torus-dirname)))
    (expand-file-name (file-name-as-directory directory))))

(defun torus--make-dir (directory)
  "Create DIRECTORY if non existent."
  (unless (file-exists-p directory)
    (when (> torus-verbosity 0)
      (message "Creating directory %s" directory))
    (make-directory directory)))

(defun torus--roll-backups (filename)
  "Roll backups of FILENAME."
  (unless (stringp filename)
    (error "In torus--roll-backups : wrong type argument"))
  (let ((file-list (list filename))
        (file-src)
        (file-dest))
    (dolist (iter (number-sequence 1 torus-backup-number))
      (push (concat filename "." (prin1-to-string iter)) file-list))
    (while (> (length file-list) 1)
      (setq file-dest (pop file-list))
      (setq file-src (car file-list))
      (when (and file-src (file-exists-p file-src))
        (copy-file file-src file-dest t)
        (when (> torus-verbosity 1)
          (message "copy %s -> %s" file-src file-dest))))))

;;; Entry
;;; ------------------------------------------------------------

(defun torus--make-entry (&optional object)
  "Return an entry ((torus-name . circle-name) . (file . position)) from OBJECT.
Use current torus, circle and location if not given.
Accepted argument formats :
- nil
- (file . position)
- (torus-name . (file . position))
- ((torus-name . circle-name) . (file . position))"
  (pcase object
    ('nil
     (let ((torus-name (torus--torus-name))
           (circle-name (torus--circle-name))
           (location (torus--root-location)))
       (when (and torus-name circle-name location)
         (cons (cons torus-name circle-name) location))))
    (`(,(pred stringp) . ,(pred integerp))
     (let ((torus-name (torus--torus-name))
           (circle-name (torus--circle-name)))
       (when (and torus-name circle-name)
         (cons (cons torus-name circle-name) object))))
    (`(,(pred stringp) . (,(pred stringp) . ,(pred integerp)))
     (let ((torus-name (torus--torus-name)))
       (when torus-name
         (cons (cons torus-name (car object)) (cdr object)))))
    (`((,(pred stringp) . ,(pred stringp)) .
       (,(pred stringp) . ,(pred integerp)))
     object)
    (_ (error "In torus--make-entry : %s wrong type argument" object))))

;;; Helix
;;; ------------------------------------------------------------

(defun torus--add-to-helix (&optional object)
  "Add an entry built from OBJECT to `torus-helix'."
  (let* ((entry (torus--make-entry object))
         (helix (duo-deref torus-helix))
         (member (duo-member entry helix)))
    (when (and entry (not member))
      (setq torus-cur-helix
            (duo-ref-insert-in-sorted-list entry torus-helix)))))

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
                (duo-ref-insert-in-sorted-list entry torus-helix))
          (when (> torus-verbosity 1)
            (message "Helix entry %s" entry)))))))

;;; Grid
;;; ------------------------------------------------------------

(defun torus--add-to-grid (&optional object)
  "Add an entry built from OBJECT to `torus-grid'."
  (let* ((entry (or object (cons (torus--torus-name) (torus--circle-name))))
         (grid (duo-deref torus-grid))
         (member (duo-member entry grid)))
    (when (and entry (not member))
      (setq torus-cur-grid
            (duo-ref-insert-in-sorted-list entry torus-grid)))))

(defun torus--build-grid ()
  "Build grid from `torus-wheel'."
  (setq torus-grid (list nil))
  (let ((torus-name)
        (circle-name)
        (entry))
    (dolist (torus (torus--torus-list))
      (setq torus-name (car torus))
      (dolist (circle (car (cdr torus)))
        (setq circle-name (car circle))
        (setq entry (cons torus-name circle-name))
        (setq torus-cur-grid
              (duo-ref-insert-in-sorted-list entry torus-grid))
        (when (> torus-verbosity 1)
          (message "Grid entry %s" entry))))))

;;; History
;;; ------------------------------------------------------------

(defun torus--add-to-history (&optional object)
  "Add an entry built from OBJECT to `torus-history'.
Move entry at beginning if already present."
  (let* ((entry (torus--make-entry object))
         (history (duo-deref torus-history))
         (member))
    (unless (eq torus-cur-history history)
      (duo-ref-roll-cons-to-beg torus-cur-history torus-history))
    (setq history (duo-deref torus-history))
    (setq member (duo-member entry history))
    (when entry
      (if member
          (setq torus-cur-history
                (duo-ref-teleport-cons-previous history member torus-history))
        (setq torus-cur-history
              (duo-ref-push-and-truncate entry
                                         torus-history
                                         torus-maximum-history-elements))))))

;;; User Input History
;;; ------------------------------------------------------------

(defun torus--add-user-input (string)
  "Add STRING to `torus-user-input-history'.
Move entry at beginning if already present."
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

;;; Lines & Columns
;;; ------------------------------------------------------------

(defun torus--make-line-col (&optional object)
  "Return an entry ((file . position) . (line .column)) from OBJECT.
Use current location, line & column if not given."
  (pcase object
    ('nil
     (cons (cons (buffer-file-name) (marker-position (point-marker)))
           (cons (line-number-at-pos) (current-column))))
    (`(,(pred stringp) . ,(pred integerp))
     (cons object (cons (line-number-at-pos) (current-column))))
    (`((,(pred stringp) . ,(pred integerp)) .
       (,(pred integerp) . ,(pred integerp)))
     object)
    (_ (error "In torus--make-line-col : %s wrong type argument" object))))

(defun torus--add-to-line-col (&optional object)
  "Add entry to `torus-line-col' according to OBJECT.
Update line & col part if necessary."
  (let* ((entry (torus--make-line-col object))
         (location (car entry))
         (table (duo-deref torus-line-col))
         (old (car (duo-assoc location table))))
    (if old
        (when (not (equal old entry))
          (when (> torus-verbosity 0)
            (message "Updating line & column %s -> %s" old entry))
          (duo-replace old entry table))
      (duo-ref-insert-in-sorted-list entry torus-line-col))))

;;; Tables
;;; ------------------------------------------------------------

(defun torus--add-entry (entry ref-table)
  "Add ENTRY to table referenced in REF-TABLE."
  (let* ((table (duo-deref ref-table))
         (member (duo-member entry table)))
    (when (and entry (not member))
      (duo-ref-insert-in-sorted-list entry ref-table))))

(defun torus--add-or-replace-entry (old new ref-table)
  "Add NEW or replace OLD by NEW in TABLE."
  (let* ((table (duo-deref ref-table))
         (member (duo-member old table)))
    (if member
        (duo-replace old new table)
      (duo-ref-insert-in-sorted-list new ref-table))))

(defun torus--replace-entries (old-entry new-entry)
  "Replace OLD-ENTRY by NEW-ENTRY in table variables.
Affected variables : `torus-helix', `torus-history'."
  (duo-replace old-entry new-entry (duo-deref torus-helix))
  (duo-replace old-entry new-entry (duo-deref torus-history)))

(defun torus--delete-file-entries (filename)
  "Delete entries matching FILENAME from table variables.
Affected variables : `torus-helix', `torus-history',
`torus-line-col', `torus-markers'."
  (duo-ref-delete-all filename torus-helix #'duo-x-match-cadr-p)
  (duo-ref-delete-all filename torus-history #'duo-x-match-cadr-p)
  (duo-ref-delete-all filename torus-line-col #'duo-x-match-caar-p)
  (duo-ref-delete-all filename torus-buffers #'duo-x-match-car-p)
  (duo-ref-delete-all filename torus-markers #'duo-x-match-caar-p)
  (setq torus-cur-helix (duo-deref torus-helix))
  (setq torus-cur-history (duo-deref torus-history)))

;;; Sync
;;; ------------------------------------------------------------

(defun torus--update-position ()
  "Update position in current location.
Do nothing if file does not match current buffer.
Sync Emacs buffer state -> Torus state."
  (if (torus--empty-circle-p)
      (message "Can’t update location on an empty circle.")
    (let* ((old-location (torus--root-location))
           (file (car old-location))
           (old-position (cdr old-location))
           (new-position (point)))
      (when (and (not (equal new-position old-position))
                 (equal file (buffer-file-name (current-buffer))))
        (let* ((old-entry (torus--make-entry))
               (old-location-line-col (car (duo-assoc
                                            old-location
                                            (duo-deref torus-line-col))))
               (old-location-marker (car (duo-assoc
                                          old-location
                                          (duo-deref torus-markers))))
               (new-location (cons file new-position))
               (new-entry (torus--make-entry new-location))
               (new-line-col (cons (line-number-at-pos) (current-column)))
               (new-marker (point-marker))
               (new-location-line-col (cons new-location new-line-col))
               (new-location-marker (cons new-location new-marker)))
          (when (> torus-verbosity 1)
            (message "Updating position %s -> %s in file %s"
                     old-position
                     new-position
                     file))
          (torus--replace-entries old-entry new-entry)
          (torus--add-or-replace-entry old-location-line-col
                                       new-location-line-col
                                       torus-line-col)
          (torus--add-or-replace-entry old-location-marker
                                       new-location-marker
                                       torus-markers)
          ;; Do it in the end, otherwise it will not be found in helix & history
          (torus--file-position new-position))))))

(defsubst torus--golden-ratio ()
  "Move cursor line in window according to Golden Ratio."
  (let* ((lines (window-text-height))
         (position (ceiling (/ lines 2.61803398875))))
    (recenter position)))

(defun torus--jump (&optional mode)
  "Jump to current location (buffer & position) in torus.
Sync Torus state -> Emacs buffer state.
Add location to `torus-buffers' and `torus-markers' if not already present.
If MODE equals :off-history, don’t write it to `torus-history'.
MODE defaults to nil."
  (if (torus--empty-circle-p)
      (message "Can’t jump on an empty circle.")
    (let* ((location (torus--root-location))
           (file-buffer (car (duo-assoc (car location)
                                        (duo-deref torus-buffers))))
           (location-marker (car (duo-assoc location
                                            (duo-deref torus-markers))))
           (marker (cdr location-marker))
           (buffer (cond (file-buffer
                          (when (> torus-verbosity 0)
                            (message "Buffer %s found" (cdr file-buffer)))
                          (cdr file-buffer))
                         (marker
                          (when (> torus-verbosity 0)
                            (message "Marker %s found" marker))
                          (marker-buffer marker))))
           (position (cond ((and marker (marker-position marker))
                            (when (> torus-verbosity 0)
                              (message "Position found in marker"))
                            (marker-position marker))
                           (t
                            (when (> torus-verbosity 0)
                              (message "Position found in location"))
                            (cdr location)))))
      (unless (buffer-live-p buffer)
        (duo-ref-delete file-buffer torus-buffers)
        (duo-ref-delete-all location torus-markers #'duo-x-match-car-p)
        (setq buffer nil))
      (unless (and marker (marker-position marker))
        (duo-ref-delete location-marker torus-markers))
      (if buffer
          (progn
            (unless (equal buffer (current-buffer))
              (when (> torus-verbosity 0)
                (message "Jumping to buffer %s" buffer))
              (switch-to-buffer buffer))
            (when (> torus-verbosity 0)
              (message "Jumping to position %s" position))
            (goto-char position)
            (torus--golden-ratio))
        (pcase-let ((`(,filename . ,position) location))
          (if (file-exists-p filename)
              (progn
                (when (> torus-verbosity 0)
                  (message "Opening file %s at %s" filename position))
                (find-file filename)
                (goto-char position)
                (torus--golden-ratio))
            (when (> torus-verbosity 0)
              (message "File %s does not exist. Deleting it from variables."
                       filename))
            (dolist (torus (torus--torus-list))
              (dolist (circle (car (cdr torus)))
                (when (duo-ref-delete location (cdr circle))
                  (torus--remove-index (cdr circle))
                  (when (> torus-verbosity 0)
                    (message "Deleting from %s >> %s" (car torus) (car circle))))))
            (torus--seek-location)
            (torus--delete-file-entries filename))))
      (when (file-exists-p (car location))
        (let* ((file-current-buffer (cons (car location) (current-buffer)))
               (location-point-marker (cons location (point-marker))))
          (torus--add-to-line-col)
          (torus--add-entry file-current-buffer torus-buffers)
          (torus--add-entry location-point-marker torus-markers))))
    (unless (eq mode :off-history)
      (torus--add-to-history))
    (torus--status-bar)))

;;; Navigate
;;; ------------------------------------------------------------

(defun torus--tune-torus (torus-name &optional mode)
  "Tune current variables to TORUS-NAME.
If MODE equals :recursive (default), seek circle & location.
Set MODE to a clear keyword, eg :not-recursive, if you don’t want
to seek recursively."
  (if torus-name
      (unless (equal torus-name (torus--torus-name))
        (let* ((mode (or mode :recursive))
               (pair (duo-assoc-index-member torus-name (torus--torus-list)))
               (index (car pair))
               (torus (cdr pair)))
          (when (> torus-verbosity 0)
            (message "Tuning to torus %s : %s" index torus-name))
          (torus--torus-index index)
          (setq torus-cur-torus torus)
          (if (eq mode :recursive)
              (progn
                (when (> torus-verbosity 0)
                  (message "Seeking circle & location."))
                (torus--seek-circle)
                (torus--seek-location))
            (setq torus-cur-circle nil)
            (setq torus-last-circle nil)
            (setq torus-cur-location nil)
            (setq torus-last-location nil))))
    (error "In torus--tune-torus : torus-name is nil")))

(defun torus--tune-circle (circle-name &optional mode)
  "Tune current variables to CIRCLE-NAME.
If MODE equals :recursive (default), seek location.
Set MODE to a clear keyword, eg :not-recursive, if you don’t want
to seek recursively."
  (if circle-name
      (unless (equal circle-name (torus--circle-name))
        (let* ((mode (or mode :recursive))
               (pair (duo-assoc-index-member circle-name (torus--circle-list)))
               (index (car pair))
               (circle (cdr pair)))
          (when (> torus-verbosity 0)
            (message "Tuning to circle %s : %s" index circle-name))
          (torus--circle-index index)
          (setq torus-cur-circle circle)
          (if (eq mode :recursive)
              (progn
                (when (> torus-verbosity 0)
                  (message "Seeking location."))
                (torus--seek-location))
            (setq torus-cur-location nil)
            (setq torus-last-location nil))))
    (error "In torus--tune-circle : circle-name is nil")))

(defun torus--tune-location (location)
  "Tune current variables to LOCATION."
  (if location
      (let* ((pair (duo-index-member location (torus--location-list)))
             (index (car pair))
             (cur-location (cdr pair)))
        (when (> torus-verbosity 0)
          (message "Tuning to location %s : %s" index location))
        (torus--location-index index)
        (setq torus-cur-location cur-location))
    (error "In torus--tune-location : location is nil")))

(defun torus--tune (entry)
  "Go to torus, circle and location according to ENTRY."
  (pcase-let* ((`((,torus-name . ,circle-name) . ,location) entry))
      (torus--tune-torus torus-name :not-recursive)
      (torus--tune-circle circle-name :not-recursive)
      (torus--tune-location location)))

;;; Window
;;; ------------------------------------------------------------

(defsubst torus--windows ()
  "Windows displaying a torus buffer."
  (duo-filter (window-list) (lambda (elem) (torus--inside-p (window-buffer elem)))))

(defun torus--main-windows ()
  "Return main window of layout."
  (let ((windows (torus--windows)))
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
;;; ------------------------------------------------------------

(defun torus--prefix-argument-split (prefix)
  "Handle prefix argument PREFIX. Used to split."
  (pcase prefix
   ('(4)
    (split-window-below)
    (other-window 1))
   ('(16)
    (split-window-right)
    (other-window 1))))

(defun torus--complete-and-clean-layout ()
  "Fill `torus-split-layout' from missing elements. Delete useless ones."
  (let ((paths (mapcar #'car torus-helix)))
    (delete-dups paths)
    (dolist (elem paths)
      (unless (assoc elem torus-split-layout)
        (push (cons elem ?m) torus-split-layout)))
    (dolist (elem torus-split-layout)
      (unless (member (car elem) paths)
        (setq torus-split-layout (torus--assoc-delete-all (car elem) torus-split-layout))))
    (setq torus-split-layout (reverse torus-split-layout))))

(defun torus--apply-or-push-layout ()
  "Apply layout of current circle, or add default is not present."
  (let* ((path (cons (car (car torus-cur-torus))
                     (car (car torus-cur-circle))))
         (entry (assoc path torus-split-layout)))
    (if entry
        (torus-split-layout-menu (cdr entry))
      (push (cons path ?m) torus-split-layout))))

;;; Hooks & Advices
;;; ----------------------------------------------------------------------

;;;###autoload
(defun torus-hello ()
  "Read torus on startup."
  (message "Torus hello.")
  (when torus-load-on-startup
    (if torus-autoread-file
        (torus-read)
      (message "Set torus-autoread-file if you want to load it."))))

;;;###autoload
(defun torus-bye ()
  "Write torus before quit."
  (message "Torus bye.")
  (when torus-save-on-exit
    (if torus-autowrite-file
        (torus-write)
      (when (y-or-n-p "Write torus ? ")
        (call-interactively 'torus-write)))))

;;;###autoload
(defun torus-after-save-torus-file ()
  "Ask whether to read torus file after edition."
  (let* ((filename (buffer-file-name (current-buffer)))
         (directory (file-name-directory filename))
         (torus-folder (torus--full-directory)))
    (when (equal directory torus-folder)
      (when (y-or-n-p "Apply changes to current torus variables ? ")
        (torus-read filename)))))

;;;###autoload
(defun torus-advice-switch-buffer (&rest args)
  "Advice to `switch-to-buffer'. ARGS are irrelevant."
  (when (> torus-verbosity 2)
    (message "Advice called with args %s" args))
  (when (torus--inside-p)
    (torus--update-position)))

;;; Compatibility
;;; ----------------------------------------------------------------------

(defun torus--convert-version-1-variables ()
  "Convert version 1 variables format to new one."
  (when (and (boundp 'torus-meta)
             (boundp 'torus-meta-history)
             (boundp 'torus-input-history)
             (boundp 'torus-layout)
             (torus--empty-wheel-p))
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
        (torus-add-torus torus-name)
        (dolist (circle (cdr torus))
          (setq circle-name (car circle))
          (torus-add-circle circle-name)
          (dolist (location (cdr circle))
            (torus-add-location location))
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
    (setq torus-history (list nil))
    (setq torus-cur-history nil)
    (let ((entry))
      (pcase-dolist (`(,location . (,circle-name . ,torus-name)) torus-meta-history)
        (when (and torus-name circle-name location)
          (setq entry (cons (cons torus-name circle-name) location))
          (torus--add-to-history entry))))
    (setq torus-cur-history (duo-ref-reverse torus-history))
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
            (torus--add-entry entry torus-split-layout)))))
    ;; --- torus-line-col ----
    ;; Nothing to do
    ))

(defun torus--unbound-version-1-variables ()
  "Unintern version 1 variables."
  (makunbound 'torus-meta)
  (makunbound 'torus-meta-index)
  (makunbound 'torus-meta-history)
  (makunbound 'torus-torus)
  (makunbound 'torus-layout))

;;; Commands
;;; ----------------------------------------------------------------------

;;; Init
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-init ()
  "Initialize torus. Add hooks and advices.
Create `torus-dirname' if needed."
  (interactive)
  (add-hook 'emacs-startup-hook 'torus-hello)
  ;; (add-hook 'after-init-hook 'torus-hello)
  (add-hook 'kill-emacs-hook 'torus-bye)
  (add-hook 'after-save-hook 'torus-after-save-torus-file)
  (advice-add #'switch-to-buffer :before #'torus-advice-switch-buffer))

;;; Bindings
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-install-default-bindings ()
  "Install default keybindings."
  (interactive)
  ;; Keyboard
  (if (stringp torus-prefix-key)
      (global-set-key (kbd torus-prefix-key) 'torus-map)
    (global-set-key torus-prefix-key 'torus-map))
  (when (>= torus-binding-level 0)
    (define-key torus-map (kbd "a") 'torus-add-here)
    (define-key torus-map (kbd "C-a") 'torus-add-circle)
    (define-key torus-map (kbd "A") 'torus-add-torus)
    (define-key torus-map (kbd "s-a") 'torus-add-menu)
    (define-key torus-map (kbd "<left>") 'torus-previous-location)
    (define-key torus-map (kbd "<right>") 'torus-next-location)
    (define-key torus-map (kbd "<up>") 'torus-previous-circle)
    (define-key torus-map (kbd "<down>") 'torus-next-circle)
    (define-key torus-map (kbd "<S-up>") 'torus-previous-torus)
    (define-key torus-map (kbd "<S-down>") 'torus-next-torus)
    (define-key torus-map (kbd "r") 'torus-read)
    (define-key torus-map (kbd "w") 'torus-write)
    "Basic")
  (when (>= torus-binding-level 1)
    (define-key torus-map (kbd "n") 'torus-rename-file)
    (define-key torus-map (kbd "C-n") 'torus-rename-circle)
    (define-key torus-map (kbd "N") 'torus-rename-torus)
    (define-key torus-map (kbd "d") 'torus-delete-location)
    (define-key torus-map (kbd "C-d") 'torus-delete-circle)
    (define-key torus-map (kbd "D") 'torus-delete-torus)
    (define-key torus-map (kbd "SPC") 'torus-switch-location)
    (define-key torus-map (kbd "C-SPC") 'torus-switch-circle)
    (define-key torus-map (kbd "S-SPC") 'torus-switch-torus)
    (define-key torus-map (kbd "s-SPC") 'torus-switch-menu)
    (define-key torus-map (kbd "s") 'torus-search-location)
    (define-key torus-map (kbd "C-s") 'torus-search-circle)
    (define-key torus-map (kbd "^") 'torus-alternate)
    (define-key torus-map (kbd "s-^") 'torus-alternate-menu)
    (define-key torus-map (kbd "<prior>") 'torus-newer)
    (define-key torus-map (kbd "<next>") 'torus-older)
    (define-key torus-map (kbd "<C-left>") 'torus-move-location-backward)
    (define-key torus-map (kbd "<C-right>") 'torus-move-location-forward)
    (define-key torus-map (kbd "<C-up>") 'torus-move-circle-backward)
    (define-key torus-map (kbd "<C-down>") 'torus-move-circle-forward)
    (define-key torus-map (kbd "<C-S-up>") 'torus-move-torus-backward)
    (define-key torus-map (kbd "<C-S-down>") 'torus-move-torus-forward)
    (define-key torus-map (kbd "m") 'torus-move-location-after)
    (define-key torus-map (kbd "C-m") 'torus-move-circle-after)
    (define-key torus-map (kbd "M") 'torus-move-torus-after)
    "Common")
  (when (>= torus-binding-level 2)
    (define-key torus-map (kbd "<M-left>") 'torus-rotate-circle-left)
    (define-key torus-map (kbd "<M-right>") 'torus-rotate-circle-right)
    (define-key torus-map (kbd "<M-up>") 'torus-rotate-torus-left)
    (define-key torus-map (kbd "<M-down>") 'torus-rotate-torus-right)
    (define-key torus-map (kbd "<M-S-up>") 'torus-rotate-wheel-left)
    (define-key torus-map (kbd "<M-S-down>") 'torus-rotate-wheel-right)
    "Advanced")
  (when (>= torus-binding-level 3)
    (define-key torus-map (kbd "p") 'torus-print-menu)
    (define-key torus-map (kbd "z") 'torus-reset-menu)
    "Debug")
  ;; Mouse
  (define-key torus-map-mouse-torus [header-line mouse-1] 'torus-switch-torus)
  (define-key torus-map-mouse-torus [header-line mouse-2] 'torus-alternate-toruses)
  (define-key torus-map-mouse-torus [header-line mouse-3] 'torus-search-location)
  (define-key torus-map-mouse-torus [header-line mouse-4] 'torus-previous-torus)
  (define-key torus-map-mouse-torus [header-line mouse-5] 'torus-next-torus)
  (define-key torus-map-mouse-circle [header-line mouse-1] 'torus-switch-circle)
  (define-key torus-map-mouse-circle [header-line mouse-2] 'torus-alternate-circles)
  (define-key torus-map-mouse-circle [header-line mouse-3] 'torus-search-circle)
  (define-key torus-map-mouse-circle [header-line mouse-4] 'torus-previous-circle)
  (define-key torus-map-mouse-circle [header-line mouse-5] 'torus-next-circle)
  (define-key torus-map-mouse-location [header-line mouse-1] 'torus-mouse-on-tab)
  (define-key torus-map-mouse-location [header-line mouse-2] 'torus-alternate-in-meta)
  (define-key torus-map-mouse-location [header-line mouse-3] 'torus-switch-location)
  (define-key torus-map-mouse-location [header-line mouse-4] 'torus-previous-location)
  (define-key torus-map-mouse-location [header-line mouse-5] 'torus-next-location))

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
        (torus-alternate-in-same-circle)
      (torus--update-position)
      (torus--seek-location pipes)
      (torus--jump))))

;;; Menus
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-print-menu (choice)
  "Print CHOICE variables."
  (interactive
   (list (read-key torus--msg-print-menu)))
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
      (?h (push 'torus-history varlist))
      (?\^h (push 'torus-cur-history varlist))
      (?u (push 'torus-user-input-history varlist))
      (?\^u (push 'torus-cur-user-input varlist))
      (?s (push 'torus-split-layout varlist))
      (?& (push 'torus-line-col varlist))
      (?b (push 'torus-buffers varlist))
      (?m (push 'torus-markers varlist))
      (?o (push 'torus-original-header-lines varlist))
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
                              'torus-history
                              'torus-cur-history
                              'torus-user-input-history
                              'torus-cur-user-input
                              'torus-split-layout
                              'torus-line-col
                              'torus-buffers
                              'torus-markers
                              'torus-original-header-lines)))
      (?\a (delete-window window)
           (message "Print cancelled by Ctrl-G."))
      (_ (message "Invalid key.")))
    (dolist (var varlist)
      (message "%s" (symbol-name var))
      (pp (symbol-value var)))))

;;;###autoload
(defun torus-reset-menu (choice &optional mode)
  "Reset CHOICE variables to nil.
MODE is :verbose by default.
Don’t print anything is MODE is :quiet."
  (interactive
   (list (read-key torus--msg-reset-menu) :verbose))
  (let ((mode (or mode :verbose))
        (list-nil-vars)
        (nil-vars))
    (pcase choice
      (?w (push 'torus-wheel list-nil-vars)
          (push 'torus-cur-torus nil-vars)
          (push 'torus-last-torus nil-vars)
          (push 'torus-cur-circle nil-vars)
          (push 'torus-last-circle nil-vars)
          (push 'torus-cur-location nil-vars)
          (push 'torus-last-location nil-vars))
      (?t (push 'torus-cur-torus nil-vars))
      (?\^t (push 'torus-last-torus nil-vars))
      (?c (push 'torus-cur-circle nil-vars))
      (?\^c (push 'torus-last-circle nil-vars))
      (?l (push 'torus-cur-location nil-vars))
      (?\^l (push 'torus-last-location nil-vars))
      (?x (push 'torus-helix list-nil-vars)
          (push 'torus-cur-helix nil-vars))
      (?\^x (push 'torus-cur-helix nil-vars))
      (?g (push 'torus-grid list-nil-vars)
          (push 'torus-cur-grid nil-vars))
      (?G (push 'torus-cur-grid nil-vars))
      (?h (push 'torus-history list-nil-vars)
          (push 'torus-cur-history nil-vars))
      (?\^h (push 'torus-cur-history nil-vars))
      (?u (push 'torus-user-input-history list-nil-vars)
          (push 'torus-cur-user-input nil-vars))
      (?\^u (push 'torus-cur-user-input nil-vars))
      (?s (push 'torus-split-layout list-nil-vars))
      (?& (push 'torus-line-col list-nil-vars))
      (?b (push 'torus-buffers list-nil-vars))
      (?m (push 'torus-markers list-nil-vars))
      (?o (push 'torus-original-header-lines list-nil-vars))
      (?a (setq list-nil-vars (list 'torus-wheel
                                    'torus-helix
                                    'torus-grid
                                    'torus-history
                                    'torus-user-input-history
                                    'torus-split-layout
                                    'torus-line-col
                                    'torus-buffers
                                    'torus-markers
                                    'torus-original-header-lines))
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
      (unless (equal mode :quiet)
        (message "%s -> (list nil)" (symbol-name var)))
      (set var (list nil)))
    (dolist (var nil-vars)
      (unless (equal mode :quiet)
        (message "%s -> nil" (symbol-name var)))
      (set var nil))))

;;;###autoload
(defun torus-add-menu (choice)
  "Add object to torus variables according to CHOICE."
  (interactive
   (list (read-key torus--msg-add-menu)))
    (pcase choice
      (?h (call-interactively 'torus-add-here))
      (?f (call-interactively 'torus-add-file))
      (?b (call-interactively 'torus-add-buffer))
      (?l (call-interactively 'torus-add-location))
      (?c (call-interactively 'torus-add-circle))
      (?t (call-interactively 'torus-add-torus))
      (?\a (message "Add cancelled by Ctrl-G."))
      (_ (message "Invalid key."))))

;;;###autoload
(defun torus-switch-menu (choice)
  "Switch according to CHOICE."
  (interactive
   (list (read-key torus--msg-switch-menu)))
    (pcase choice
      (?t (call-interactively 'torus-switch-torus))
      (?c (call-interactively 'torus-switch-circle))
      (?l (call-interactively 'torus-switch-location))
      (?\a (message "Switch cancelled by Ctrl-G."))
      (_ (message "Invalid key."))))

;;;###autoload
(defun torus-alternate-menu (choice)
  "Alternate according to CHOICE."
  (interactive
   (list (read-key torus--msg-alternate-menu)))
  (pcase choice
    (?^ (funcall 'torus-alternate))
    (?c (funcall 'torus-alternate-in-same-circle))
    (?i (funcall 'torus-alternate-in-other-circle))
    (?t (funcall 'torus-alternate-in-same-torus))
    (?o (funcall 'torus-alternate-in-other-torus))
    (?\a (message "Alternate operation cancelled by Ctrl-G."))
    (_ (message "Invalid key."))))

;;;###autoload
(defun torus-split-split-menu (choice)
  "Split according to CHOICE."
  (interactive
   (list (read-key torus--msg-split-menu)))
  (torus--complete-and-clean-layout)
  (let ((circle (caar torus-cur-torus)))
    (when (member choice '(?m ?o ?h ?v ?l ?r ?t ?b ?g))
      (setcdr (assoc circle torus-split-layout) choice))
    (pcase choice
      (?m nil)
      (?o (delete-other-windows))
      (?h (funcall 'torus-split-horizontally))
      (?v (funcall 'torus-split-vertically))
      (?l (funcall 'torus-split-main-left))
      (?r (funcall 'torus-split-main-right))
      (?t (funcall 'torus-split-main-top))
      (?b (funcall 'torus-split-main-bottom))
      (?g (funcall 'torus-split-grid))
      (?\a (message "Layout cancelled by Ctrl-G."))
      (_ (message "Invalid key.")))))

;;; Read & Write
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-read (&optional filename interactive-p)
  "Read main torus variables from FILENAME as Lisp code.
It’s assumed to be called interactively when INTERACTIVE-P is not nil.
An adequate path and extension is added if needed.
The directory is created if needed."
  (interactive
   (list
    (read-file-name
     "Torus file : "
     (file-name-as-directory torus-dirname))
    t))
  (setq filename (or filename torus-autoread-file))
  (torus--add-user-input filename)
  (when (or (not interactive-p)
            (torus--empty-wheel-p)
            (y-or-n-p torus--msg-replace-variables))
    (let* ((file (torus--complete-filename filename))
           (directory (file-name-directory file))
           (buffer))
      (torus--make-dir directory)
      (if (file-exists-p file)
          (progn
            (when (> torus-verbosity 0)
              (message "Reading file %s" file))
            (torus-reset-menu ?a :quiet)
            (setq buffer (find-file-noselect file))
            (eval-buffer buffer)
            (kill-buffer buffer)
            ;; Version 1 variables
            (torus--convert-version-1-variables)
            (torus--unbound-version-1-variables)
            ;; Seek
            (torus--seek-torus)
            (torus--seek-circle)
            (torus--seek-location)
            (setq torus-cur-helix (duo-deref torus-helix))
            (setq torus-cur-grid (duo-deref torus-grid))
            (setq torus-cur-history (duo-deref torus-history))
            (setq torus-cur-user-input (duo-deref torus-user-input-history))
            ;; Jump to current location
            (torus--jump))
        (message "File %s does not exist." file)))))

;;;###autoload
(defun torus-write (&optional filename)
  "Write main torus variables to FILENAME as Lisp code.
An adequate path and extension is added if needed.
The directory is created if needed."
  (interactive
   (list
    (read-file-name
     "Torus file : "
     (file-name-as-directory torus-dirname))))
  (setq filename (or filename torus-autowrite-file))
  (torus--add-user-input filename)
  ;; Let’s write
  (if (torus--empty-wheel-p)
      (message "Write cancelled : wheel is empty.")
    (let* ((file (torus--complete-filename filename))
           (directory (file-name-directory file))
           (buffer (find-file-noselect file))
           (varlist (list 'torus-wheel
                          'torus-helix
                          'torus-grid
                          'torus-history
                          'torus-user-input-history
                          'torus-split-layout
                          'torus-line-col)))
      (torus--make-dir directory)
      (torus--update-position)
      (torus--roll-backups file)
      ;; We surely don’t want to read a file we’ve just written
      (remove-hook 'after-save-hook 'torus-after-save-torus-file)
      ;; Do the thing
      (with-current-buffer buffer
        (when (> torus-verbosity 0)
          (message "Writing file %s" file))
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
        (kill-buffer))
      ;; Restore the hook
      (add-hook 'after-save-hook 'torus-after-save-torus-file))))

;;; Add
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-add-torus (torus-name)
  "Create a new torus named TORUS-NAME in `torus-wheel'."
  (interactive
   (list (read-string "Name of the new torus : " nil 'torus-cur-user-input)))
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
      (message "Torus %s is already present in wheel." torus-name)
      nil)))

;;;###autoload
(defun torus-add-circle (circle-name)
  "Add a new circle CIRCLE-NAME to current torus."
  (interactive
   (list (read-string "Name of the new circle : " nil 'torus-cur-user-input)))
  (unless torus-cur-torus
    (call-interactively 'torus-add-torus))
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
      (message "Circle %s is already present in torus %s."
               circle-name
               torus-name)
      nil)))

;;;###autoload
(defun torus-add-location (location)
  "Add LOCATION to current circle."
  (interactive (list (read-string "New location : " nil 'torus-cur-user-input)))
  (unless torus-cur-torus
    (call-interactively 'torus-add-torus))
  (unless torus-cur-circle
    (call-interactively 'torus-add-circle))
  (torus--add-user-input (prin1-to-string location))
  (let* ((location (if (consp location)
                       location
                     (car (read-from-string location))))
         (member (duo-member location (torus--location-list))))
    (if member
        (progn
          (message "Location %s is already present in torus %s circle %s."
                   location
                   (torus--torus-name)
                   (torus--circle-name))
          nil)
      (if (and (stringp (car location)) (integerp (cdr location)))
          (progn
            (setq torus-last-location (duo-ref-add location
                                                   (torus--ref-location-list)
                                                   torus-last-location))
            (setq torus-cur-location torus-last-location)
            (torus--add-index (torus--ref-location-list))
            (torus--add-to-helix)
            (torus--add-to-history)
            torus-cur-location)
        (error "In torus-add-location : bad argument format")))))

;;;###autoload
(defun torus-add-here ()
  "Add current file and point to current circle."
  (interactive)
  (if (buffer-file-name)
      (let* ((buffer (current-buffer))
             (pointmark (point-marker))
             (location (cons (buffer-file-name) (marker-position pointmark)))
             (file-buffer (cons (car location) buffer))
             (location-marker (cons location pointmark)))
        (torus-add-location location)
        (torus--add-to-line-col)
        (duo-ref-push-new file-buffer torus-buffers)
        (duo-ref-push-new location-marker torus-markers)
        (torus--status-bar)
        torus-cur-location)
    (message "Buffer must have a filename to be added to the torus.")
    nil))

;;;###autoload
(defun torus-add-file (file-name)
  "Add FILE-NAME to the current circle."
  (interactive (list (read-file-name "File to add : ")))
  (if (file-exists-p file-name)
      (progn
        (find-file file-name)
        (torus-add-here))
    (message "File %s does not exist." file-name)
    nil))

;;;###autoload
(defun torus-add-buffer (buffer-name)
  "Add BUFFER-NAME at current position to the current circle."
  (interactive (list (read-buffer "Buffer to add : ")))
  (if (buffer-live-p (get-buffer buffer-name))
      (progn
        (switch-to-buffer buffer-name)
        (torus-add-here))
    (message "Buffer %s does not exist." buffer-name)
    nil))

;;; Rename
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-rename-torus (torus-name)
  "Rename current torus."
  (interactive
   (list (read-string (format "New name of torus %s : " (torus--torus-name))
                      nil
                      'torus-user-input-history)))
  (if (torus--empty-wheel-p)
      (message "Wheel is empty. Please add a torus first.")
    (unless (= (length torus-name) 0)
      (let ((old-name (torus--torus-name)))
        (torus--add-user-input torus-name)
        (torus--torus-name torus-name)
        (duo-replace-all-caar old-name torus-name (duo-deref torus-helix))
        (duo-replace-all-car old-name torus-name (duo-deref torus-grid))
        (duo-replace-all-caar old-name torus-name (duo-deref torus-history))
        (duo-replace-all-caar old-name torus-name (duo-deref torus-split-layout))
        (message "Renamed torus %s -> %s" old-name torus-name)))))

;;;###autoload
(defun torus-rename-circle (circle-name)
  "Rename current circle."
  (interactive
   (list (read-string (format "New name of circle %s : " (torus--circle-name))
                      nil
                      'torus-user-input-history)))
  (if (torus--empty-torus-p)
      (message "Torus is empty. Please add a circle first.")
    (unless (= (length circle-name) 0)
      (let ((old-name (torus--circle-name)))
        (torus--add-user-input circle-name)
        (torus--circle-name circle-name)
        (duo-replace-all-cdar old-name circle-name (duo-deref torus-helix))
        (duo-replace-cdr old-name circle-name (duo-deref torus-grid))
        (duo-replace-all-cdar old-name circle-name (duo-deref torus-history))
        (duo-replace-cdar old-name circle-name (duo-deref torus-split-layout))
        (message "Renamed circle %s -> %s" old-name circle-name)))))

(defun torus-rename-file (file-name)
  "Rename current file in FILE-NAME."
  (interactive
   (list (read-string (format "New name of file %s : " (torus--file-name))
                      nil
                      'torus-user-input-history)))
  (if (torus--empty-circle-p)
      (message "Circle is empty. Please add a location first.")
    (unless (= (length file-name) 0)
      (let ((old-name (torus--file-name))
            (modified (buffer-modified-p)))
        (torus--jump)
        (if (not (equal old-name (buffer-file-name)))
            (message "Can’t rename file : current location does not match current buffer.")
          (torus--add-user-input file-name)
          (unless (file-name-absolute-p file-name)
            (setq file-name (concat (file-name-directory (buffer-file-name))
                                    file-name)))
          (when (file-exists-p old-name)
            (rename-file old-name file-name))
          (set-visited-file-name file-name)
          (if modified
              (save-buffer)
            (set-buffer-modified-p nil))
          (torus--file-name file-name)
          (duo-replace-all-cadr old-name file-name (duo-deref torus-helix))
          (duo-replace-all-cadr old-name file-name (duo-deref torus-history))
          (duo-replace-all-caar old-name file-name (duo-deref torus-line-col))
          (duo-replace-car old-name file-name (duo-deref torus-buffers))
          (duo-replace-all-caar old-name file-name (duo-deref torus-markers))
          (message "Renamed file %s -> %s" old-name file-name))))))

;;; Delete
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-delete-torus (torus-name &optional mode)
  "Delete torus given by TORUS-NAME.
if MODE equals :force, don’t ask confirmation.
MODE defaults to nil."
  (interactive
   (list (completing-read
          "Delete torus : "
          (mapcar #'car (torus--torus-list)) nil t)))
  (when (or (equal mode :force)
            (y-or-n-p (format "Delete torus %s ? " torus-name)))
    (duo-ref-delete torus-name (torus--ref-torus-list) nil #'duo-x-match-car-p)
    (torus--remove-index (torus--ref-torus-list))
    (if (torus--empty-wheel-p)
        (progn
          (setq torus-cur-torus nil)
          (setq torus-cur-circle nil)
          (setq torus-cur-location nil)
          (setq torus-last-torus nil)
          (setq torus-last-circle nil)
          (setq torus-last-location nil)
          (setq torus-cur-helix nil)
          (setq torus-cur-grid nil)
          (setq torus-cur-history nil))
      (torus--seek-torus)
      (torus--seek-circle)
      (torus--seek-location)
      (torus--jump))
    (duo-ref-delete-all torus-name torus-helix #'duo-x-match-caar-p)
    (duo-ref-delete-all torus-name torus-grid #'duo-x-match-car-p)
    (duo-ref-delete-all torus-name torus-history #'duo-x-match-caar-p)
    (duo-ref-delete-all torus-name torus-split-layout #'duo-x-match-caar-p)
    (setq torus-cur-helix (duo-deref torus-helix))
    (setq torus-cur-grid (duo-deref torus-grid))
    (setq torus-cur-history (duo-deref torus-history))))

;;;###autoload
(defun torus-delete-circle (circle-name &optional mode)
  "Delete circle given by CIRCLE-NAME.
if MODE equals :force, don’t ask confirmation.
MODE defaults to nil."
  (interactive
   (list (completing-read
          "Delete circle : "
          (mapcar #'car (torus--circle-list)) nil t)))
  (when (or (equal mode :force)
            (y-or-n-p (format "Delete circle %s ? " circle-name)))
    (duo-ref-delete circle-name (torus--ref-circle-list) nil #'duo-x-match-car-p)
    (torus--remove-index (torus--ref-circle-list))
    (if (torus--empty-torus-p)
        (progn
          (setq torus-cur-circle nil)
          (setq torus-cur-location nil)
          (setq torus-last-circle nil)
          (setq torus-last-location nil))
      (torus--seek-circle)
      (torus--seek-location)
      (torus--jump))
    (let ((path (cons (torus--torus-name) circle-name)))
      (duo-ref-delete-all path torus-helix #'duo-x-match-car-p)
      (duo-ref-delete path torus-grid)
      (duo-ref-delete-all path torus-history #'duo-x-match-car-p)
      (duo-ref-delete path torus-split-layout nil #'duo-x-match-car-p)
      (setq torus-cur-helix (duo-deref torus-helix))
      (setq torus-cur-grid (duo-deref torus-grid))
      (setq torus-cur-history (duo-deref torus-history)))))

;;;###autoload
(defun torus-delete-location (location &optional mode)
  "Delete LOCATION from torus.
if MODE equals :force, don’t ask confirmation.
MODE defaults to nil."
  (interactive
   (list (completing-read
          "Delete location : "
          (mapcar #'torus--entry-to-string (torus--location-list)) nil t)))
  (when (or (equal mode :force)
            (y-or-n-p (format "Delete location %s ? " location)))
    (unless (consp location)
      (let ((string-list (mapcar #'torus--entry-to-string
                                 (torus--location-list))))
        (setq location (car (duo-at-index (duo-index-of location string-list)
                                          (torus--location-list))))))
    (duo-ref-delete location (torus--ref-location-list))
    (torus--remove-index (torus--ref-location-list))
    (if (torus--empty-circle-p)
        (progn
          (setq torus-cur-location nil)
          (setq torus-last-location nil))
      (torus--seek-location)
      (torus--jump))
    (let* ((path (cons (torus--torus-name) (torus--circle-name)))
           (entry (cons path location)))
      (duo-ref-delete entry torus-helix)
      (duo-ref-delete entry torus-history)
      (setq torus-cur-helix (duo-deref torus-helix))
      (setq torus-cur-history (duo-deref torus-history)))))

;;; Next / Previous
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-previous-torus ()
  "Jump to the previous torus.
With prefix argument \\[universal-argument],
open the buffer in a horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument],
open the buffer in a vertical split."
  (interactive)
  (if (torus--empty-wheel-p)
      (message torus--msg-empty-wheel)
    (torus--prefix-argument-split current-prefix-arg)
    (torus--update-position)
    (torus--decrease-index (torus--ref-torus-list))
    (setq torus-cur-torus
          (duo-circ-previous torus-cur-torus (torus--torus-list)))
    (torus--seek-circle)
    (torus--seek-location)
    (torus--jump))
  torus-cur-torus)

;;;###autoload
(defun torus-next-torus ()
  "Jump to the next torus.
With prefix argument \\[universal-argument],
open the buffer in a horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument],
open the buffer in a vertical split."
  (interactive)
  (if (torus--empty-wheel-p)
      (message torus--msg-empty-wheel)
    (torus--prefix-argument-split current-prefix-arg)
    (torus--update-position)
    (torus--increase-index (torus--ref-torus-list))
    (setq torus-cur-torus
          (duo-circ-next torus-cur-torus (torus--torus-list)))
    (torus--seek-circle)
    (torus--seek-location)
    (torus--jump))
  torus-cur-torus)

;;;###autoload
(defun torus-previous-circle ()
  "Jump to the previous circle.
With prefix argument \\[universal-argument],
open the buffer in a horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument],
open the buffer in a vertical split."
  (interactive)
  (if (torus--empty-torus-p)
      (message torus--msg-empty-torus (torus--torus-name))
    (torus--prefix-argument-split current-prefix-arg)
    (torus--update-position)
    (torus--decrease-index (torus--ref-circle-list))
    (setq torus-cur-circle
          (duo-circ-previous torus-cur-circle (torus--circle-list)))
    (torus--seek-location)
    (torus--jump))
  torus-cur-circle)

;;;###autoload
(defun torus-next-circle ()
  "Jump to the next circle.
With prefix argument \\[universal-argument],
open the buffer in a horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument],
open the buffer in a vertical split."
  (interactive)
  (if (torus--empty-torus-p)
      (message torus--msg-empty-torus (torus--torus-name))
    (torus--prefix-argument-split current-prefix-arg)
    (torus--update-position)
    (torus--increase-index (torus--ref-circle-list))
    (setq torus-cur-circle
          (duo-circ-next torus-cur-circle (torus--circle-list)))
    (torus--seek-location)
    (torus--jump))
  torus-cur-circle)

;;;###autoload
(defun torus-previous-location ()
  "Jump to the previous location.
With prefix argument \\[universal-argument],
open the buffer in a horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument],
open the buffer in a vertical split."
  (interactive)
  (if (torus--empty-circle-p)
      (message torus--msg-empty-circle (torus--circle-name) (torus--torus-name))
    (torus--prefix-argument-split current-prefix-arg)
    (torus--update-position)
    (torus--decrease-index (torus--ref-location-list))
    (setq torus-cur-location
          (duo-circ-previous torus-cur-location (torus--location-list)))
    (torus--jump))
  torus-cur-location)

;;;###autoload
(defun torus-next-location ()
  "Jump to the next location.
With prefix argument \\[universal-argument],
open the buffer in a horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument],
open the buffer in a vertical split."
  (interactive)
  (if (torus--empty-circle-p)
      (message torus--msg-empty-circle (torus--circle-name) (torus--torus-name))
    (torus--prefix-argument-split current-prefix-arg)
    (torus--update-position)
    (torus--increase-index (torus--ref-location-list))
    (setq torus-cur-location
          (duo-circ-next torus-cur-location (torus--location-list)))
    (torus--jump))
  torus-cur-location)

;;; Switch
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-switch-torus (torus-name)
  "Switch to TORUS-NAME torus.
With prefix argument \\[universal-argument],
open the buffer in a horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument],
open the buffer in a vertical split."
  (interactive
   (list (completing-read
          "Switch to torus : "
          (mapcar #'car (torus--torus-list)) nil t)))
  (torus--prefix-argument-split current-prefix-arg)
  (torus--update-position)
  (torus--tune-torus torus-name)
  (torus--jump))

;;;###autoload
(defun torus-switch-circle (circle-name)
  "Switch to CIRCLE-NAME circle in current torus.
With prefix argument \\[universal-argument],
open the buffer in a horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument],
open the buffer in a vertical split."
  (interactive
   (list (completing-read
          "Switch to circle : "
          (mapcar #'car (torus--circle-list)) nil t)))
  (torus--prefix-argument-split current-prefix-arg)
  (torus--update-position)
  (torus--tune-circle circle-name)
  (torus--jump))

;;;###autoload
(defun torus-switch-location (location)
  "Switch to LOCATION in current circle and torus.
With prefix argument \\[universal-argument],
open the buffer in a horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument],
open the buffer in a vertical split."
  (interactive
   (list
    (completing-read
     "Switch to location : "
     (mapcar #'torus--entry-to-string (torus--location-list)) nil t)))
  (torus--prefix-argument-split current-prefix-arg)
  (let* ((string-list (mapcar #'torus--entry-to-string (torus--location-list)))
         (index (if (consp location)
                    (duo-index-of location (torus--location-list))
                  (duo-index-of location string-list))))
    (when (> torus-verbosity 0)
        (message "Switching to location %s : %s" index location))
    (torus--update-position)
    (torus--location-index index)
    (setq torus-cur-location (duo-at-index index (torus--location-list))))
  (torus--jump))

;;; Search
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-search-location (entry-string)
  "Search torus, circle and location matching ENTRY-STRING in `torus-helix'."
  (interactive
   (list
    (completing-read
     "Search location : "
     (mapcar #'torus--entry-to-string (duo-deref torus-helix)) nil t)))
  (torus--prefix-argument-split current-prefix-arg)
  (let* ((index (duo-index-of entry-string
                              (mapcar #'torus--entry-to-string
                                      (duo-deref torus-helix))))
         (entry))
    (torus--update-position)
    (setq entry (car (duo-at-index index (duo-deref torus-helix))))
    (torus--tune entry)
    (torus--jump)))

;;;###autoload
(defun torus-search-circle (entry-string)
  "Search Torus & Circle matching ENTRY-STRING in `torus-grid'."
  (interactive
   (list
    (completing-read
     "Search circle : "
     (mapcar #'torus--entry-to-string (duo-deref torus-grid)) nil t)))
  (torus--prefix-argument-split current-prefix-arg)
  (let* ((index (duo-index-of entry-string
                              (mapcar #'torus--entry-to-string
                                      (duo-deref torus-grid))))
         (entry (car (duo-at-index index (duo-deref torus-grid)))))
    (torus--update-position)
    (pcase-let* ((`(,torus-name . ,circle-name) entry))
      (torus--tune-torus torus-name :not-recursive)
      (torus--tune-circle circle-name))
    (torus--jump)))

;;; History
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-newer ()
  "Go to newer location in history."
  (interactive)
  (if (torus--empty-history-p)
      (message "No newer entry in empty history.")
    (torus--update-position)
    (setq torus-cur-history (duo-ref-rotate-right torus-history))
    (torus--tune (car torus-cur-history))
    (torus--jump)))

;;;###autoload
(defun torus-older ()
  "Go to older location in history."
  (interactive)
  (if (torus--empty-history-p)
      (message "No older entry in empty history.")
    (torus--update-position)
    (setq torus-cur-history (duo-ref-rotate-left torus-history))
    (torus--tune (car torus-cur-history))
    (torus--jump)))

;;; Alternate
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-alternate ()
  "Alternate last two locations in history.
If outside the torus, just return inside, to the last torus location."
  (interactive)
  (when (torus--inside-p)
    (if (torus--empty-history-p)
        (message "No older entry in empty history.")
      (let ((history (duo-deref torus-history)))
        (if (< (length history) 2)
            (message "Can’t alternate : history has less than two elements.")
          (torus--update-position)
          (setq torus-cur-history (cdr torus-cur-history))
          (duo-ref-teleport-cons-previous history torus-cur-history torus-history)
          (torus--tune (car torus-cur-history))))))
  (torus--jump))

;;;###autoload
(defun torus-alternate-in-same-torus ()
  "Alternate last two locations in history belonging to the current circle.
If outside the torus, just return inside, to the last torus location."
  (interactive)
  (when (torus--inside-p)
    (if (torus--empty-history-p)
        (message "No older entry in empty history.")
      (let ((history (duo-deref torus-history)))
        (if (< (length history) 2)
            (message "Can’t alternate : history has less than two elements.")
          (torus--update-position)
          (setq torus-cur-history (duo-circ-next-in-group torus-cur-history
                                                          history
                                                          #'duo-equal-caar-p))
          (duo-ref-teleport-cons-previous history torus-cur-history torus-history)
          (torus--tune (car torus-cur-history))))))
  (torus--jump))

;;;###autoload
(defun torus-alternate-in-same-circle ()
  "Alternate last two locations in history belonging to the current circle.
If outside the torus, just return inside, to the last torus location."
  (interactive)
  (when (torus--inside-p)
    (if (torus--empty-history-p)
        (message "No older entry in empty history.")
      (let ((history (duo-deref torus-history)))
        (if (< (length history) 2)
            (message "Can’t alternate : history has less than two elements.")
          (torus--update-position)
          (setq torus-cur-history (duo-circ-next-in-group torus-cur-history
                                                          history
                                                          #'duo-equal-car-p))
          (duo-ref-teleport-cons-previous history torus-cur-history torus-history)
          (torus--tune (car torus-cur-history))))))
  (torus--jump))

;;;###autoload
(defun torus-alternate-in-other-torus ()
  "Alternate last two toruses in meta history.
If outside the torus, just return inside, to the last torus location."
  (interactive)
  (when (torus--inside-p)
    (if (torus--empty-history-p)
        (message "No older entry in empty history.")
      (let ((history (duo-deref torus-history)))
        (if (< (length history) 2)
            (message "Can’t alternate : history has less than two elements.")
          (torus--update-position)
          ;; TODO
          (setq torus-cur-history (duo-circ-next-not-in-group torus-cur-history
                                                          history
                                                          #'duo-equal-caar-p))
          (duo-ref-teleport-cons-previous history torus-cur-history torus-history)
          (torus--tune (car torus-cur-history))))))
  (torus--jump))

;;;###autoload
(defun torus-alternate-in-other-circle ()
  "Alternate last two circles in history.
If outside the torus, just return inside, to the last torus location."
  (interactive)
  (when (torus--inside-p)
    (if (torus--empty-history-p)
        (message "No older entry in empty history.")
      (let ((history (duo-deref torus-history)))
        (if (< (length history) 2)
            (message "Can’t alternate : history has less than two elements.")
          (torus--update-position)
          ;; TODO
          (setq torus-cur-history (duo-circ-next-not-in-group torus-cur-history
                                                          history
                                                          #'duo-equal-car-p))
          (duo-ref-teleport-cons-previous history torus-cur-history torus-history)
          (torus--tune (car torus-cur-history))))))
  (torus--jump))

;;; Move
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-move-torus-backward ()
  "Move the current torus to the previous position."
  (interactive)
  (if (torus--empty-wheel-p)
      (message torus--msg-empty-wheel)
    (torus--update-position)
    (torus--decrease-index (torus--ref-torus-list))
    (duo-ref-circ-move-previous torus-cur-torus (torus--ref-torus-list))
    (torus--wheel-status))
  torus-cur-torus)

;;;###autoload
(defun torus-move-torus-forward ()
  "Move the current torus to the next position."
  (interactive)
  (if (torus--empty-wheel-p)
      (message torus--msg-empty-wheel)
    (torus--update-position)
    (torus--increase-index (torus--ref-torus-list))
    (duo-ref-circ-move-next torus-cur-torus (torus--ref-torus-list))
    (torus--wheel-status))
  torus-cur-torus)

;;;###autoload
(defun torus-move-circle-backward ()
  "Move the current circle to the previous position."
  (interactive)
  (if (torus--empty-torus-p)
      (message torus--msg-empty-torus (torus--torus-name))
    (torus--update-position)
    (torus--decrease-index (torus--ref-circle-list))
    (duo-ref-circ-move-previous torus-cur-circle (torus--ref-circle-list))
    (torus--torus-status))
  torus-cur-circle)

;;;###autoload
(defun torus-move-circle-forward ()
  "Move the current circle to the next position."
  (interactive)
  (if (torus--empty-torus-p)
      (message torus--msg-empty-torus (torus--torus-name))
    (torus--update-position)
    (torus--increase-index (torus--ref-circle-list))
    (duo-ref-circ-move-next torus-cur-circle (torus--ref-circle-list))
    (torus--torus-status))
  torus-cur-circle)

;;;###autoload
(defun torus-move-location-backward ()
  "Move the current location to the previous position."
  (interactive)
  (if (torus--empty-circle-p)
      (message torus--msg-empty-circle (torus--circle-name) (torus--torus-name))
    (torus--update-position)
    (torus--decrease-index (torus--ref-location-list))
    (duo-ref-circ-move-previous torus-cur-location (torus--ref-location-list))
    (force-mode-line-update t))
  torus-cur-location)

;;;###autoload
(defun torus-move-location-forward ()
  "Move the current location to the next position."
  (interactive)
  (if (torus--empty-circle-p)
      (message torus--msg-empty-circle (torus--circle-name) (torus--torus-name))
    (torus--update-position)
    (torus--increase-index (torus--ref-location-list))
    (duo-ref-circ-move-next torus-cur-location (torus--ref-location-list))
    (force-mode-line-update t))
  torus-cur-location)

;;; After
;;; ------------------------------

;;;###autoload
(defun torus-move-torus-after (torus-name)
  "Move current torus after TORUS-NAME."
  (interactive
   (list (completing-read
          "Move current torus after : "
          (mapcar #'car (torus--torus-list)) nil t)))
  (torus--update-position)
  (duo-ref-teleport-cons-after torus-name
                               torus-cur-torus
                               (torus--ref-torus-list)
                               nil
                               #'duo-x-match-car-p)
  (let ((index (duo-index-of (torus--root-torus) (torus--torus-list))))
    (torus--torus-index index))
  (torus--wheel-status))

;;;###autoload
(defun torus-move-circle-after (circle-name)
  "Move current circle after CIRCLE-NAME."
  (interactive
   (list (completing-read
          "Move current circle after : "
          (mapcar #'car (torus--circle-list)) nil t)))
  (torus--update-position)
  (duo-ref-teleport-cons-after circle-name
                               torus-cur-circle
                               (torus--ref-circle-list)
                               nil
                               #'duo-x-match-car-p)
  (let ((index (duo-index-of (torus--root-circle) (torus--circle-list))))
    (torus--circle-index index))
  (torus--torus-status))

;;;###autoload
(defun torus-move-location-after (location)
  "Move current location after LOCATION."
  (interactive
   (list
    (completing-read
     "Move current location after : "
     (mapcar #'torus--entry-to-string (torus--location-list)) nil t)))
  (torus--update-position)
  (let* ((string-list (mapcar #'torus--entry-to-string (torus--location-list)))
         (index (if (consp location)
                    (duo-index-of location (torus--location-list))
                  (duo-index-of location string-list))))
    (unless (consp location)
      (setq location (car (duo-at-index index (torus--location-list)))))
    (duo-ref-teleport-cons-after location
                                 torus-cur-location
                                 (torus--ref-location-list)
                                 nil)
    (torus--location-index (1+ index)))
  (force-mode-line-update t))

;;; Rotate
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-rotate-wheel-left ()
  "Rotate toruses of the wheel to the left (backward)."
  (interactive)
  (if (torus--empty-wheel-p)
      (message torus--msg-empty-wheel)
    (torus--update-position)
    (torus--decrease-index (torus--ref-torus-list))
    (duo-ref-rotate-left (torus--ref-torus-list))
    (torus--wheel-status))
  torus-cur-torus)

;;;###autoload
(defun torus-rotate-wheel-right ()
  "Rotate toruses of the wheel to the right (forward)."
  (interactive)
  (if (torus--empty-wheel-p)
      (message torus--msg-empty-wheel)
    (torus--update-position)
    (torus--increase-index (torus--ref-torus-list))
    (duo-ref-rotate-right (torus--ref-torus-list))
    (torus--wheel-status))
  torus-cur-torus)

;;;###autoload
(defun torus-rotate-torus-left ()
  "Rotate circles of the current torus to the left (backward)."
  (interactive)
  (if (torus--empty-torus-p)
      (message torus--msg-empty-torus (torus--torus-name))
    (torus--update-position)
    (torus--decrease-index (torus--ref-circle-list))
    (duo-ref-rotate-left (torus--ref-circle-list))
    (torus--torus-status))
  torus-cur-circle)

;;;###autoload
(defun torus-rotate-torus-right ()
  "Rotate circles of the current torus to the right (forward)."
  (interactive)
  (if (torus--empty-torus-p)
      (message torus--msg-empty-torus (torus--torus-name))
    (torus--update-position)
    (torus--increase-index (torus--ref-circle-list))
    (duo-ref-rotate-right (torus--ref-circle-list))
    (torus--torus-status))
  torus-cur-circle)

;;;###autoload
(defun torus-rotate-circle-left ()
  "Rotate locations of the current circle to the left (backward)."
  (interactive)
  (if (torus--empty-circle-p)
      (message torus--msg-empty-circle (torus--circle-name) (torus--torus-name))
    (torus--update-position)
    (torus--decrease-index (torus--ref-location-list))
    (duo-ref-rotate-left (torus--ref-location-list))
    (force-mode-line-update t))
  torus-cur-location)

;;;###autoload
(defun torus-rotate-circle-right ()
  "Rotate locations of the current circle to the right (forward)."
  (interactive)
  (if (torus--empty-circle-p)
      (message torus--msg-empty-circle (torus--circle-name) (torus--torus-name))
    (torus--update-position)
    (torus--increase-index (torus--ref-location-list))
    (duo-ref-rotate-right (torus--ref-location-list))
    (force-mode-line-update t))
  torus-cur-location)

;;; Split
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-split-horizontally ()
  "Split horizontally to view all buffers in current circle.
Split until `torus-maximum-horizontal-split' is reached."
  (interactive)
  (let ((numsplit (1- (torus--circle-length))))
    (when (> torus-verbosity 1)
      (message "numsplit = %d" numsplit))
    (if (> numsplit (1- torus-maximum-horizontal-split))
        (message "Too many files to split.")
      (delete-other-windows)
      (dotimes (iter numsplit)
        (when (> torus-verbosity 2)
          (message "iter = %d" iter))
        (split-window-below)
        (balance-windows)
        (other-window 1)
        (torus-next-location))
      (other-window 1)
      (torus-next-location))))

;;;###autoload
(defun torus-split-vertically ()
  "Split vertically to view all buffers in current circle.
Split until `torus-maximum-vertical-split' is reached."
  (interactive)
  (let ((numsplit (1- (torus--circle-length))))
    (when (> torus-verbosity 1)
      (message "numsplit = %d" numsplit))
    (if (> numsplit (1- torus-maximum-horizontal-split))
        (message "Too many files to split.")
      (delete-other-windows)
      (dotimes (iter numsplit)
        (when (> torus-verbosity 2)
          (message "iter = %d" iter))
        (split-window-right)
        (balance-windows)
        (other-window 1)
        (torus-next-location))
      (other-window 1)
      (torus-next-location))))

;;;###autoload
(defun torus-split-main-left ()
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
      (torus-next-location)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> torus-verbosity 2)
          (message "iter = %d" iter))
        (split-window-below)
        (balance-windows)
        (other-window 1)
        (torus-next-location))
      (other-window 1)
      (torus-next-location))))

;;;###autoload
(defun torus-split-main-right ()
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
      (torus-next-location)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> torus-verbosity 2)
          (message "iter = %d" iter))
        (split-window-below)
        (balance-windows)
        (other-window 1)
        (torus-next-location))
      (other-window 1)
      (torus-next-location))))

;;;###autoload
(defun torus-split-main-top ()
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
      (torus-next-location)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> torus-verbosity 2)
          (message "iter = %d" iter))
        (split-window-right)
        (balance-windows)
        (other-window 1)
        (torus-next-location))
      (other-window 1)
      (torus-next-location))))

;;;###autoload
(defun torus-split-main-bottom ()
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
      (torus-next-location)
      (dolist (iter (number-sequence 1 numsplit))
        (when (> torus-verbosity 2)
          (message "iter = %d" iter))
        (split-window-right)
        (balance-windows)
        (other-window 1)
        (torus-next-location))
      (other-window 1)
      (torus-next-location))))

;;;###autoload
(defun torus-split-grid ()
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
            (torus-next-location)))
        (when (< total max-iter)
          (other-window 1)
          (torus-next-location)))
    (other-window 1)
    (torus-next-location))))

;;; ============================================================
;;; From here, it’s a mess
;;; ============================================================

;;; Modifications
;;; ------------------------------------------------------------

(defun torus--prefix-circles (prefix torus-name)
  "Return vars of TORUS-NAME with PREFIX to the circle names."
  (unless (and (stringp prefix) (stringp torus-name))
    (error "In torus--prefix-circles : wrong type argument"))
  (let* ((entry (cdr (assoc torus-name torus-wheel)))
         (torus (copy-tree (cdr (assoc "torus" entry))))
         (history (copy-tree (cdr (assoc "history" entry)))))
    (if (> (length prefix) 0)
        (progn
          (message "Prefix is %s" prefix)
          (dolist (elem torus)
            (setcar elem
                    (concat prefix torus-prefix-separator (car elem))))
          (dolist (elem history)
            (setcdr elem
                    (concat prefix torus-prefix-separator (cdr elem)))))
      (message "Prefix is blank"))
    (list torus history)))

;;; Add
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-add-copy-of-torus (torus-name)
  "Create a new torus named TORUS-NAME as copy of the current torus."
  (interactive
   (list (read-string "Name of the new torus : "
                      nil
                      'torus-user-input-history)))
  (setq torus-cur-torus (copy-tree torus-cur-torus))
  (if torus-cur-torus
      (setcar torus-cur-torus torus-name)
    (setq torus-cur-torus (list torus-name)))
  (if torus-wheel
      (push torus-cur-torus torus-wheel)
    (setq torus-wheel (list torus-cur-torus))))

;;;###autoload
(defun torus-search-meta-history (location-name)
  "Search LOCATION-NAME in `torus-history'."
  (interactive
   (list
    (completing-read
     "Search location in history : "
     (mapcar #'torus--concise torus-history) nil t)))
  (torus--prefix-argument-split current-prefix-arg)
  (when torus-history
    (let* ((index (cl-position location-name torus-history
                            :test #'torus--equal-concise-p))
           (before (cl-subseq torus-history 0 index))
           (element (nth index torus-history))
           (after (cl-subseq torus-history (1+ index))))
      (setq torus-history (append (list element) before after)))
    (torus--meta-switch (car torus-history))))

;;; Move
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-move-location-to-circle (circle-name)
  "Move current location to CIRCLE-NAME."
  (interactive
   (list (completing-read
          "Move current location to circle : "
          (mapcar #'car torus-cur-torus) nil t)))
  (torus--update-position)
  (let* ((location (car (cdr (car torus-cur-torus))))
         (circle (cdr (assoc circle-name torus-cur-torus)))
         (old-name (car (car torus-cur-torus)))
         (old-pair (cons location old-name)))
    (if (member location circle)
        (message "Location %s already exists in circle %s."
                 (torus--concise location)
                 circle-name)
      (message "Moving location %s to circle %s."
               (torus--concise location)
               circle-name)
      (pop (cdar torus-cur-torus))
      (setcdr (assoc circle-name torus-cur-torus)
              (push location circle))
      (dolist (location-circle torus-helix)
        (when (equal location-circle old-pair)
          (setcdr location-circle circle-name)))
      (dolist (location-circle torus-history)
        (when (equal location-circle old-pair)
          (setcdr location-circle circle-name)))
      (torus--jump))))


;;;###autoload
(defun torus-move-circle-to-torus (torus-name)
  "Move current circle to TORUS-NAME."
  (interactive
   (list (completing-read
          "Move current circle to torus : "
          (mapcar #'car torus-wheel) nil t)))
  (torus--update-position)
  (let* ((circle (cl-copy-seq (car torus-cur-torus)))
         (torus (copy-tree
                 (cdr (assoc "torus" (assoc torus-name torus-wheel)))))
         (circle-name (car circle))
         (circle-torus (cons circle-name (caar torus-wheel))))
    (if (member circle torus)
        (message "Circle %s already exists in torus %s."
                 circle-name
                 torus-name)
      (message "Moving circle %s to torus %s."
               circle-name
               torus-name)
      (when (> torus-verbosity 2)
        (message "circle-torus %s" circle-torus))
      (setcdr (assoc "torus" (assoc torus-name torus-wheel))
              (push circle torus))
      (setq torus-cur-torus (torus--assoc-delete-all circle-name torus-cur-torus))
      (setq torus-helix
            (torus--reverse-assoc-delete-all circle-name torus-helix))
      (setq torus-history
            (torus--reverse-assoc-delete-all circle-name torus-history))
      (setq torus-markers
            (torus--reverse-assoc-delete-all circle-name torus-markers))
      (setq torus-helix
            (torus--reverse-assoc-delete-all circle-torus torus-helix))
      (setq torus-history
            (torus--reverse-assoc-delete-all circle-torus torus-history))
      (torus--build-table)
      (setq torus-helix (torus--build-helix))
      (torus--jump))))

;;;###autoload
(defun torus-copy-location-to-circle (circle-name)
  "Copy current location to CIRCLE-NAME."
  (interactive
   (list (completing-read
          "Copy current location to circle : "
          (mapcar #'car torus-cur-torus) nil t)))
  (torus--update-position)
  (let* ((location (car (cdr (car torus-cur-torus))))
         (circle (cdr (assoc circle-name torus-cur-torus))))
    (if (member location circle)
        (message "Location %s already exists in circle %s."
                 (torus--concise location)
                 circle-name)
      (message "Copying location %s to circle %s."
                 (torus--concise location)
                 circle-name)
      (setcdr (assoc circle-name torus-cur-torus) (push location circle))
      (torus--build-table)
      (setq torus-helix (torus--build-helix)))))

;;;###autoload
(defun torus-copy-circle-to-torus (torus-name)
  "Copy current circle to TORUS-NAME."
  (interactive
   (list (completing-read
          "Copy current circle to torus : "
          (mapcar #'car torus-wheel) nil t)))
  (torus--update-position)
  (let* ((circle (cl-copy-seq (car torus-cur-torus)))
         (torus (copy-tree
                 (cdr (assoc "torus" (assoc torus-name torus-wheel))))))
    (if (member circle torus)
        (message "Circle %s already exists in torus %s."
                 (car circle)
                 torus-name)
      (message "Copying circle %s to torus %s."
               (car circle)
               torus-name)
      (setcdr (assoc "torus" (assoc torus-name torus-wheel))
              (push circle torus)))
    (torus--build-table)
    (setq torus-helix (torus--build-helix))))

;;; Join
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-prefix-circles-of-current-torus (prefix)
  "Add PREFIX to circle names of `torus-cur-torus'."
  (interactive
   (list
    (read-string (format torus--msg-prefix-circle
                         (car (car torus-wheel)))
                 nil
                 'torus-user-input-history)))
  (let ((varlist))
    (setq varlist (torus--prefix-circles prefix (car (car torus-wheel))))
    (setq torus-cur-torus (car varlist))
    (setq torus-history (car (cdr varlist))))
  (torus--build-table)
  (setq torus-helix (torus--build-helix)))

;;;###autoload
(defun torus-join-circles (circle-name)
  "Join current circle with CIRCLE-NAME."
  (interactive
   (list
    (completing-read "Join current circle with circle : "
                     (mapcar #'car torus-cur-torus) nil t)))
  (let* ((current-name (car (car torus-cur-torus)))
         (join-name (concat current-name torus-join-separator circle-name))
         (user-choice
          (read-string (format "Name of the joined torus [%s] : " join-name))))
    (when (> (length user-choice) 0)
      (setq join-name user-choice))
    (torus-add-circle join-name)
    (setcdr (car torus-cur-torus)
            (append (cdr (assoc current-name torus-cur-torus))
                    (cdr (assoc circle-name torus-cur-torus))))
    (delete-dups (cdr (car torus-cur-torus))))
  (torus--update-meta)
  (torus--build-table)
  (setq torus-helix (torus--build-helix))
  (torus--jump))

;;;###autoload
(defun torus-join-toruses (torus-name)
  "Join current torus with TORUS-NAME in `torus-wheel'."
  (interactive
   (list
    (completing-read "Join current torus with torus : "
                     (mapcar #'car torus-wheel) nil t)))
  (torus--prefix-argument-split current-prefix-arg)
  (torus--update-meta)
  (let* ((current-name (car (car torus-wheel)))
         (join-name (concat current-name torus-join-separator torus-name))
         (user-choice
          (read-string (format "Name of the joined torus [%s] : " join-name)))
         (prompt-current
          (format torus--msg-prefix-circle current-name))
         (prompt-added
          (format torus--msg-prefix-circle torus-name))
         (prefix-current
          (read-string prompt-current nil 'torus-user-input-history))
         (prefix-added
          (read-string prompt-added nil 'torus-user-input-history))
         (varlist)
         (torus-added)
         (history-added)
         (input-added))
    (when (> (length user-choice) 0)
      (setq join-name user-choice))
    (torus--update-input-history prefix-current)
    (torus--update-input-history prefix-added)
    (torus-add-copy-of-torus join-name)
    (torus-prefix-circles-of-current-torus prefix-current)
    (setq varlist (torus--prefix-circles prefix-added torus-name))
    (setq torus-added (car varlist))
    (setq history-added (car (cdr varlist)))
    (setq input-added (car (cdr (cdr varlist))))
    (if (seq-intersection torus-cur-torus torus-added #'torus--equal-car-p)
        (message torus--msg-circle-name-collision)
      (setq torus-cur-torus (append torus-cur-torus torus-added))
      (setq torus-history (append torus-history history-added))
      (setq torus-user-input-history (append torus-user-input-history input-added))))
  (torus--update-meta)
  (torus--build-table)
  (setq torus-helix (torus--build-helix))
  (torus--jump))

;;; Autogroup
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-autogroup (quoted-function)
  "Autogroup all torus locations according to the values of QUOTED-FUNCTION.
A new torus is created on `torus-wheel' to contain the new circles.
The function must return the names of the new circles as strings."
  (interactive)
  (let ((torus-name
         (read-string "Name of the autogroup torus : "
                      nil
                      'torus-user-input-history))
        (all-locations))
    (if (assoc torus-name torus-wheel)
        (message "torus %s already exists in torus-wheel" torus-name)
      (torus-add-copy-of-torus torus-name)
      (dolist (circle torus-cur-torus)
        (dolist (location (cdr circle))
          (push location all-locations)))
      (setq torus-cur-torus (seq-group-by quoted-function all-locations))))
  (setq torus-history nil)
  (setq torus-markers nil)
  (setq torus-user-input-history nil)
  (torus--build-table)
  (setq torus-helix (torus--build-helix))
  (torus--update-meta)
  (torus--jump))

;;;###autoload
(defun torus-autogroup-by-path ()
  "Autogroup all location of the torus by directories.
A new torus is created to contain the new circles."
  (interactive)
  (torus-autogroup (lambda (elem) (directory-file-name (file-name-directory (car elem))))))

;;;###autoload
(defun torus-autogroup-by-directory ()
  "Autogroup all location of the torus by directories.
A new torus is created to contain the new circles."
  (interactive)
  (torus-autogroup #'torus--directory))

;;;###autoload
(defun torus-autogroup-by-extension ()
  "Autogroup all location of the torus by extension.
A new torus is created to contain the new circles."
  (interactive)
  (torus-autogroup #'torus--extension-description))

;;;###autoload
(defun torus-autogroup-by-git-repo ()
  "Autogroup all location of the torus by git repositories.
A new torus is created to contain the new circles."
  ;; TODO
  )

;;;###autoload
(defun torus-autogroup-menu (choice)
  "Autogroup according to CHOICE."
  (interactive
   (list (read-key torus--msg-autogroup-menu)))
    (pcase choice
      (?p (funcall 'torus-autogroup-by-path))
      (?d (funcall 'torus-autogroup-by-directory))
      (?e (funcall 'torus-autogroup-by-extension))
      (?\a (message "Autogroup cancelled by Ctrl-G."))
      (_ (message "Invalid key."))))

;;; Batch
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-run-elisp-code-on-circle (elisp-code)
  "Run ELISP-CODE to all files of the circle."
  (interactive (list (read-string
                      "Elisp code to run to all files of the circle : ")))
  (dolist (iter (number-sequence 1 (length (cdar torus-cur-torus))))
    (when (> torus-verbosity 1)
      (message "%d. Applying %s to %s" iter elisp-code (cadar torus-cur-torus))
      (message "Evaluated : %s"
               (car (read-from-string (format "(progn %s)" elisp-code)))))
    (torus--eval-string elisp-code)
    (torus-next-location)))

;;;###autoload
(defun torus-run-elisp-command-on-circle (command)
  "Run an Emacs Lisp COMMAND to all files of the circle."
  (interactive (list (read-command
                      "Elisp command to run to all files of the circle : ")))
  (dolist (iter (number-sequence 1 (length (cdar torus-cur-torus))))
    (when (> torus-verbosity 1)
      (message "%d. Applying %s to %s" iter command (cadar torus-cur-torus)))
    (funcall command)
    (torus-next-location)))

;;;###autoload
(defun torus-run-shell-command-on-circle (command)
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
      (torus-next-location))
    (setq shell-command-dont-erase-buffer keep-value)))

;;;###autoload
(defun torus-run-async-shell-command-on-circle (command)
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
      (torus-next-location))
    (setq async-shell-command-buffer keep-value)))

;;;###autoload
(defun torus-batch-menu (choice)
  "Split according to CHOICE."
  (interactive
   (list (read-key torus--msg-batch-menu)))
  (pcase choice
    (?e (call-interactively 'torus-run-elisp-code-on-circle))
    (?c (call-interactively 'torus-run-elisp-command-on-circle))
    (?! (call-interactively 'torus-run-shell-command-on-circle))
    (?& (call-interactively 'torus-run-async-shell-command-on-circle))
    (?\a (message "Batch operation cancelled by Ctrl-G."))
    (_ (message "Invalid key."))))

;;; Edit
;;; ------------------------------------------------------------

;;;###autoload
(defun torus-edit (filename)
  "Edit torus file FILENAME in the torus files dir.
Be sure to understand what you’re doing, and not leave some variables
in inconsistent state, or you might encounter strange undesired effects."
  (interactive
   (list
    (read-file-name
     "torus file : "
     (file-name-as-directory torus-dirname))))
  (find-file filename))

;;; End
;;; ----------------------------------------------------------------------

(provide 'torus)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; torus.el ends here
