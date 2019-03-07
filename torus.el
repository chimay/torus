;;; torus.el --- A buffer groups manager             -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Chimay

;; Author : Chimay
;; Name: Torus
;; Package-Version: 1.7
;; Package-requires: ((emacs "26"))
;; Keywords: files, buffers, group, persistent, history, layout
;; URL: https://github.com/chimay/torus

;;; Commentary:

;; If you ever dreamed about creating and switching buffer groups at will
;; in Emacs, Torus is the tool you want.
;;
;; Note that :
;;
;;   - An location is a pair (buffer (or filename) . position)
;;   - A buffer group, in fact an location group, is called a circle
;;   - The set of all buffer groups is called the torus (a circle of circles)
;;
;; In short, this plugin let you organize your buffers by creating as
;; many buffer groups as you need, add the files you want to it and
;; quickly navigate between :
;;
;;   - Buffers of the same group
;;   - Buffer groups (circles)
;;
;; Original idea by Stefan Kamphausen, see https://www.skamphausen.de/cgi-bin/ska/mtorus
;;
;; See https://github.com/chimay/torus/blob/master/README.org for more details

;;; License
;;; ------------------------------

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
;;; ------------------------------

;; Stefan Kamphausen, https://www.skamphausen.de/cgi-bin/ska/mtorus
;; Sebastian Freundt, https://sourceforge.net/projects/mtorus.berlios/

;;; Code:
;;; ------------------------------------------------------------

;;; Requires
;;; ------------------------------

(require 'cl-lib)

(declare-function subseq "cl-lib")
(declare-function copy-seq "cl-lib")
(declare-function position "cl-lib")
(declare-function find "cl-lib")

(require 'seq)

(declare-function seq-intersection "seq")
(declare-function seq-group-by "seq")

;;; Custom
;;; ------------------------------

(defgroup torus nil
  "An interface to navigating groups of buffers."
  :tag "Torus"
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
  "Whether to activate optional keybindings."
  :type 'integer
  :group 'torus)

(defcustom torus-verbosity 1
  "Level of verbosity.
1 = normal
2 = light debug
3 = heavy debug."
  :type 'integer
  :group 'torus)

(defcustom torus-dirname user-emacs-directory
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

(defcustom torus-autoread-file nil
  "The file to load on startup when `torus-load-on-startup' is t."
  :type 'string
  :group 'torus)

(defcustom torus-autowrite-file nil
  "The file to write before quitting Emacs when `torus-save-on-exit' is t."
  :type 'string
  :group 'torus)

(defcustom torus-history-maximum-elements 30
  "Maximum number of elements in `torus-history'."
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

(defcustom torus-prefix-separator " : "
  "String between the prefix and the circle names.
The name of the new circles will be of the form :
\"User_input_prefix `torus-prefix-separator' Name_of_the_added_circle\"
without the spaces. If the user enter a blank prefix,
the added circle names remain untouched."
  :type 'string
  :group 'torus)

(defcustom torus-join-separator " - "
  "String between the names when joining.
The name of the new object will be of the form :
\"Object-1 `torus-join-separator' Object-2\"
without the spaces."
  :type 'string
  :group 'torus)

;;; Variables
;;; ------------------------------

(defvar torus-meta nil
  "List of existing toruses.
You can create new torus with `torus-add-torus`.
Some functions also create a new torus to work with.")

(defvar torus-torus nil
  "The torus is a list of circles.
A circle is a list of locations, stored in the form :
\(\"circle name\" locations)
A location is a pair (file . position)
Most recent entries are in the beginning of the lists.")

(defvar torus-index nil
  "Alist giving circles corresponding to torus locations.
Each element has the form :
\((file . position) . circle)
Allow to search among all files of the torus.")

(defvar torus-history nil
  "Alist containing the history of locations in the torus.
Each element is of the form :
\((file . position) . circle)
Same format as in `torus-index'.")

(defvar torus-layout nil
  "Alist containing split layout of circles.
Each element is of the form:
\(circle . layout)")

(defvar torus-markers nil
  "Alist containing markers to opened files.
Each element is of the form :
\((file . position) . marker)
Contain only the files opened in buffers.")

(defvar torus-input-history nil
  "History of user input.")

;;; Extensions
;;; ------------

(defvar torus-extension ".el"
  "Extension for torus files.")

;;; Prompts
;;; ------------

(defvar torus--message-write-choice
  "Write [a] all (default) [m] meta [t] torus \n\
      [i] index [h] history [l] layout [n] input history")

(defvar torus--message-reset-choice
  "Reset [a] all [m] meta [t] torus \n\
      [i] index [h] history [l] layout [C-m] markers [n] input history")

(defvar torus--message-print-choice
  "Print [a] all [m] meta [t] torus \n\
      [i] index [h] history [l] layout [C-m] markers [n] input history")

(defvar torus--message-autogroup-choice
  "Autogroup by [p] path [d] directory [e] extension")

(defvar torus--message-layout-choice
  "Layout [m] manual [o] one window [h] horizontal [v] vertical [g] grid \n\
       main window on [l] left [r] right [t] top [b] bottom")

(defvar torus--message-file-does-not-exist
  "File %s does not exist anymore. It will be removed from the torus.")

(defvar torus--message-empty-circle
  "No location in circle %s. You can use torus-add-location to fill the circle.")

(defvar torus--message-empty-torus
  "Torus is empty. You can use torus-add-circle to add a circle to it.")

(defvar torus--message-existent-location
  "Location %s already exists in circle %s")

(defvar torus--message-prefix-circle
  "Prefix for the circle of torus %s (leave blank for none) ? ")

(defvar torus--message-circle-name-collision
  "Circle name collision. Please add/adjust prefixes to avoid confusion.")

(defvar torus--message-replace-torus
  "This will replace the current torus variables. Continue ? ")

;;; Keymap with prefix
;;; ------------------------------

(defvar torus-map)

(define-prefix-command 'torus-map)

;;; Toolbox
;;; ------------------------------

(defun torus--equal-car-p (one two)
  "Whether the cars of ONE and TWO are equal."
  (equal (car one) (car two)))

(defun torus--assoc-delete-all (key alist)
  "Remove all elements with key matching KEY in ALIST."
  (cl-remove key alist :test 'equal :key 'car))

(when (fboundp 'assoc-delete-all)
  (defalias 'torus--assoc-delete-all 'assoc-delete-all))

(defun torus--reverse-assoc-delete-all (value alist)
  "Remove all elements with value matching VALUE in ALIST."
  (cl-remove value alist :test 'equal :key 'cdr))

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
    (when (> torus-verbosity 1)
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
;;; ------------------------------

;;; Strings
;;; ------------

(defun torus--buffer-or-filename (location)
  "Return buffer name of LOCATION if existent in `torus-markers', file basename otherwise."
  (unless (consp location)
    (error "torus--buffer-or-filename : wrong type argument."))
  (let* ((bookmark (cdr (assoc location torus-markers)))
         (buffer (when bookmark
                   (marker-buffer bookmark))))
    (if buffer
        (buffer-name buffer)
      (file-name-nondirectory (car location)))))

(defun torus--concise (object)
  "Return OBJECT in concise string format.
If OBJECT is a string : simply returns OBJECT.
If OBJECT is \(File . Position) : returns \"File at Position.\"
If OBJECT is \((File . Position) . Circle) : returns
\"Circle > File at Position.\""
  (if (stringp object)
      object
    (when (consp object)
      (if (consp (car object))
          (let* ((location (car object))
                 (file (torus--buffer-or-filename location))
                 (position (prin1-to-string (cdr location)))
                 (circle (cdr object)))
            (concat circle " > " file " at " position))
        (let ((file (torus--buffer-or-filename object))
              (position (prin1-to-string (cdr object))))
          (concat file " at " position))))))

(defun torus--short (location)
  "Return LOCATION in short string format.
Shorter than concise. Useful for tab like messages."
  (unless (consp location)
    (error "torus--short : wrong type argument."))
  (if (equal location (cadar torus-torus))
      (concat "["
              (torus--buffer-or-filename location)
              ":"
              (prin1-to-string (line-number-at-pos (cdr location)))
              "]")
    (concat (torus--buffer-or-filename location)
            ":"
            (prin1-to-string (line-number-at-pos (cdr location))))))

(defun torus--prefix-circles (prefix torus-name)
  "Return vars of TORUS-NAME with PREFIX to the circle names."
  (unless (and (stringp prefix) (stringp torus-name))
    (error "torus--prefix-circles : wrong type argument."))
  (let* ((entry (cdr (assoc torus-name torus-meta)))
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

;;; Predicates
;;; ------------

(defun torus--equal-concise-p (one two)
  "Whether the concise representations of ONE and TWO are equal."
  (equal (torus--concise one)
         (torus--concise two)))

(defun torus--inside-p ()
  "Whether the current location belongs to the torus."
  (if (and (car torus-torus) (> (length (car torus-torus)) 1))
      (let* ((location (car (cdr (car torus-torus)))))
        (equal (car location) (buffer-file-name (current-buffer))))))

;;; Updates
;;; ------------

(defun torus--update-position ()
  "Update position in current location.
Do nothing if file does not match current buffer."
  (unless (and torus-torus (listp torus-torus))
    (error "torus--update-history : bad torus."))
  (let ((circle (car torus-torus)))
    (unless (and circle (listp circle) (> (length circle) 1))
      (error "torus--update-history : bad circle.")))
  (let* ((here (point))
         (marker (point-marker))
         (old-location (car (cdr (car torus-torus))))
         (old-here (cdr old-location))
         (file (car old-location))
         (new-location)
         (new-location-marker))
    (when (and (equal file (buffer-file-name (current-buffer)))
               (not (equal here old-here)))
      (setq new-location (cons file here))
      (setq new-location-marker (cons new-location marker))
      (when (> torus-verbosity 2)
        (message "Old location : %s" old-location)
        (message "New location : %s" new-location))
      (setcar (cdr (car torus-torus)) new-location)
      (when (assoc old-location torus-index)
        (setcar (assoc old-location torus-index) new-location))
      (when (assoc old-location torus-history)
        (setcar (assoc old-location torus-history) new-location))
      (if (assoc old-location torus-markers)
          (progn
            (setcdr (assoc old-location torus-markers) marker)
            (setcar (assoc old-location torus-markers) new-location))
        (push new-location-marker torus-markers)))))

(defun torus--update-history ()
  "Add current location to `torus-history'."
  (unless (and torus-torus (listp torus-torus))
    (error "torus--update-history : bad torus."))
  (let ((circle (car torus-torus)))
    (unless (and circle (listp circle) (> (length circle) 1))
      (error "torus--update-history : bad circle.")))
  (let* ((circle (car torus-torus))
         (location (car (cdr circle)))
         (location-circle (cons location (car circle))))
    (push location-circle torus-history)
    (delete-dups torus-history)
    (setq torus-history
          (subseq torus-history 0
                  (min (length torus-history)
                       torus-history-maximum-elements)))))

(defun torus--update-layout ()
  "Fill `torus-layout' from missing elements. Delete useless ones."
  (let ((circles (mapcar #'car torus-torus)))
    (dolist (elem circles)
      (unless (assoc elem torus-layout)
        (push (cons elem ?m) torus-layout)))
    (dolist (elem torus-layout)
      (unless (member (car elem) circles)
        (setq torus-layout (torus--assoc-delete-all (car elem) torus-layout))))
    (setq torus-layout (reverse torus-layout))))

(defun torus--apply-or-fill-layout ()
  "Apply layout of current circle, or add default is not present."
  (let ((circle-name (caar torus-torus)))
    (if (consp (assoc circle-name torus-layout))
        (torus-layout-menu (cdr (assoc (caar torus-torus) torus-layout)))
      (push (cons circle-name ?m) torus-layout))))

(defun torus--update-input-history (name)
  "Add NAME to `torus-input-history' if not already there."
  ;; (delete-dups torus-input-history)
  (unless (or (= (length name) 0) (member name torus-input-history))
    (push name torus-input-history)))

(defun torus--update-meta ()
  "Update current torus in `torus-meta'."
  (torus--update-position)
  (when torus-meta
    (let ((entry (cdar torus-meta)))
      (if (equal '("torus" "history" "layout" "input history")
                 (mapcar 'car entry))
          (progn
            (if (assoc "input history" entry)
                (setcdr (assoc "input history" (cdar torus-meta)) (copy-seq torus-input-history))
              (push (cons "input history" torus-input-history) (cdar torus-meta)))
            (if (assoc "layout" entry)
                (setcdr (assoc "layout" (cdar torus-meta)) (copy-tree torus-layout))
              (push (cons "layout" torus-layout) (cdar torus-meta)))
            (if (assoc "history" entry)
                (setcdr (assoc "history" (cdar torus-meta)) (copy-tree torus-history))
              (push (cons "history" torus-history) (cdar torus-meta)))
            (if (assoc "torus" entry)
                (setcdr (assoc "torus" (cdar torus-meta)) (copy-tree torus-torus))
              (push (cons "torus" torus-torus) (cdar torus-meta))))
        ;; Reordering if needed
        (push (cons "input history" torus-input-history) (cdar torus-meta))
        (push (cons "layout" torus-layout) (cdar torus-meta))
        (push (cons "history" torus-history) (cdar torus-meta))
        (push (cons "torus" torus-torus) (cdar torus-meta))
        (setf (cdar torus-meta) (subseq (cdar torus-meta) 0 4))))))

(defun torus--update-from-meta ()
  "Update main torus variables from `torus-meta'."
  (unless (and torus-meta (listp torus-meta) (listp (car torus-meta)))
    (error "torus--update-from-meta : bad meta torus."))
  (let ((entry (cdr (car torus-meta))))
    (if (assoc "torus" entry)
        (setq torus-torus (copy-tree (cdr (assoc "torus" entry))))
      (setq torus-torus nil))
    (if (assoc "history" entry)
        (setq torus-history (copy-tree (cdr (assoc "history" entry))))
      (setq torus-history nil))
    (if (assoc "layout" entry)
        (setq torus-layout (copy-tree (cdr (assoc "layout" entry))))
      (setq torus-layout nil))
    (if (assoc "input history" entry)
        (setq torus-input-history (copy-seq (cdr (assoc "input history" entry))))
      (setq torus-input-history nil))))

(defun torus--jump ()
  "Jump to current location (buffer & position) in torus.
Add the location to `torus-markers' if not already present."
  (unless (and torus-torus (listp torus-torus))
    (error "torus--jump : bad torus."))
  (let ((circle (car torus-torus)))
    (unless (and circle (listp circle) (> (length circle) 1))
      (error "torus--jump : bad circle.")))
  (let* ((location (car (cdr (car torus-torus))))
         (file (car location))
         (position (cdr location))
         (bookmark (cdr (assoc location torus-markers)))
         (buffer (when bookmark
                   (marker-buffer bookmark))))
    (if (and bookmark buffer (buffer-live-p buffer))
        (progn
          (when (> torus-verbosity 1)
            (message "Found %s in markers" bookmark))
          (when (not (equal buffer (current-buffer)))
            (switch-to-buffer buffer))
          (goto-char bookmark))
      (when (> torus-verbosity 1)
        (message "Found %s in torus" location))
      (when bookmark
        (setq torus-markers (torus--assoc-delete-all location torus-markers)))
      (if (file-exists-p file)
          (progn
            (when (> torus-verbosity 1)
              (message "Opening file %s at %s" file position))
            (find-file file)
            (goto-char position)
            (push (cons location (point-marker)) torus-markers))
        (message (format torus--message-file-does-not-exist file))
        (setcdr (car torus-torus) (delete location (cdr (car torus-torus))))
        (setq torus-markers (torus--assoc-delete-all location torus-markers))
        (setq torus-index (torus--assoc-delete-all location torus-index))
        (setq torus-history (torus--assoc-delete-all location torus-history))))
    (torus--update-history)
    (torus-info)))

;;; Build
;;; ------------

(defun torus--build-index ()
  "Build `torus-index'."
  (setq torus-index nil)
  (dolist (circle torus-torus)
    (dolist (location (cdr circle))
      (let ((location-circle (cons location (car circle))))
        (unless (member location-circle torus-index)
          (push location-circle torus-index)))))
  (setq torus-index (reverse torus-index)))

;;; Switch
;;; ------------

(defun torus--switch (location-circle)
  "Jump to circle and location countained in LOCATION-CIRCLE."
  (unless (and location-circle
               (consp location-circle)
               (consp (car location-circle)))
    (error "torus--switch : wrong type argument.."))
  (torus--update-position)
  (let* ((circle-name (cdr location-circle))
         (circle (assoc circle-name torus-torus))
         (index (position circle torus-torus :test #'equal))
         (before (subseq torus-torus 0 index))
         (after (subseq torus-torus index)))
    (if index
        (setq torus-torus (append after before))
      (message "Circle not found.")))
  (let* ((circle (cdr (car torus-torus)))
         (location (car location-circle))
         (index (position location circle :test #'equal))
         (before (subseq circle 0 index))
         (after (subseq circle index)))
    (if index
        (setcdr (car torus-torus) (append after before))
      (message "Location not found.")))
  (torus--jump)
  (torus--apply-or-fill-layout))

;;; Splits
;;; ------------

(defun torus--prefix-argument (prefix)
  "Handle prefix argument PREFIX. Used to split."
  (pcase prefix
   ('(4)
    (split-window-below)
    (other-window 1))
   ('(16)
    (split-window-right)
    (other-window 1))))

;;; Hooks & Advices
;;; ------------------------------

;;;###autoload
(defun torus-quit ()
  "Write torus before quit."
  (when torus-save-on-exit
    (if torus-autowrite-file
        (torus-write torus-autowrite-file)
      (when (y-or-n-p "Write torus ? ")
        (call-interactively 'torus-write))))
  ;; To be sure they will be nil at startup, even if some plugin saved
  ;; global variables
  (torus-reset-menu ?a))

;;;###autoload
(defun torus-start ()
  "Read torus on startup."
  (when torus-load-on-startup
    (if torus-autoread-file
        (torus-read torus-autoread-file)
      (message "Set torus-autoread-file if you want to load it."))))

;;;###autoload
(defun torus-advice-switch-buffer (&rest args)
  "Advice to `switch-to-buffer'. ARGS are irrelevant."
  (when (> torus-verbosity 2)
    (message "Advice called with args %s" args))
  (when (and torus-torus (torus--inside-p))
    (torus--update-position)))

;;; Commands
;;; ------------------------------

;;;###autoload
(defun torus-init ()
  "Initialize torus.
Create directory if needed.
Add hooks.
Add advices."
  (interactive)
  (unless (file-exists-p torus-dirname)
    (make-directory torus-dirname))
  (add-hook 'emacs-startup-hook 'torus-start)
  (add-hook 'kill-emacs-hook 'torus-quit)
  (advice-add #'switch-to-buffer :before #'torus-advice-switch-buffer))

;;;###autoload
(defun torus-install-default-bindings ()
  "Install default keybindings."
  (interactive)
  (if (stringp torus-prefix-key)
      (global-set-key (kbd torus-prefix-key) 'torus-map)
    (global-set-key torus-prefix-key 'torus-map))
  (when (>= torus-binding-level 0)
    (define-key torus-map (kbd "i") 'torus-info)
    (define-key torus-map (kbd "c") 'torus-add-circle)
    (define-key torus-map (kbd "l") 'torus-add-location)
    (define-key torus-map (kbd "f") 'torus-add-file)
    (define-key torus-map (kbd "+") 'torus-add-torus)
    (define-key torus-map (kbd "<left>") 'torus-previous-circle)
    (define-key torus-map (kbd "<right>") 'torus-next-circle)
    (define-key torus-map (kbd "<up>") 'torus-previous-location)
    (define-key torus-map (kbd "<down>") 'torus-next-location)
    (define-key torus-map (kbd "SPC") 'torus-switch-circle)
    (define-key torus-map (kbd "=") 'torus-switch-location)
    (define-key torus-map (kbd "@") 'torus-switch-torus)
    (define-key torus-map (kbd "s") 'torus-search)
    (define-key torus-map (kbd "d") 'torus-delete-location)
    (define-key torus-map (kbd "D") 'torus-delete-circle)
    (define-key torus-map (kbd "-") 'torus-delete-torus)
    (define-key torus-map (kbd "r") 'torus-read)
    (define-key torus-map (kbd "w") 'torus-write))
  (when (>= torus-binding-level 1)
    (define-key torus-map (kbd "<next>") 'torus-history-older)
    (define-key torus-map (kbd "<prior>") 'torus-history-newer)
    (define-key torus-map (kbd "h") 'torus-search-history)
    (define-key torus-map (kbd "^") 'torus-alternate)
    (define-key torus-map (kbd "<") 'torus-alternate-circles)
    (define-key torus-map (kbd ">") 'torus-alternate-in-same-circle)
    (define-key torus-map (kbd "n") 'torus-rename-circle)
    (define-key torus-map (kbd "N") 'torus-rename-torus)
    (define-key torus-map (kbd "m") 'torus-move-location)
    (define-key torus-map (kbd "M") 'torus-move-circle)
    (define-key torus-map (kbd "C-m") 'torus-move-to-circle)
    (define-key torus-map (kbd "M-m") 'torus-move-all-to-circle)
    (define-key torus-map (kbd "y") 'torus-copy-to-circle)
    (define-key torus-map (kbd "j") 'torus-join-circles)
    (define-key torus-map (kbd "J") 'torus-join-toruses)
    (define-key torus-map (kbd "#") 'torus-layout-menu))
  (when (>= torus-binding-level 2)
    (define-key torus-map (kbd "! l") 'torus-reverse-locations)
    (define-key torus-map (kbd "! c") 'torus-reverse-circles)
    (define-key torus-map (kbd "! d") 'torus-deep-reverse)
    (define-key torus-map (kbd ":") 'torus-prefix-circles-of-current-torus)
    (define-key torus-map (kbd "g") 'torus-autogroup-menu))
  (when (>= torus-binding-level 3)
    (define-key torus-map (kbd "p") 'torus-print-menu)
    (define-key torus-map (kbd "z") 'torus-reset-menu)
    (define-key torus-map (kbd "C-d") 'torus-delete-current-location)
    (define-key torus-map (kbd "M-d") 'torus-delete-current-circle)))

;;;###autoload
(defun torus-reset-menu (choice)
  "Reset CHOICE variables to nil."
  (interactive
   (list (read-key torus--message-reset-choice)))
  (let ((varlist))
    (pcase choice
      (?m (push 'torus-meta varlist))
      (?t (push 'torus-torus varlist))
      (?i (push 'torus-index varlist))
      (?h (push 'torus-history varlist))
      (?l (push 'torus-layout varlist))
      (?\^m (push 'torus-markers varlist))
      (?n (push torus-input-history varlist))
      (?a (setq varlist (list 'torus-meta
                              'torus-torus
                              'torus-index
                              'torus-history
                              'torus-layout
                              'torus-markers
                              'torus-input-history)))
      (?\a (message "Reset cancelled by Ctrl-G."))
      (_ (message "Invalid key.")))
    (dolist (var varlist)
      (when (> torus-verbosity 1)
        (message "%s -> nil" (symbol-name var)))
      (set var nil))))

;;; Printing
;;; ------------

;;;###autoload
(defun torus-info ()
  "Print local info : circle name and locations."
  (interactive)
  (let (pretty-list (string-join )))
  (if torus-torus
      (if (> (length (car torus-torus)) 1)
          (let* ((circle (car torus-torus))
                 (prettylist
                  (mapcar
                   (lambda (elem)
                     (cons
                      (torus--buffer-or-filename elem)
                      (cdr elem)))
                   (cdr circle))))
            (message "%s : %s" (car circle) prettylist))
        (message torus--message-empty-circle (car (car torus-torus))))
    (message torus--message-empty-torus)))

;;;###autoload
(defun torus-print-menu (choice)
  "Print CHOICE variables."
  (interactive
   (list (read-key torus--message-print-choice)))
  (let ((varlist)
        (window (view-echo-area-messages)))
    (pcase choice
      (?m (push 'torus-meta varlist))
      (?t (push 'torus-torus varlist))
      (?i (push 'torus-index varlist))
      (?h (push 'torus-history varlist))
      (?l (push 'torus-layout varlist))
      (?\^m (push 'torus-markers varlist))
      (?n (push 'torus-input-history varlist))
      (?a (setq varlist (list 'torus-meta
                              'torus-torus
                              'torus-index
                              'torus-history
                              'torus-layout
                              'torus-markers
                              'torus-input-history)))
      (?\a (delete-window window)
           (message "Print cancelled by Ctrl-G."))
      (_ (message "Invalid key.")))
    (dolist (var varlist)
      (message "%s" (symbol-name var))
      (pp (symbol-value var)))))

;;; Adding
;;; ------------

;;;###autoload
(defun torus-add-circle (circle-name)
  "Add a new circle CIRCLE-NAME to torus."
  (interactive
   (list
    (read-string "Name of the new circle : "
                 nil
                 'torus-input-history)))
  (unless (stringp circle-name)
    (error "torus-add-circle : wrong type argument"))
  (torus--update-input-history circle-name)
  (let ((torus-name (car (car torus-meta))))
    (if (assoc circle-name torus-torus)
        (message "Circle %s already exists in torus" circle-name)
      (message "Adding circle %s to torus %s" circle-name torus-name)
      (push (list circle-name) torus-torus)
      (push (cons circle-name ?m) torus-layout))))

;;;###autoload
(defun torus-add-location ()
  "Add current file and point to current circle."
  (interactive)
  (unless torus-torus
    (when (y-or-n-p "Torus is empty. Do you want to add a first circle ? ")
      (call-interactively 'torus-add-circle)))
  (if torus-torus
      (if (buffer-file-name)
          (let* ((circle (car torus-torus))
                 (pointmark (point-marker))
                 (location (cons (buffer-file-name) (marker-position pointmark)))
                 (location-marker (cons location pointmark))
                 (location-circle (cons location (car circle))))
            (if (member location (cdr circle))
                (message torus--message-existent-location
                         (torus--concise location) (car circle))
              (message "Adding %s to circle %s" location (car circle))
              (if (> (length circle) 1)
                  (setcdr circle (append (list location) (cdr circle)))
                (setf circle (append circle (list location))))
              (setf (car torus-torus) circle)
              (unless (member location-circle torus-index)
                (push location-circle torus-index))
              (torus--update-history)
              (unless (member location-marker torus-markers)
                (push location-marker torus-markers))
              (unless torus-meta
                (torus-add-torus "default"))))
        (message "Buffer must have a filename to be added to the torus."))
    (message "Torus is empty. Please add a circle first with torus-add-circle.")))

;;;###autoload
(defun torus-add-file (filename)
  "Add FILENAME to the current circle.
The location added will be (file . 1)."
  (interactive (list (read-file-name "File to add : ")))
  (if (file-exists-p filename)
      (progn
        (find-file filename)
        (torus-add-location))
    (message "File %s does not exist." filename)))

;;;###autoload
(defun torus-add-torus (torus-name)
  "Create a new torus named TORUS-NAME.
Copy the current torus variables into the new torus."
  (interactive
   (list (read-string "Name of the new torus : "
                      nil
                      'torus-input-history)))
  (torus--update-meta)
  (if (and torus-torus torus-history torus-input-history)
      (progn
        (torus--update-input-history torus-name)
        (if (assoc torus-name torus-meta)
            (message "Torus %s already exists in torus-meta" torus-name)
          (message "Creating torus %s" torus-name)
          (push (list torus-name) torus-meta)
          (push (cons "input history" torus-input-history) (cdr (car torus-meta)))
          (push (cons "layout" torus-layout) (cdr (car torus-meta)))
          (push (cons "history" torus-history) (cdr (car torus-meta)))
          (push (cons "torus" torus-torus) (cdr (car torus-meta)))))
    (message "Cannot create an empty torus. Please add at least a location.")))

;;; Navigating
;;; ------------

;;;###autoload
(defun torus-previous-circle ()
  "Jump to the previous circle."
  (interactive)
  (if torus-torus
      (if (> (length torus-torus) 1)
          (progn
            (torus--prefix-argument current-prefix-arg)
            (torus--update-position)
            (setf torus-torus (append (last torus-torus) (butlast torus-torus)))
            (torus--jump)
            (torus--apply-or-fill-layout))
        (message "Only one circle in torus."))
    (message torus--message-empty-torus)))

;;;###autoload
(defun torus-next-circle ()
  "Jump to the next circle."
  (interactive)
  (if torus-torus
      (if (> (length torus-torus) 1)
          (progn
            (torus--prefix-argument current-prefix-arg)
            (torus--update-position)
            (setf torus-torus (append (cdr torus-torus) (list (car torus-torus))))
            (torus--jump)
            (torus--apply-or-fill-layout))
        (message "Only one circle in torus."))
    (message torus--message-empty-torus)))

;;;###autoload
(defun torus-previous-location ()
  "Jump to the previous location."
  (interactive)
  (if torus-torus
      (if (> (length (car torus-torus)) 1)
          (let ((circle (cdr (car torus-torus))))
            (torus--prefix-argument current-prefix-arg)
            (torus--update-position)
            (setf circle (append (last circle) (butlast circle)))
            (setcdr (car torus-torus) circle)
            (torus--jump))
        (message torus--message-empty-circle (car (car torus-torus))))
    (message torus--message-empty-torus)))

;;;###autoload
(defun torus-next-location ()
  "Jump to the next location."
  (interactive)
  (if torus-torus
      (if (> (length (car torus-torus)) 1)
          (let ((circle (cdr (car torus-torus))))
            (torus--prefix-argument current-prefix-arg)
            (torus--update-position)
            (setf circle (append (cdr circle) (list (car circle))))
            (setcdr (car torus-torus) circle)
            (torus--jump))
        (message torus--message-empty-circle (car (car torus-torus))))
    (message torus--message-empty-torus)))

;;;###autoload
(defun torus-switch-circle (circle-name)
  "Jump to CIRCLE-NAME circle.
With prefix argument \\[universal-argument], open the buffer in a
horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument], open the
buffer in a vertical split."
  (interactive
   (list (completing-read
          "Go to circle : "
          (mapcar #'car torus-torus) nil t)))
  (torus--prefix-argument current-prefix-arg)
  (torus--update-position)
  (let* ((circle (assoc circle-name torus-torus))
         (index (position circle torus-torus :test #'equal))
         (before (subseq torus-torus 0 index))
         (after (subseq torus-torus index)))
    (setq torus-torus (append after before)))
  (torus--jump)
  (torus--apply-or-fill-layout))

;;;###autoload
(defun torus-switch-location (location-name)
  "Jump to LOCATION-NAME location.
With prefix argument \\[universal-argument], open the buffer in a
horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument], open the
buffer in a vertical split."
  (interactive
   (list
    (completing-read
     "Go to location : "
     (mapcar #'torus--concise (cdr (car torus-torus))) nil t)))
  (torus--prefix-argument current-prefix-arg)
  (torus--update-position)
  (let* ((circle (cdr (car torus-torus)))
         (index (position location-name circle
                          :test #'torus--equal-concise-p))
         (before (subseq circle 0 index))
         (after (subseq circle index)))
    (setcdr (car torus-torus) (append after before)))
  (torus--jump))

;;;###autoload
(defun torus-switch-torus (torus-name)
  "Jump to TORUS-NAME torus.
With prefix argument \\[universal-argument], open the buffer in a
horizontal split.
With prefix argument \\[universal-argument] \\[universal-argument], open the
buffer in a vertical split."
  (interactive
   (list (completing-read
          "Go to torus : "
          (mapcar #'car torus-meta) nil t)))
  (torus--prefix-argument current-prefix-arg)
  (torus--update-meta)
  (let* ((torus (assoc torus-name torus-meta))
         (index (position torus torus-meta :test #'equal))
         (before (subseq torus-meta 0 index))
         (after (subseq torus-meta index)))
    (setq torus-meta (append after before)))
  (torus--update-from-meta)
  (torus--build-index)
  (torus--update-layout)
  (torus--apply-or-fill-layout)
  (torus--jump))

;;; Searching
;;; ------------

;;;###autoload
(defun torus-search (location-name)
  "Search LOCATION-NAME in the torus.
Go to the first matching circle and location."
  (interactive
   (list
    (completing-read
     "Search location in torus : "
     (mapcar #'torus--concise torus-index) nil t)))
  (torus--prefix-argument current-prefix-arg)
  (let* ((location-circle
          (find
           location-name torus-index
           :test #'torus--equal-concise-p)))
    (torus--switch location-circle)))

;;; History
;;; ------------

;;;###autoload
(defun torus-history-newer ()
  "Go to newer location in history."
  (interactive)
  (if torus-torus
      (progn
        (torus--prefix-argument current-prefix-arg)
        (if torus-history
            (progn
              (setq torus-history (append (last torus-history) (butlast torus-history)))
              (torus--switch (car torus-history)))
          (message "History is empty.")))
    (message torus--message-empty-torus)))

;;;###autoload
(defun torus-history-older ()
  "Go to older location in history."
  (interactive)
  (if torus-torus
      (progn
        (torus--prefix-argument current-prefix-arg)
        (if torus-history
            (progn
              (setq torus-history (append (cdr torus-history) (list (car torus-history))))
              (torus--switch (car torus-history)))
          (message "History is empty.")))
    (message torus--message-empty-torus)))

;;;###autoload
(defun torus-search-history (location-name)
  "Search LOCATION-NAME in `torus-history'."
  (interactive
   (list
    (completing-read
     "Search location in history : "
     (mapcar #'torus--concise torus-history) nil t)))
  (torus--prefix-argument current-prefix-arg)
  (when torus-history
    (let* ((index (position location-name torus-history
                            :test #'torus--equal-concise-p))
           (before (subseq torus-history 0 index))
           (element (nth index torus-history))
           (after (subseq torus-history (1+ index))))
      (setq torus-history (append (list element) before after)))
    (torus--switch (car torus-history))))

;;;###autoload
(defun torus-alternate ()
  "Alternate last two locations in history.
If outside the torus, just return inside, to the last torus location."
  (interactive)
  (if torus-torus
      (progn
        (torus--prefix-argument current-prefix-arg)
        (if (torus--inside-p)
            (if (and torus-history (>= (length torus-history) 2))
                (progn
                  (setq torus-history (append (list (car (cdr torus-history)))
                                              (list (car torus-history))
                                              (nthcdr 2 torus-history)))
                  (torus--switch (car torus-history)))
              (message "History has less than two elements."))
          (torus--jump)))
    (message torus--message-empty-torus)))

;;;###autoload
(defun torus-alternate-circles ()
  "Alternate last two circles in history."
  (interactive)
  (if torus-torus
      (progn
        (torus--prefix-argument current-prefix-arg)
        (let ((history torus-history)
              (circle (car (car torus-torus)))
              (element)
              (location-circle))
          (while (and (not location-circle) history)
            (setq element (pop history))
            (when (not (equal circle (cdr element)))
              (setq location-circle element)))
          (if location-circle
              (torus--switch location-circle)
            (message "No alternate circle in history."))))
    (message torus--message-empty-torus)))

;;;###autoload
(defun torus-alternate-in-same-circle ()
  "Alternate last two locations in history belonging to the current circle."
  (interactive)
  (if torus-torus
      (progn
        (torus--prefix-argument current-prefix-arg)
        (let ((history torus-history)
              (circle (car (car torus-torus)))
              (element)
              (location-circle))
          (pop history)
          (while (and (not location-circle) history)
            (setq element (pop history))
            (when (equal circle (cdr element))
              (setq location-circle element)))
          (if location-circle
              (torus--switch location-circle)
            (message "No alternate file in same circle in history."))))
    (message torus--message-empty-torus)))

;;; Renaming
;;; ------------

;;;###autoload
(defun torus-rename-circle ()
  "Rename current circle."
  (interactive)
  (if torus-torus
      (let*
          ((old-name (car (car torus-torus)))
           (prompt (format "New name of circle %s : " old-name))
           (circle-name (read-string prompt nil 'torus-input-history)))
        (torus--update-input-history circle-name)
        (setcar (car torus-torus) circle-name)
        (dolist (location-circle torus-index)
          (when (equal (cdr location-circle) old-name)
            (setcdr location-circle circle-name)))
        (dolist (location-circle torus-history)
          (when (equal (cdr location-circle) old-name)
            (setcdr location-circle circle-name)))
        (message "Renamed circle %s -> %s" old-name circle-name))
    (message "Torus is empty. Please add a circle first with torus-add-circle.")))

;;;###autoload
(defun torus-rename-torus ()
  "Rename current torus."
  (interactive)
  (if torus-meta
      (let*
          ((old-name (car (car torus-meta)))
           (prompt (format "New name of torus %s : " old-name))
           (torus-name (read-string prompt nil 'torus-input-history)))
        (torus--update-input-history torus-name)
        (setcar (car torus-meta) torus-name)
        (message "Renamed torus %s -> %s" old-name torus-name))
    (message "Meta Torus is empty.")))

;;; Moving
;;; ------------

;;;###autoload
(defun torus-move-circle (circle-name)
  "Move current circle after CIRCLE-NAME."
  (interactive
   (list (completing-read
          "Move circle after : "
          (mapcar #'car torus-torus) nil t)))
  (let* ((circle (assoc circle-name torus-torus))
         (index (1+ (position circle torus-torus :test #'equal)))
         (current (list (car torus-torus)))
         (before (subseq torus-torus 1 index))
         (after (subseq torus-torus index)))
    (setq torus-torus (append before current after)))
  (torus-switch-circle circle-name))

;;;###autoload
(defun torus-move-location (location-name)
  "Move current location after LOCATION-NAME."
  (interactive
   (list
    (completing-read
     "Move location after : "
     (mapcar #'torus--concise (cdr (car torus-torus))) nil t)))
  (let* ((circle (cdr (car torus-torus)))
         (index (1+ (position location-name circle
                              :test #'torus--equal-concise-p)))
         (current (list (car circle)))
         (before (subseq circle 1 index))
         (after (subseq circle index)))
    (setcdr (car torus-torus) (append before current after)))
  (torus-switch-location location-name))

;;;###autoload
(defun torus-move-to-circle (circle-name)
  "Move current location to CIRCLE-NAME."
  (interactive
   (list (completing-read
          "Move location to circle : "
          (mapcar #'car torus-torus) nil t)))
  (torus--update-position)
  (let* ((location (pop (cdr (car torus-torus))))
        (circle (cdr (assoc circle-name torus-torus)))
        (old-name (car (car torus-torus)))
        (old-pair (cons location old-name)))
    (setcdr (assoc circle-name torus-torus)
            (push location circle))
    (dolist (location-circle torus-index)
      (when (equal location-circle old-pair)
        (setcdr location-circle circle-name)))
    (dolist (location-circle torus-history)
      (when (equal location-circle old-pair)
        (setcdr location-circle circle-name))))
  (torus--jump))

;;;###autoload
(defun torus-move-all-to-circle (circle-name)
  "Move all locations of the current circle to CIRCLE-NAME."
  (interactive
   (list (completing-read
          "Move all locations of current circle to circle : "
          (mapcar #'car torus-torus) nil t)))
  (torus--update-position)
  (while (> (length (car torus-torus)) 1)
    (let* ((location (pop (cdr (car torus-torus))))
           (circle (cdr (assoc circle-name torus-torus)))
           (old-name (car (car torus-torus)))
           (old-pair (cons location old-name)))
      (setcdr (assoc circle-name torus-torus)
              (push location circle))
      (dolist (location-circle torus-index)
        (when (equal location-circle old-pair)
          (setcdr location-circle circle-name)))
      (dolist (location-circle torus-history)
        (when (equal location-circle old-pair)
          (setcdr location-circle circle-name)))))
  (torus--jump)
  (torus-delete-current-circle)
  (torus-switch-circle circle-name))

;;;###autoload
(defun torus-copy-to-circle (circle-name)
  "Move current location to CIRCLE-NAME."
  (interactive
   (list (completing-read
          "Copy location to circle : "
          (mapcar #'car torus-torus) nil t)))
  (torus--update-position)
  (let* ((location (car (cdr (car torus-torus))))
        (circle (cdr (assoc circle-name torus-torus))))
    (setcdr (assoc circle-name torus-torus)
            (push location circle)))
  (torus--build-index)
  (torus--jump))

;;;###autoload
(defun torus-reverse-circles ()
  "Reverse order of the circles."
  (interactive)
  (setq torus-torus (reverse torus-torus))
  (torus--jump))

;;;###autoload
(defun torus-reverse-locations ()
  "Reverse order of the locations in the current circles."
  (interactive)
  (setcdr (car torus-torus) (reverse (cdr (car torus-torus))))
  (torus--jump))

;;;###autoload
(defun torus-deep-reverse ()
  "Reverse order of the locations in each circle."
  (interactive)
  (setq torus-torus (reverse torus-torus))
  (dolist (circle torus-torus)
    (setcdr circle (reverse (cdr circle))))
  (torus--jump))

;;; Joining
;;; ------------------------------

;;;###autoload
(defun torus-prefix-circles-of-current-torus (prefix)
  "Add PREFIX to circle names of `torus-torus'."
  (interactive
   (list
    (read-string (format torus--message-prefix-circle
                         (car (car torus-meta)))
                 nil
                 'torus-input-history)))
  (let ((varlist))
    (setq varlist (torus--prefix-circles prefix (car (car torus-meta))))
    (setq torus-torus (car varlist))
    (setq torus-history (car (cdr varlist))))
  (torus--build-index))

;;;###autoload
(defun torus-join-circles (circle-name)
  "Join current circle with CIRCLE-NAME."
  (interactive
   (list
    (completing-read "Join current circle with circle : "
                     (mapcar #'car torus-torus) nil t)))
  (let* ((current-name (car (car torus-torus)))
         (join-name (concat current-name " - " circle-name))
         (user-choice
          (read-string (format "Name of the joined torus [%s] : " join-name))))
    (when (> (length user-choice) 0)
      (setq join-name user-choice))
    (torus-add-circle join-name)
    (setcdr (car torus-torus)
            (append (cdr (assoc current-name torus-torus))
                    (cdr (assoc circle-name torus-torus))))
    (delete-dups (cdr (car torus-torus))))
  (torus--update-meta)
  (torus--build-index)
  (torus--jump))

;;;###autoload
(defun torus-join-toruses (torus-name)
  "Join current torus with TORUS-NAME in `torus-meta'."
  (interactive
   (list
    (completing-read "Join current torus with torus : "
                     (mapcar #'car torus-meta) nil t)))
  (torus--prefix-argument current-prefix-arg)
  (torus--update-meta)
  (let* ((current-name (car (car torus-meta)))
         (join-name (concat current-name " - " torus-name))
         (user-choice
          (read-string (format "Name of the joined torus [%s] : " join-name)))
         (prompt-current
          (format torus--message-prefix-circle current-name))
         (prompt-added
          (format torus--message-prefix-circle torus-name))
         (prefix-current
          (read-string prompt-current nil 'torus-input-history))
         (prefix-added
          (read-string prompt-added nil 'torus-input-history))
         (varlist)
         (torus-added)
         (history-added)
         (input-added))
    (when (> (length user-choice) 0)
      (setq join-name user-choice))
    (torus--update-input-history prefix-current)
    (torus--update-input-history prefix-added)
    (torus-add-torus join-name)
    (torus-prefix-circles-of-current-torus prefix-current)
    (setq varlist (torus--prefix-circles prefix-added torus-name))
    (setq torus-added (car varlist))
    (setq history-added (car (cdr varlist)))
    (setq input-added (car (cdr (cdr varlist))))
    (if (seq-intersection torus-torus torus-added #'torus--equal-car-p)
        (message torus--message-circle-name-collision)
      (setq torus-torus (append torus-torus torus-added))
      (setq torus-history (append torus-history history-added))
      (setq torus-input-history (append torus-input-history input-added))))
  (torus--update-meta)
  (torus--build-index)
  (torus--jump))

;;; Autogrouping
;;; ------------

;;;###autoload
(defun torus-autogroup (quoted-function)
  "Autogroup all torus locations according to the values of QUOTED-FUNCTION.
A new torus is created on `torus-meta' to contain the new circles.
The function must return the names of the new circles as strings."
  (interactive)
  (let ((torus-name
         (read-string "Name of the autogroup torus : "
                      nil
                      'torus-input-history))
        (all-locations))
    (if (assoc torus-name torus-meta)
        (message "Torus %s already exists in torus-meta" torus-name)
      (torus-add-torus torus-name)
      (dolist (circle torus-torus)
        (dolist (location (cdr circle))
          (push location all-locations)))
      (setq torus-torus (seq-group-by quoted-function all-locations))))
  (setq torus-history nil)
  (setq torus-markers nil)
  (setq torus-input-history nil)
  (torus--build-index)
  (torus--update-meta)
  (torus--jump))

;;;###autoload
(defun torus-autogroup-by-path ()
  "Autogroup all location of the torus by directories."
  (interactive)
  (torus-autogroup (lambda (elem) (directory-file-name (file-name-directory (car elem))))))

;;;###autoload
(defun torus-autogroup-by-directory ()
  "Autogroup all location of the torus by directories."
  (interactive)
  (torus-autogroup #'torus--directory))

;;;###autoload
(defun torus-autogroup-by-extension ()
  "Autogroup all location of the torus by extension."
  (interactive)
  (torus-autogroup #'torus--extension-description))

;;;###autoload
(defun torus-autogroup-menu (choice)
  "Autogroup according to CHOICE."
  (interactive
   (list (read-key torus--message-autogroup-choice)))
    (pcase choice
      (?p (funcall 'torus-autogroup-by-path))
      (?d (funcall 'torus-autogroup-by-directory))
      (?e (funcall 'torus-autogroup-by-extension))
      (?\a (message "Autogroup cancelled by Ctrl-G."))
      (_ (message "Invalid key."))))

;;; Deleting
;;; ------------

;;;###autoload
(defun torus-delete-circle (circle-name)
  "Delete circle given by CIRCLE-NAME."
  (interactive
   (list
    (completing-read "Delete circle : "
                     (mapcar #'car torus-torus) nil t)))
  (when (y-or-n-p (format "Delete circle %s ? " circle-name))
    (setq torus-torus (torus--assoc-delete-all circle-name torus-torus))
    (setq torus-index
          (torus--reverse-assoc-delete-all circle-name torus-index))
    (setq torus-history
          (torus--reverse-assoc-delete-all circle-name torus-history))
    (setq torus-markers
          (torus--reverse-assoc-delete-all circle-name torus-markers))
    (torus--jump)))

;;;###autoload
(defun torus-delete-location (location-name)
  "Delete location given by LOCATION-NAME."
  (interactive
   (list
    (completing-read
     "Delete location : "
     (mapcar #'torus--concise (cdr (car torus-torus))) nil t)))
  (if (and
       (> (length (car torus-torus)) 1)
       (y-or-n-p
        (format
         "Delete %s from circle %s ? "
         location-name
         (car (car torus-torus)))))
      (let* ((circle (cdr (car torus-torus)))
             (index (position location-name circle
                              :test #'torus--equal-concise-p))
           (location (nth index circle)))
        (setcdr (car torus-torus) (delete location circle))
        (setq torus-index (torus--assoc-delete-all location torus-index))
        (setq torus-history (torus--assoc-delete-all location torus-history))
        (setq torus-markers (torus--assoc-delete-all location torus-markers))
        (torus--jump))
    (message "No location in current circle.")))

;;;###autoload
(defun torus-delete-current-circle ()
  "Delete current circle."
  (interactive)
  (torus-delete-circle (torus--concise (car (car torus-torus)))))

;;;###autoload
(defun torus-delete-current-location ()
  "Remove current location from current circle."
  (interactive)
  (torus-delete-location (torus--concise (car (cdr (car torus-torus))))))

;;;###autoload
(defun torus-delete-torus (torus-name)
  "Delete torus given by TORUS-NAME."
  (interactive
   (list
    (completing-read "Delete torus : "
                     (mapcar #'car torus-meta) nil t)))
  (when (y-or-n-p (format "Delete torus %s ? " torus-name))
    (when (equal torus-name (car (car torus-meta)))
      (torus-switch-torus (car (car (cdr torus-meta)))))
    (setq torus-meta (torus--assoc-delete-all torus-name torus-meta))))

;;; Splitting
;;; ------------

;;;###autoload
(defun torus-split-horizontally ()
  "Split horizontally to view all buffers in current circle.
Split until `torus-maximum-horizontal-split' is reached."
  (interactive)
  (let* ((circle (cdr (car torus-torus)))
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
        (torus-next-location))
      (other-window 1)
      (torus-next-location))))

;;;###autoload
(defun torus-split-vertically ()
  "Split vertically to view all buffers in current circle.
Split until `torus-maximum-vertical-split' is reached."
  (interactive)
  (let* ((circle (cdr (car torus-torus)))
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
        (torus-next-location))
      (other-window 1)
      (torus-next-location))))

;;;###autoload
(defun torus-split-main-left ()
  "Split with left main window to view all buffers in current circle."
  (interactive)
  (let* ((circle (cdr (car torus-torus)))
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
  (let* ((circle (cdr (car torus-torus)))
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
  (let* ((circle (cdr (car torus-torus)))
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
  (let* ((circle (cdr (car torus-torus)))
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
  (let* ((circle (cdr (car torus-torus)))
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

;;;###autoload
(defun torus-layout-menu (choice)
  "Split according to CHOICE."
  (interactive
   (list (read-key torus--message-layout-choice)))
  (torus--update-layout)
  (let ((circle (caar torus-torus)))
    (when (member choice '(?m ?o ?h ?v ?l ?r ?t ?b ?g))
      (setcdr (assoc circle torus-layout) choice))
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

;;; File R/W
;;; ------------

;;;###autoload
(defun torus-write (filename)
  "Write main torus variables to FILENAME as Lisp code.
An adequate extension is added if needed.
If called interactively, ask for the variables to save (default : all)."
  (interactive
   (list
    (read-file-name
     "Torus file : "
     (file-name-as-directory torus-dirname))))
  (torus--update-position)
  (let*
      ((file-basename (file-name-nondirectory filename))
       (minus-len-ext (- (min (length torus-extension)
                              (length filename))))
       (buffer)
       (varlist '(torus-meta
                  torus-torus
                  torus-index
                  torus-history
                  torus-layout
                  torus-input-history)))
    (when (called-interactively-p 'interactive)
      (pcase (read-key torus--message-write-choice)
        (?m (setq varlist (list 'torus-meta)))
        (?t (setq varlist (list 'torus-torus)))
        (?i (setq varlist (list 'torus-index)))
        (?h (setq varlist (list 'torus-history)))
        (?l (setq varlist (list 'torus-layout)))
        (?n (setq varlist (list 'torus-input-history)))
        (?\a (setq varlist nil))
        (_ (message "All variables will be written."))))
    (torus--update-input-history file-basename)
    (unless (equal (subseq filename minus-len-ext) torus-extension)
      (setq filename (concat filename torus-extension)))
    (unless torus-index
      (torus--build-index))
    (torus--update-layout)
    (torus--update-meta)
    (if varlist
        (if (and torus-meta
                 torus-torus
                 torus-index
                 torus-history
                 torus-layout
                 torus-input-history)
            (progn
              (setq buffer (find-file-noselect filename))
              (with-current-buffer buffer
                (erase-buffer)
                (dolist (var varlist)
                  (insert (concat
                           "(setq "
                           (symbol-name var)
                           " (quote "))
                  (pp (symbol-value var) buffer)
                  (insert "))\n\n"))
                (save-buffer)
                (kill-buffer)))
          (message "Write cancelled : some variables are nil."))
      (message "Write cancelled by Ctrl-G."))))

;;;###autoload
(defun torus-read (filename)
  "Read main torus variables from FILENAME as Lisp code."
  (interactive
   (list
    (read-file-name
     "Torus file : "
     (file-name-as-directory torus-dirname))))
  (let*
      ((file-basename (file-name-nondirectory filename))
       (minus-len-ext (- (min (length torus-extension)
                              (length filename))))
       (buffer))
    (unless (equal (subseq filename minus-len-ext) torus-extension)
      (setq filename (concat filename torus-extension)))
    (when (or (and (not torus-meta)
                   (not torus-torus)
                   (not torus-index)
                   (not torus-history)
                   (not torus-layout)
                   (not torus-input-history))
              (y-or-n-p torus--message-replace-torus))
      (torus--update-input-history file-basename)
      (if (file-exists-p filename)
          (progn
            (setq buffer (find-file-noselect filename))
            (eval-buffer buffer)
            (kill-buffer buffer))
        (message "File %s does not exist." filename))))
  ;; Also saved in file
  ;; (torus--update-meta)
  ;; (torus--build-index)
  (torus--jump))

;;; End
;;; ------------------------------

(provide 'torus)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; torus.el ends here
