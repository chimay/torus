;;; torus.el --- A buffer groups manager             -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Chimay

;; Author : Chimay
;; Name: Torus
;; Package-Version: 1.5
;; Package-requires: ((emacs "26"))
;; Keywords: files, buffer, group, switch, save, split
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
;; many buffer groups as you need, and quickly navigate between :
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

;;; Code:
;;; ------------------------------------------------------------

;;; Version
;;; ------------------------------

(defvar torus-version "1.5"
  "Version number of torus.")

;;; Requires
;;; ------------------------------

(require 'cl-lib)

;;; Custom group
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

(defcustom torus-dirname user-emacs-directory
  "The directory where the torus are read and written."
  :type 'string
  :group 'torus)

(defcustom torus-optional-bindings 0
  "Whether to activate optional keybindings."
  :type 'integer
  :group 'torus)

(defcustom torus-save-on-exit nil
  "Whether to ask to save torus on exit of Emacs.
If set to t `torus-init' will install saving of torus on exit.
The function `torus-quit' is placed on `kill-emacs-hook'."
  :type 'boolean
  :group 'torus)

(defcustom torus-history-maximum-elements 30
  "Maximum number of elements in `torus-history'."
  :type 'integer
  :group 'torus)

(defcustom torus-maximum-horizontal-split 3
  "Maximum number of horizontal split, see `torus-split-horizontally'."
  :type 'integer
  :group 'torus)

(defcustom torus-maximum-vertical-split 3
  "Maximum number of vertical split, see `torus-split-vertically'."
  :type 'integer
  :group 'torus)

(defcustom torus-verbosity 1
  "Level of verbosity.
1 = normal
2 = debug."
  :type 'integer
  :group 'torus)

;;; Variables
;;; ------------------------------

(defvar torus-torus nil
  "The torus is a list of circles.
A circle is a list of locations, stored in the form :
\(\"circle name\" locations)
A location is a pair (file . position)
Most recent entries are in the beginning of the lists.")

(defvar torus-meta nil
  "List of existing toruses.
You can create new torus with `torus-add-torus`.
A new torus is also created when you load one from a file.")

(defvar torus-index nil
  "Alist giving circles corresponding to torus locations.
Each element has the form :
\((file . position) . circle)
Allow to search among all files of the torus.")

(defvar torus-history nil
  "Alist containing the history of visited buffers (locations) in the torus.
Each element is of the form :
\((file . position) . circle)
Same format as in `torus-index'.")

(defvar torus-markers nil
  "Alist containing markers to opened files.
Each element is of the form :
\((file . position) . marker)
Contain only the files opened in buffers.")

(defvar torus-input-history nil
  "History of user input.")

(defvar torus-filename nil
  "Filename where the last torus has been saved or read.")

(defvar torus-added nil
  "Last torus added from a file.")

(defvar torus-added-history nil
  "Last history added from a file.")

(defvar torus-prefix-key (kbd "s-t")
  "Prefix key for the torus key mappings.")

(defvar torus-prefix-separator " : "
  "String between the prefix and the circle names.
When a torus read from a file is appended to the existing one,
the name of the new circles will be of the form :
\"user_input_prefix `torus-prefix-separator' name_of_the_added_circle\"
without the spaces. If the user enter
a blank prefix, the added circle names remain untouched.")

;; Long prompts

(defvar torus--message-empty-torus "Torus is empty. You can use torus-add-circle to add a circle to it.")
(defvar torus--message-empty-circle "No location in circle %s. You can use torus-add-location to fill the circle.")
(defvar torus--message-prefix-circle "Prefix for the circle names of %s (leave blank for none) ? ")
(defvar torus--message-existent-location "Location %s already exists in circle %s")
(defvar torus--message-print-choice "Print [l] torus list [t] torus \n      [i] index [h] history [m] markers [n] input history [a] all")

;;; Keymap with prefix
;;; ------------------------------

(defvar torus-map)

(define-prefix-command 'torus-map)

;;; Toolbox
;;; ------------------------------

(defun torus--assoc-delete-all (key alist)
  "Remove all elements with key matching KEY in ALIST."
  (cl-remove key alist :test 'equal :key 'car))

(when (fboundp 'assoc-delete-all)
  (defalias 'torus--assoc-delete-all 'assoc-delete-all))

(defun torus--reverse-assoc-delete-all (value alist)
  "Remove all elements with value matching VALUE in ALIST."
  (cl-remove value alist :test 'equal :key 'cdr))

;;; Private Functions
;;; ------------------------------

(defun torus--buffer-or-filename (location)
  "Return buffer name of LOCATION if existent in `torus-markers', file basename otherwise."
  (let* ((bookmark (cdr (assoc location torus-markers)))
         (buffer (when bookmark (marker-buffer bookmark))))
    (if buffer
        (buffer-name buffer)
      (file-name-nondirectory (car location)))))

(defun torus--concise (object)
  "Return OBJECT in concise string format.
If OBJECT is a string : simply returns OBJECT.
If OBJECT is \(File . Position) : returns \"File at Position.\"
If OBJECT is \((File . Position) . Circle) : returns
\"File at Position : circle Circle.\""
  (if (stringp object) object
    (if (consp object)
        (if (consp (car object))
            (let* ((location (car object))
                   (file (torus--buffer-or-filename location))
                   (position (prin1-to-string (cdr location)))
                   (circle (cdr object)))
              (concat circle " > " file " at " position))
          (let ((file (torus--buffer-or-filename object))
                 (position (prin1-to-string (cdr object))))
            (concat file " at " position))))))

(defun torus--equal-concise (one two)
  "Return t if the concise representations of ONE and TWO are equal."
  (equal (torus--concise one)
         (torus--concise two)))

(defun torus--inside-p ()
  "Whether the current location belongs to the torus."
  (if (and (car torus-torus) (> (length (car torus-torus)) 1))
      (let* ((location (car (cdr (car torus-torus)))))
        (equal (car location) (buffer-file-name (current-buffer))))))

(defun torus--update-position ()
  "Update position in current location.
Do nothing if file does not match current buffer."
  (if (and torus-torus (car torus-torus) (> (length (car torus-torus)) 1))
      (let* ((location (car (cdr (car torus-torus))))
             (bookmark (assoc location torus-markers)))
        (when (equal (car location) (buffer-file-name (current-buffer)))
          (setcdr (car (cdr (car torus-torus))) (point))
          (if bookmark
              (setcdr (assoc location torus-markers) (point-marker))
            (push (cons location (point-marker)) torus-markers))))))

(defun torus--update-history ()
  "Add current location to `torus-history'."
  (if (and (car torus-torus) (> (length (car torus-torus)) 1))
      (let* ((circle (car torus-torus))
             (location (car (cdr circle)))
             (location-circle (cons location (car circle))))
        (push location-circle torus-history)
        (delete-dups torus-history)
        (setq torus-history
              (subseq torus-history 0
                      (min (length torus-history)
                           torus-history-maximum-elements))))))

(defun torus--update-meta ()
  "Update current torus in `torus-meta'."
  (torus--update-position)
  (when torus-meta
    (setf (alist-get
           "torus"
           (cdr (car torus-meta))
           nil nil 'equal)
          torus-torus)
    (setf (alist-get
           "history"
           (cdr (car torus-meta))
           nil nil 'equal)
          torus-history)
    (setf (alist-get
           "input history"
           (cdr (car torus-meta))
           nil nil 'equal)
          torus-input-history)))

(defun torus--update-from-meta ()
  "Update main torus variables from `torus-meta'."
  (let ((entry (cdr (car torus-meta))))
    (setq torus-torus (cdr (assoc "torus" entry)))
    (setq torus-history (cdr (assoc "history" entry)))
    (setq torus-input-history (cdr (assoc "input history" entry)))))

(defun torus--jump ()
  "Jump to current location (buffer & position) in torus.
Add the location to `torus-markers' if not already present."
  (if (and torus-torus (car torus-torus) (> (length (car torus-torus)) 1))
      (let* ((location (car (cdr (car torus-torus))))
             (file (car location))
             (bookmark (cdr (assoc location torus-markers)))
             (buffer (when bookmark (marker-buffer bookmark))))
        (if (and bookmark buffer (buffer-live-p buffer))
            (progn
              (when (> torus-verbosity 1) (message "Found %s in torus-markers" bookmark))
              (when (not (equal buffer (current-buffer)))
                (switch-to-buffer buffer))
              (goto-char bookmark))
          (when (> torus-verbosity 1) (message "Found %s in torus" location))
          (setq torus-markers (torus--assoc-delete-all location torus-markers))
          (pp torus-markers)
          (if (file-exists-p file)
              (progn
                (find-file file)
                (goto-char (cdr location))
                (push (cons location (point-marker)) torus-markers))
            (message (format "File %s does not exist anymore. It will be removed from the torus." file))
            (setcdr (car torus-torus) (delete location (cdr (car torus-torus))))
            (setq torus-index (torus--assoc-delete-all location torus-index))
            (setq torus-history (torus--assoc-delete-all location torus-history))
            (setq torus-markers (torus--assoc-delete-all location torus-markers))))
        (torus--update-history)
        (torus-info))))

(defun torus--switch (location-circle)
  "Jump to circle and location countained in LOCATION-CIRCLE."
  (torus--update-position)
  (if (and location-circle (consp location-circle) (consp (car location-circle)))
      (progn
        (let* ((circle-name (cdr location-circle))
               (circle (assoc circle-name torus-torus))
               (index (position circle torus-torus :test #'equal))
               (before (subseq torus-torus 0 index))
               (after (subseq torus-torus index (length torus-torus))))
          (setq torus-torus (append after before)))
        (let* ((circle (cdr (car torus-torus)))
               (location (car location-circle))
               (index (position location circle :test #'equal))
               (before (subseq circle 0 index))
               (after (subseq circle index (length circle))))
          (setcdr (car torus-torus) (append after before)))))
  (torus--jump))

(defun torus--build-index ()
  "Build `torus-index'."
  (setq torus-index nil)
  (dolist (circle torus-torus)
    (dolist (location (cdr circle))
      (let ((location-circle (cons location (car circle))))
        (unless (member location-circle torus-index)
          (push location-circle torus-index)))))
  (setq torus-index (reverse torus-index)))

(defun torus--prefix-argument (prefix)
  "Handle prefix argument PREFIX. Used to split."
  (pcase prefix
   ('(4)
    (split-window-below)
    (other-window 1))
   ('(16)
    (split-window-right)
    (other-window 1))))

(defun torus--quit ()
  "Write torus before quit."
  (if torus-save-on-exit
      (when (and
             torus-torus
             (y-or-n-p "Write torus ? "))
        (torus-write-all))))

;;; Commands
;;; ------------------------------

(defun torus-install-default-bindings ()
  "Install default keybindings."
  (interactive)
  (global-set-key torus-prefix-key 'torus-map)
  (when (>= torus-optional-bindings 0)
    (define-key torus-map (kbd "i") 'torus-info)
    (define-key torus-map (kbd "c") 'torus-add-circle)
    (define-key torus-map (kbd "l") 'torus-add-location)
    (define-key torus-map (kbd "<left>") 'torus-previous-circle)
    (define-key torus-map (kbd "<right>") 'torus-next-circle)
    (define-key torus-map (kbd "<up>") 'torus-previous-location)
    (define-key torus-map (kbd "<down>") 'torus-next-location)
    (define-key torus-map (kbd "SPC") 'torus-switch-circle)
    (define-key torus-map (kbd "=") 'torus-switch-location)
    (define-key torus-map (kbd "@") 'torus-switch-torus)
    (define-key torus-map (kbd "<next>") 'torus-history-older)
    (define-key torus-map (kbd "<prior>") 'torus-history-newer)
    (define-key torus-map (kbd "^") 'torus-alternate)
    (define-key torus-map (kbd "n") 'torus-rename-circle)
    (define-key torus-map (kbd "d") 'torus-delete-location)
    (define-key torus-map (kbd "D") 'torus-delete-circle)
    (define-key torus-map (kbd "_") 'torus-split-horizontally)
    (define-key torus-map (kbd "|") 'torus-split-vertically)
    (define-key torus-map (kbd "r") 'torus-read-all)
    (define-key torus-map (kbd "w") 'torus-write-all)
    (define-key torus-map (kbd "a") 'torus-read-append-all))
  (when (>= torus-optional-bindings 1)
    (define-key torus-map (kbd "+") 'torus-add-torus)
    (define-key torus-map (kbd "s") 'torus-search)
    (define-key torus-map (kbd "h") 'torus-search-history)
    (define-key torus-map (kbd "<") 'torus-alternate-circles)
    (define-key torus-map (kbd ">") 'torus-alternate-in-same-circle)
    (define-key torus-map (kbd "N") 'torus-rename-torus)
    (define-key torus-map (kbd "m") 'torus-move-location)
    (define-key torus-map (kbd "M") 'torus-move-circle)
    (define-key torus-map (kbd "t") 'torus-move-to-circle)
    (define-key torus-map (kbd "T") 'torus-move-all-to-circle)
    (define-key torus-map (kbd "y") 'torus-copy-to-circle)
    (define-key torus-map (kbd "-") 'torus-delete-torus))
  (when (>= torus-optional-bindings 2)
    (define-key torus-map (kbd "z") 'torus-zero)
    (define-key torus-map (kbd "p") 'torus-print)
    (define-key torus-map (kbd "! l") 'torus-reverse-locations)
    (define-key torus-map (kbd "! c") 'torus-reverse-circles)
    (define-key torus-map (kbd "! d") 'torus-deep-reverse)
    (define-key torus-map (kbd ":") 'torus-prefix-circles-of-current-torus)
    (define-key torus-map (kbd "R") 'torus-read-meta)
    (define-key torus-map (kbd "W") 'torus-write-meta))
  (when (>= torus-optional-bindings 3)
    (define-key torus-map (kbd "x") 'torus-delete-current-location)
    (define-key torus-map (kbd "X") 'torus-delete-current-circle)
    (define-key torus-map (kbd "C-r") 'torus-read)
    (define-key torus-map (kbd "C-w") 'torus-write)
    (define-key torus-map (kbd "C-a") 'torus-read-append)))

(defun torus-zero ()
  "Reset torus and main variables to nil."
  (interactive)
  (message "Main variables -> nil")
  (setq torus-meta nil)
  (setq torus-torus nil)
  (setq torus-index nil)
  (setq torus-history nil)
  (setq torus-markers nil)
  (setq torus-input-history nil)
  (setq torus-added nil)
  (setq torus-filename nil))

(defun torus-init ()
  "Initialize torus, create directory if needed, add hooks."
  (interactive)
  (torus-zero)
  (unless (file-exists-p torus-dirname) (make-directory torus-dirname))
  (add-hook 'kill-emacs-hook 'torus--quit))

;;; Printing
;;; ------------

(defun torus-info ()
  "Print local info : circle name and locations."
  (interactive)
  (if torus-torus
      (if (> (length (car torus-torus)) 1)
          (let* ((circle (car torus-torus))
                 (prettylist
                  (mapcar
                   #'(lambda (elem)
                       (cons
                        (torus--buffer-or-filename elem)
                        (cdr elem)))
                   (cdr circle))))
            (message "%s : %s" (car circle) prettylist))
        (message torus--message-empty-circle (car (car torus-torus))))
    (message torus--message-empty-torus)))

(defun torus-print ()
  "Print torus and markers in opened files."
  (interactive)
  (let ((choice (read-key torus--message-print-choice))
        (window))
    (setq window (view-echo-area-messages))
    (pcase choice
      (?l (pp torus-meta))
      (?t (pp torus-torus))
      (?i (pp torus-index))
      (?h (pp torus-history))
      (?m (pp torus-markers))
      (?n (pp torus-input-history))
      (?a (dolist (var '(torus-meta
                         torus-torus
                         torus-index
                         torus-history
                         torus-markers
                         torus-input-history))
            (pp (symbol-value var))))
      (?\a (delete-window window)
           (message "Print cancelled by Ctrl-G."))
      (_ (message "Invalid key.")))))

;;; Adding
;;; ------------

(defun torus-add-circle (circle-name)
  "Add a new circle CIRCLE-NAME to torus."
  (interactive
   (list
    (read-string "Name for the new circle : "
                 nil
                 'torus-input-history)))
    (delete-dups torus-input-history)
    (unless (or (= (length circle-name) 0) (member circle-name torus-input-history))
      (push circle-name torus-input-history))
    (if (assoc circle-name torus-torus)
        (message "Circle %s already exists in torus" circle-name)
      (message "Adding circle %s to torus" circle-name)
      (push (list circle-name) torus-torus)))

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

(defun torus-add-torus (&rest arguments)
  "Create a new torus with ARGUMENTS.
ARGUMENTS is either :
\()
\(torus-name)
\(`torus-torus' `torus-history' `torus-input-history')
\(torus-name `torus-torus' `torus-history' `torus-input-history')
If no torus name is given, prompts for one.
If no torus, history, input history is given, take the current ones."
  (interactive)
  (torus--update-meta)
  (let ((lenarg (length arguments))
        (args)
        (name)
        (prompt "Name for the new torus : "))
    (pcase lenarg
      (0 (setq name (read-string prompt nil 'torus-input-history))
         (setq args (list torus-torus torus-history torus-input-history)))
      (1 (setq name (car arguments))
         (setq args (list torus-torus torus-history torus-input-history)))
      (3 (setq args arguments))
      (4 (setq name (car arguments))
         (setq args (subseq arguments 1))))
    (delete-dups torus-input-history)
    (unless (or (= (length name) 0) (member name torus-input-history))
      (push name torus-input-history))
    (if (and (nth 0 args)
             (nth 1 args)
             (nth 2 args))
        (if (assoc name torus-meta)
            (message "Torus %s already exists in torus-meta" name)
          (message "Creating torus %s" name)
          (push (list name) torus-meta)
          (push (cons "input history" (nth 2 args)) (cdr (car torus-meta)))
          (push (cons "history" (nth 1 args)) (cdr (car torus-meta)))
          (push (cons "torus" (nth 0 args)) (cdr (car torus-meta))))
      (message "Cannot create a new torus in torus-meta with empty variable(s)"))))

;;; Navigating
;;; ------------

(defun torus-previous-circle ()
  "Jump to the previous circle."
  (interactive)
  (torus--prefix-argument current-prefix-arg)
  (if torus-torus
      (if (> (length torus-torus) 1)
          (progn
            (torus--update-position)
            (setf torus-torus (append (last torus-torus) (butlast torus-torus)))
            (torus--jump))
        (message "Only one circle in torus."))
    (message torus--message-empty-torus)))

(defun torus-next-circle ()
  "Jump to the next circle."
  (interactive)
  (torus--prefix-argument current-prefix-arg)
  (if torus-torus
      (if (> (length torus-torus) 1)
          (progn
            (torus--update-position)
            (setf torus-torus (append (cdr torus-torus) (list (car torus-torus))))
            (torus--jump))
        (message "Only one circle in torus."))
    (message torus--message-empty-torus)))

(defun torus-previous-location ()
  "Jump to the previous location."
  (interactive)
  (torus--prefix-argument current-prefix-arg)
  (if torus-torus
      (if (> (length (car torus-torus)) 1)
          (let ((circle (cdr (car torus-torus))))
            (torus--update-position)
            (setf circle (append (last circle) (butlast circle)))
            (setcdr (car torus-torus) circle)
            (torus--jump))
        (message torus--message-empty-circle (car (car torus-torus))))
    (message torus--message-empty-torus)))

(defun torus-next-location ()
  "Jump to the next location."
  (interactive)
  (torus--prefix-argument current-prefix-arg)
  (if torus-torus
      (if (> (length (car torus-torus)) 1)
          (let ((circle (cdr (car torus-torus))))
            (torus--update-position)
            (setf circle (append (cdr circle) (list (car circle))))
            (setcdr (car torus-torus) circle)
            (torus--jump))
        (message torus--message-empty-circle (car (car torus-torus))))
    (message torus--message-empty-torus)))

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
         (after (subseq torus-torus index (length torus-torus))))
    (setq torus-torus (append after before)))
  (torus--jump))

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
                          :test #'torus--equal-concise))
         (before (subseq circle 0 index))
         (after (subseq circle index (length circle))))
    (setcdr (car torus-torus) (append after before)))
  (torus--jump))

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
         (after (subseq torus-meta index (length torus-meta))))
    (setq torus-meta (append after before)))
  (torus--update-from-meta)
  (torus--build-index)
  (torus--jump))

;;; Searching
;;; ------------

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
           :test #'torus--equal-concise)))
    (torus--switch location-circle)))

;;; History
;;; ------------

(defun torus-history-newer ()
  "Go to newer location in history."
  (interactive)
  (torus--prefix-argument current-prefix-arg)
  (when torus-history
    (setq torus-history (append (last torus-history) (butlast torus-history)))
    (torus--switch (car torus-history))))

(defun torus-history-older ()
  "Go to older location in history."
  (interactive)
  (torus--prefix-argument current-prefix-arg)
  (when torus-history
    (setq torus-history (append (cdr torus-history) (list (car torus-history))))
    (torus--switch (car torus-history))))

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
                            :test #'torus--equal-concise))
           (before (subseq torus-history 0 index))
           (element (nth index torus-history))
           (after (subseq torus-history (1+ index) (length torus-history))))
      (setq torus-history (append (list element) before after)))
    (torus--switch (car torus-history))))

(defun torus-alternate ()
  "Alternate last two locations in history.
If outside the torus, just return inside, to the last torus location."
  (interactive)
  (torus--prefix-argument current-prefix-arg)
  (if (torus--inside-p)
      (when (and torus-history (>= (length torus-history) 2))
        (setq torus-history (append
                             (list (car (cdr torus-history)))
                             (list (car torus-history))
                             (nthcdr 2 torus-history)))
        (torus--switch (car torus-history)))
    (torus--jump)))

(defun torus-alternate-circles ()
  "Alternate last two circles in history."
  (interactive)
  (let ((history torus-history)
        (circle (car (car torus-torus)))
        (element)
        (location-circle))
    (while (and (not location-circle) history)
      (setq element (pop history))
      (when (not (equal circle (cdr element)))
        (setq location-circle element)))
    (torus--switch location-circle)))

(defun torus-alternate-in-same-circle ()
  "Alternate last two locations in history belonging to the current circle."
  (interactive)
  (let ((history torus-history)
        (circle (car (car torus-torus)))
        (element)
        (location-circle))
    (pop history)
    (while (and (not location-circle) history)
      (setq element (pop history))
      (when (equal circle (cdr element))
        (setq location-circle element)))
    (torus--switch location-circle)))

;;; Renaming
;;; ------------

(defun torus-rename-circle ()
  "Rename current circle."
  (interactive)
  (if torus-torus
      (let*
          ((name)
           (oldname (car (car torus-torus)))
           (prompt (format "New name for circle %s : " oldname)))
        (setq name (read-string prompt nil 'torus-input-history))
        (delete-dups torus-input-history)
        (unless (or (= (length name) 0) (member name torus-input-history))
          (push name torus-input-history))
        (setcar (car torus-torus) name)
        (dolist (location-circle torus-index)
          (when (equal (cdr location-circle) oldname)
            (setcdr location-circle name)))
        (dolist (location-circle torus-history)
          (when (equal (cdr location-circle) oldname)
            (setcdr location-circle name)))
        (message "Renamed circle %s -> %s" oldname name))
    (message "Torus is empty. Please add a circle first with torus-add-circle.")))

(defun torus-rename-torus ()
  "Rename current torus."
  (interactive)
  (if torus-meta
      (let*
          ((name)
           (oldname (car (car torus-meta)))
           (prompt (format "New name for torus %s : " oldname)))
        (setq name (read-string prompt nil 'torus-input-history))
        (delete-dups torus-input-history)
        (unless (or (= (length name) 0) (member name torus-input-history))
          (push name torus-input-history))
        (setcar (car torus-meta) name)
        (message "Renamed torus %s -> %s" oldname name))
    (message "Torus List is empty. You can add the current torus to the list with torus-add-torus.")))

;;; Moving
;;; ------------

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
         (after (subseq torus-torus index (length torus-torus))))
    (setq torus-torus (append before current after)))
  (torus-switch-circle circle-name))

(defun torus-move-location (location-name)
  "Move current location after LOCATION-NAME."
  (interactive
   (list
    (completing-read
     "Move location after : "
     (mapcar #'torus--concise (cdr (car torus-torus))) nil t)))
  (let* ((circle (cdr (car torus-torus)))
         (index (1+ (position location-name circle
                              :test #'torus--equal-concise)))
         (current (list (car circle)))
         (before (subseq circle 1 index))
         (after (subseq circle index (length circle))))
    (setcdr (car torus-torus) (append before current after)))
  (torus-switch-location location-name))

(defun torus-move-to-circle (circle-name)
  "Move current location to CIRCLE-NAME."
  (interactive
   (list (completing-read
          "Move location to circle : "
          (mapcar #'car torus-torus) nil t)))
  (torus--update-position)
  (let* ((location (pop (cdr (car torus-torus))))
        (circle (cdr (assoc circle-name torus-torus)))
        (oldname (car (car torus-torus)))
        (oldpair (cons location oldname)))
    (setcdr (assoc circle-name torus-torus)
            (push location circle))
    (dolist (location-circle torus-index)
      (when (equal location-circle oldpair)
        (setcdr location-circle circle-name)))
    (dolist (location-circle torus-history)
      (when (equal location-circle oldpair)
        (setcdr location-circle circle-name))))
  (torus--jump))

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
           (oldname (car (car torus-torus)))
           (oldpair (cons location oldname)))
      (setcdr (assoc circle-name torus-torus)
              (push location circle))
      (dolist (location-circle torus-index)
        (when (equal location-circle oldpair)
          (setcdr location-circle circle-name)))
      (dolist (location-circle torus-history)
        (when (equal location-circle oldpair)
          (setcdr location-circle circle-name)))))
  (torus--jump)
  (torus-delete-current-circle)
  (torus-switch-circle circle-name))

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

(defun torus-reverse-circles ()
  "Reverse order of the circles."
  (interactive)
  (setq torus-torus (reverse torus-torus))
  (torus--jump))

(defun torus-reverse-locations ()
  "Reverse order of the locations in the current circles."
  (interactive)
  (setcdr (car torus-torus) (reverse (cdr (car torus-torus))))
  (torus--jump))

(defun torus-deep-reverse ()
  "Reverse order of the locations in each circle."
  (interactive)
  (setq torus-torus (reverse torus-torus))
  (dolist (circle torus-torus)
    (setcdr circle (reverse (cdr circle))))
  (torus--jump))

;;; Joining
;;; ------------------------------

(defun torus-join (torus-name)
  "Join current torus with TORUS-NAME in `torus-meta'."
  (interactive)
  )

;;; Deleting
;;; ------------

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
                              :test #'torus--equal-concise))
           (location (nth index circle)))
        (setcdr (car torus-torus) (delete location circle))
        (setq torus-index (torus--assoc-delete-all location torus-index))
        (setq torus-history (torus--assoc-delete-all location torus-history))
        (setq torus-markers (torus--assoc-delete-all location torus-markers))
        (torus--jump))
    (message "No location in current circle.")))

(defun torus-delete-current-circle ()
  "Delete current circle."
  (interactive)
  (torus-delete-circle (torus--concise (car (car torus-torus)))))

(defun torus-delete-current-location ()
  "Remove current location from current circle."
  (interactive)
  (torus-delete-location (torus--concise (car (cdr (car torus-torus))))))

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

(defun torus-split-horizontally ()
  "Split horizontally to view all buffers in current circle.
Split until `torus-maximum-horizontal-split' is reached.
Note: the current location in torus will be on the bottom."
  (interactive)

  (let* ((circle (cdr (car torus-torus)))
         (numsplit (1- (min (length circle) torus-maximum-horizontal-split))))
    (dolist (iter (number-sequence 1 numsplit))
      (split-window-below)
      (other-window 1)
      (torus-next-location)))
  (balance-windows)
  (other-window 1)
  (torus-next-location))

(defun torus-split-vertically ()
  "Split vertically to view all buffers in current circle.
Split until `torus-maximum-vertical-split' is reached.
Note: the current location in torus will be on the right."
  (interactive)
  (let* ((circle (cdr (car torus-torus)))
         (numsplit (1- (min (length circle) torus-maximum-vertical-split))))
    (dolist (i (number-sequence 1 numsplit))
      (message "i = %d" i)
      (split-window-right)
      (other-window 1)
      (torus-next-location)))
  (balance-windows)
  (other-window 1)
  (torus-next-location))

;;; File R/W
;;; ------------

(defun torus-write ()
  "Write torus to a file.
A \".torus\" extension is added if needed."
  (interactive)
  (torus--update-position)
  (setq torus-filename
        (read-file-name
         "Torus file : "
         (file-name-as-directory torus-dirname)))
  (let*
      ((file-basename (file-name-nondirectory torus-filename))
       (file-extension  ".torus")
       (minus-len-ext (- (length file-extension)))
       (buffer))
    (unless (member file-basename torus-input-history)
      (push file-basename torus-input-history))
    (unless (equal (subseq torus-filename minus-len-ext) file-extension)
      (setq torus-filename (concat torus-filename file-extension)))
    (setq buffer (find-file-noselect torus-filename))
    (with-current-buffer buffer
      (erase-buffer)
      (pp torus-torus buffer)
      (save-buffer)
      (kill-buffer))))

(defun torus-read ()
  "Read torus from a file. Replace the old Torus.
A \".torus\" extension is added if needed."
  (interactive)
  (setq torus-filename
        (read-file-name
         "Torus file : "
         (file-name-as-directory torus-dirname)))
  (let*
      ((file-basename (file-name-nondirectory torus-filename))
       (file-extension  ".torus")
       (minus-len-ext (- (length file-extension)))
       (buffer))
    (unless (member file-basename torus-input-history)
      (push file-basename torus-input-history))
    (unless (equal (subseq torus-filename minus-len-ext) file-extension)
      (setq torus-filename (concat torus-filename file-extension)))
    (if (file-exists-p torus-filename)
        (progn
          (setq buffer (find-file-noselect torus-filename))
          (setq torus-torus (read buffer))
          (kill-buffer buffer))
      (message "File %s does not exist." torus-filename)))
  (torus--build-index)
  (torus--jump))

(defun torus-prefix-circles (quoted-torus &optional quoted-history)
  "Add a prefix to circle names of QUOTED-TORUS and QUOTED-HISTORY.
Ask for a prefix to apply to the names of the circles of
QUOTED-TORUS.
An input history is available."
  (interactive)
  (let ((my-torus (symbol-value quoted-torus))
        (my-history (symbol-value quoted-history))
        (prefix)
        (prompt))
    (setq prompt
          (format torus--message-prefix-circle (symbol-name quoted-torus)))
    (setq prefix (read-string prompt nil 'torus-input-history))
    (delete-dups torus-input-history)
    (unless (or (= (length prefix) 0) (member prefix torus-input-history))
      (push prefix torus-input-history))
    (if (> (length prefix) 0)
        (progn
          (message "Prefix is %s" prefix)
          (dolist (elem my-torus)
            (setcar
             elem
             (concat prefix torus-prefix-separator (car elem))))
          (torus--build-index)
          (dolist (elem my-history)
            (setcdr
             elem
             (concat prefix torus-prefix-separator (cdr elem)))))
      (message "Prefix is blank"))))

(defun torus-prefix-circles-of-current-torus ()
  "Add a prefix to circle names of `torus-torus'."
  (interactive)
  (torus-prefix-circles 'torus-torus 'torus-history))

(defun torus-read-append ()
  "Read torus from a file and append it to the existing one.
Ask for a prefix to apply to the names of the existing circles,
then for another prefix to apply to the names of the added
circles.
An input history is available."
  (interactive)
  (torus--update-position)
  (setq torus-filename
        (read-file-name
         "Torus file : "
         (file-name-as-directory torus-dirname)))
  (let*
      ((file-basename (file-name-nondirectory torus-filename))
       (file-extension  ".torus")
       (minus-len-ext (- (length file-extension)))
       (buffer))
    (unless (member file-basename torus-input-history)
      (push file-basename torus-input-history))
    (unless (equal (subseq torus-filename minus-len-ext) file-extension)
      (setq torus-filename (concat torus-filename file-extension)))
    (if (file-exists-p torus-filename)
        (progn
          (setq buffer (find-file-noselect torus-filename))
          (setq torus-added (read buffer))
          (kill-buffer buffer)
          (torus-prefix-circles 'torus-torus)
          (torus-prefix-circles 'torus-added)
          (setq torus-torus (append torus-torus torus-added))
          (setq torus-torus
                (remove-duplicates
                 torus-torus
                 :test #'(lambda (a b)
                           (equal (car a) (car b))))))
      (message "File %s does not exist." torus-filename)))
  (torus--build-index)
  (torus--jump))

(defun torus-write-all ()
  "Write main torus variables to a file as Lisp code.
A \".el\" extension is added if needed."
  (interactive)
  (torus--update-position)
  (setq torus-filename
        (read-file-name
         "Torus file : "
         (file-name-as-directory torus-dirname)))
  (let*
      ((file-basename (file-name-nondirectory torus-filename))
       (file-extension  ".el")
       (minus-len-ext (- (length file-extension)))
       (buffer)
       (varlist '(torus-torus torus-index torus-history torus-input-history)))
    (unless (member file-basename torus-input-history)
      (push file-basename torus-input-history))
    (unless (equal (subseq torus-filename minus-len-ext) file-extension)
      (setq torus-filename (concat torus-filename file-extension)))
    (setq buffer (find-file-noselect torus-filename))
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
      (kill-buffer))))

(defun torus-read-all ()
  "Read main torus variables from a file as Lisp code."
  (interactive)
  (setq torus-filename
        (read-file-name
         "Torus file : "
         (file-name-as-directory torus-dirname)))
  (let*
      ((file-basename (file-name-nondirectory torus-filename))
       (file-extension  ".el")
       (minus-len-ext (- (length file-extension)))
       (buffer))
    (if (assoc file-basename torus-meta)
        (progn
          (message "Torus %s already exists in torus-meta" (file-name-nondirectory torus-filename))
          (torus-switch-torus (file-name-nondirectory torus-filename)))
      (unless (member file-basename torus-input-history)
        (push file-basename torus-input-history))
      (unless (equal (subseq torus-filename minus-len-ext) file-extension)
        (setq torus-filename (concat torus-filename file-extension)))
      (if (file-exists-p torus-filename)
          (progn
            (when (and torus-torus torus-history torus-input-history)
                  (torus-add-torus file-basename))
            (setq buffer (find-file-noselect torus-filename))
            (eval-buffer buffer)
            (kill-buffer buffer)
            ;; For the first torus added
            (unless torus-meta
              (torus-add-torus file-basename)))
        (message "File %s does not exist." torus-filename))))
  (torus--update-meta)
  ;; Also saved in file
  ;; (torus--build-index)
  (torus--jump))

(defun torus-read-append-all ()
  "Read main torus variables from a file and append it to the existing one."
  (interactive)
  (torus--update-position)
  (setq torus-filename
        (read-file-name
         "Torus file : "
         (file-name-as-directory torus-dirname)))
  (let*
      ((file-basename (file-name-nondirectory torus-filename))
       (oldtorus torus-torus)
       (oldhistory torus-history)
       (oldinput torus-input-history)
       (file-extension  ".el")
       (minus-len-ext (- (length file-extension)))
       (buffer))
    (unless (member file-basename torus-input-history)
      (push file-basename torus-input-history))
    (unless (equal (subseq torus-filename minus-len-ext) file-extension)
      (setq torus-filename (concat torus-filename file-extension)))
    (if (file-exists-p torus-filename)
        (progn
          (setq buffer (find-file-noselect torus-filename))
          (eval-buffer buffer)
          (kill-buffer buffer)
          (setq torus-added torus-torus)
          (setq torus-torus oldtorus)
          (setq torus-added-history torus-history)
          (setq torus-history oldhistory)
          (torus-prefix-circles 'torus-torus 'torus-history)
          (torus-prefix-circles 'torus-added 'torus-added-history)
          (setq torus-torus (append torus-torus torus-added))
          (setq torus-history (append torus-history torus-added-history))
          (setq torus-torus
                (remove-duplicates
                 torus-torus
                 :test #'(lambda (a b)
                           (equal (car a) (car b)))))
          (setq torus-history (append oldhistory torus-history))
          (setq torus-input-history (append oldinput torus-input-history)))
      (message "File %s does not exist." torus-filename)))
  ;; Rebuild with the added torus
  (torus--build-index)
  (torus--jump))

(defun torus-write-meta ()
  "Write `torus-meta' to a file as Lisp code.
A \"-meta.el\" extension is added if needed."
  (interactive)
  (torus--update-position)
  (setq torus-filename
        (read-file-name
         "Torus file : "
         (file-name-as-directory torus-dirname)))
  (let*
      ((file-basename (file-name-nondirectory torus-filename))
       (file-extension  "-meta.el")
       (minus-len-ext (- (length file-extension)))
       (buffer))
    (unless (member file-basename torus-input-history)
      (push file-basename torus-input-history))
    (unless (equal (subseq torus-filename minus-len-ext) file-extension)
      (setq torus-filename (concat torus-filename file-extension)))
    (torus--update-meta)
    (setq buffer (find-file-noselect torus-filename))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (concat
               "(setq "
               (symbol-name 'torus-meta)
               " (quote \n"))
      (pp torus-meta buffer)
      (insert "))\n\n")
      (save-buffer)
      (kill-buffer))))

(defun torus-read-meta ()
  "Read `torus-meta' from a file as Lisp code."
  (interactive)
  (setq torus-filename
        (read-file-name
         "Torus file : "
         (file-name-as-directory torus-dirname)))
  (let*
      ((file-basename (file-name-nondirectory torus-filename))
       (file-extension  "-meta.el")
       (minus-len-ext (- (length file-extension)))
       (buffer))
    (when (y-or-n-p "This will replace the current torus and torus-meta. Continue ? ")
      (unless (member file-basename torus-input-history)
        (push file-basename torus-input-history))
      (unless (equal (subseq torus-filename minus-len-ext) file-extension)
        (setq torus-filename (concat torus-filename file-extension)))
      (if (file-exists-p torus-filename)
          (progn
            (setq buffer (find-file-noselect torus-filename))
            (eval-buffer buffer)
            (kill-buffer buffer))
        (message "File %s does not exist." torus-filename))))
  (torus--update-from-meta)
  (torus--build-index)
  (torus--jump))

;;; End
;;; ------------------------------

(provide 'torus)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; torus.el ends here
