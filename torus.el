;;; -*- lexical-binding: t; -*-

;;; torus.el --- A buffer groups manager

;; Copyright (C) 2019 Chimay

;; Author : Chimay <orduval@gmail.com>
;; Name: Torus
;; Package-Version: 1.3
;; Package-requires: ((emacs "26"))
;; Keywords: buffer, group, switch, save, split
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

(defvar torus-version "1.3"
  "Version number of torus."
  )

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

(defcustom torus-save-on-exit nil

  "Whether to ask to save torus on exit of Emacs.

If set to t `torus-init' will install saving of torus on exit.
The function `torus-quit' is placed on `kill-emacs-hook'."

  :type 'boolean
  :group 'torus)

(defcustom torus-history-maximum-elements 30

  "Maximum number of elements in `torus-history'"

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

;;; Variables
;;; ------------------------------

(defvar torus-torus nil
  "List of circles.
A circle is in the form :

\"name\" (list of (file . position))

Most recent entries are in the beginning of the lists.")

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

(defvar torus-prefix-key (kbd "s-t")
  "Prefix key for the torus key mappings.")

(defvar torus-prefix-separator " - "
  "String between the prefix and the circle names.

When a torus read from a file is append to the existing one,
the name of the new circles will be of the form :

user_input_prefix `torus-prefix-separator' name_of_the_added_circle

without the spaces. So, if you want some spacing between the
prefix and the circle name, just put it in
`torus-prefix-separator'.

If the user enter a blank prefix, the added circle names remain
untouched.")

(defvar torus--message-empty-torus "Torus is empty. You can use torus-add-circle to add a group to it.")
(defvar torus--message-empty-circle "No location found in circle %s. You can use torus-add-location to fill the circle.")
(defvar torus--message-existent-location "Location %s already exists in circle %s")
(defvar torus--message-print-choice "Print [t] torus [i] index [h] history [l] last [m] markers [n] input history")

;;; Keymap with prefix
;;; ------------------------------

(define-prefix-command 'torus-map)

;;; Compatibility
;;; ------------------------------

;; Torus needs assoc-delete-all, which is included in Emacs 27 or newer.
;; For older versions, here is a workaround :

(unless (fboundp 'assoc-delete-all)
  (defun torus--assoc-delete-all (element alist)
    "Remove all elements matching ELEMENT in ALIST."
    (cl-remove element alist :test 'equal :key 'car))
  (defalias assoc-delete-all torus--assoc-delete-all))

;;; Functions
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

If OBJECT is a string : nothing is done
\(File . Position) -> Buffer or File name at Position
\((File . Position) . Circle) -> Buffer or File name at Position : circle Circle"

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

(defun torus--update ()

  "Update position in current location.
Do nothing if file does not match current buffer."

  (if (and (car torus-torus) (> (length (car torus-torus)) 1))
      (let* ((location (car (cdr (car torus-torus))))
             (bookmark (assoc location torus-markers)))
        (when (equal (car location) (buffer-file-name (current-buffer)))
          (setcdr (car (cdr (car torus-torus))) (point))
          (if bookmark
              (setcdr (assoc location torus-markers) (point-marker))
            (push (cons location (point-marker)) torus-markers))))))

(defun torus--update-history ()

  "Add current location to `torus-history'"

  (if (and (car torus-torus) (> (length (car torus-torus)) 1))
      (let* ((circle (car torus-torus))
             (location (car (cdr circle)))
             (location-circle (cons location (car circle))))
        (push location-circle torus-history)
        (delete-dups torus-history)
        (when (> (length torus-history) torus-history-maximum-elements)
          (setq torus-history
                (subseq torus-history 0
                        torus-history-maximum-elements))))))

(defun torus--jump ()

  "Jump to current location (buffer & position) in torus.

Add the location to `torus-markers' if not already present."

  (if (and (car torus-torus) (> (length (car torus-torus)) 1))
      (let* ((location (car (cdr (car torus-torus))))
             (bookmark (cdr (assoc location torus-markers)))
             (buffer (when bookmark (marker-buffer bookmark))))
        (if (and bookmark buffer (buffer-live-p buffer))
            (progn
              (message "Found %s in torus-markers" bookmark)
              (switch-to-buffer buffer)
              (goto-char bookmark))
          (message "Found %s in torus" location)
          (setq torus-markers (assoc-delete-all location torus-markers))
          (pp torus-markers)
          (find-file (car location))
          (goto-char (cdr location))
          (push (cons location (point-marker)) torus-markers))
        (torus--update-history)
        (torus-info))))

(defun torus--switch (location-circle)

  "Jump to circle and location countained in LOCATION-CIRCLE."

  (cond
   ((equal current-prefix-arg '(4))
    (split-window-below)
    (other-window 1))
   ((equal current-prefix-arg '(16))
    (split-window-right)
    (other-window 1)))

  (torus--update)

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
          (push location-circle torus-index))))))

(defun torus--quit ()

  "Write torus before quit."

  (when (and
         torus-torus
         (y-or-n-p "Write torus ? "))
    (torus-write)))

;;; Commands
;;; ------------------------------

(defun torus-install-default-bindings ()

  "Install default keybindings."

  (interactive)

  (global-set-key torus-prefix-key 'torus-map)

  (define-key torus-map (kbd "z") 'torus-zero)
  (define-key torus-map (kbd "p") 'torus-print)
  (define-key torus-map (kbd "i") 'torus-info)
  (define-key torus-map (kbd "c") 'torus-add-circle)
  (define-key torus-map (kbd "e") 'torus-add-location)
  (define-key torus-map (kbd "m") 'torus-rename-circle)
  (define-key torus-map (kbd "d") 'torus-delete-location)
  (define-key torus-map (kbd "x") 'torus-delete-circle)
  (define-key torus-map (kbd "D") 'torus-delete-current-location)
  (define-key torus-map (kbd "X") 'torus-delete-current-circle)
  (define-key torus-map (kbd "<left>") 'torus-previous-circle)
  (define-key torus-map (kbd "<right>") 'torus-next-circle)
  (define-key torus-map (kbd "<up>") 'torus-previous-location)
  (define-key torus-map (kbd "<down>") 'torus-next-location)
  (define-key torus-map (kbd "SPC") 'torus-switch-circle)
  (define-key torus-map (kbd "=") 'torus-switch-location)
  (define-key torus-map (kbd "s") 'torus-search)
  (define-key torus-map (kbd "h") 'torus-history-older)
  (define-key torus-map (kbd "l") 'torus-history-newer)
  (define-key torus-map (kbd "^") 'torus-alternate)
  (define-key torus-map (kbd "/") 'torus-search-history)
  (define-key torus-map (kbd "_") 'torus-split-horizontally)
  (define-key torus-map (kbd "|") 'torus-split-vertically)
  (define-key torus-map (kbd "r") 'torus-read)
  (define-key torus-map (kbd "w") 'torus-write)
  (define-key torus-map (kbd "f") 'torus-prefix-circles-of-current-torus)
  (define-key torus-map (kbd "a") 'torus-read-append))

(defun torus-zero ()

  "Reset main variables."

  (interactive)
  (message "Main variables -> nil")
  (setq torus-torus nil)
  (setq torus-index nil)
  (setq torus-history nil)
  (setq torus-last nil)
  (setq torus-markers nil)
  (setq torus-input-history nil)
  (setq torus-added nil)
  (setq torus-filename nil))

(defun torus-init ()

  "Initialize torus, add hooks."

  (interactive)
  (torus-zero)
  (if torus-save-on-exit (add-hook 'kill-emacs-hook 'torus--quit))
  (unless (file-exists-p torus-dirname) (make-directory torus-dirname))

  )

;;; Printing
;;; ------------

(defun torus-print ()

  "Print torus and markers in opened files."

  (interactive)

  (let ((choice
         (read-key torus--message-print-choice)))
    (view-echo-area-messages)
    (cond ((equal choice ?t)
           (pp torus-torus))
          ((equal choice ?i)
           (pp torus-index))
          ((equal choice ?h)
           (pp torus-history))
          ((equal choice ?l)
           (pp torus-last))
          ((equal choice ?m)
           (pp torus-markers))
          ((equal choice ?n)
           (pp torus-input-history))
          (t
           (message "Invalid key.")))))

(defun torus-info ()

  "Print local info : circle name and locations."

  (interactive)

  (if torus-torus
      (if (> (length (car torus-torus)) 1)
          (let* ((circle (car torus-torus))
                 (prettylist
                  (mapcar
                   #'(lambda (el)
                       (cons
                        (torus--buffer-or-filename el)
                        (cdr el)))
                   (cdr circle))))
            (message "%s : %s" (car circle) prettylist))
        (message torus--message-empty-circle (car (car torus-torus))))
    (message torus--message-empty-torus)))

;;; Adding
;;; ------------

(defun torus-add-circle (name)

  "Add circle named NAME to torus."

  (interactive "sName for the new circle : ")

  (if (assoc name torus-torus)
      (message "Circle %s already exists in torus" name)
    (message "Adding circle %s to torus" name)
    (push (list name) torus-torus)))

(defun torus-add-location ()

  "Add current file and point to current circle."

  (interactive)

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
      (unless (member location-marker torus-markers)
        (push location-marker torus-markers)))))

;;; Renaming
;;; ------------

(defun torus-rename-circle (name)

  "Rename current circle as NAME."

  (interactive "sNew name for the circle : ")
  (setcar (car torus-torus) name))

;;; Deleting
;;; ------------

(defun torus-delete-circle (circle-name)

  "Delete circle given by CIRCLE-NAME."

  (interactive
   (list
    (completing-read "Delete circle : "
                     (mapcar #'car torus-torus) nil t)))

  (when (y-or-n-p (format "Delete circle %s ? " circle-name))
      (setq torus-torus (assoc-delete-all circle-name torus-torus))
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
        (setq torus-index (assoc-delete-all location torus-index))
        (setq torus-markers (assoc-delete-all location torus-markers))
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

;;; Moving
;;; ------------

(defun torus-previous-circle ()

  "Jump to the previous circle."

  (interactive)

  (if torus-torus
      (if (> (length (car torus-torus)) 1)
          (progn
            (torus--update)
            (setf torus-torus (append (last torus-torus) (butlast torus-torus)))
            (torus--jump))
        (message torus--message-empty-circle (car (car torus-torus))))
    (message torus--message-empty-torus)))

(defun torus-next-circle ()

  "Jump to the next circle."

  (interactive)

  (if torus-torus
      (if (> (length (car torus-torus)) 1)
          (progn
            (torus--update)
            (setf torus-torus (append (cdr torus-torus) (list (car torus-torus))))
            (torus--jump))
        (message torus--message-empty-circle (car (car torus-torus))))
    (message torus--message-empty-torus)))

(defun torus-previous-location ()

  "Jump to the previous location."

  (interactive)

  (if torus-torus
      (if (> (length (car torus-torus)) 1)
          (let ((circle (cdr (car torus-torus))))
            (torus--update)
            (setf circle (append (last circle) (butlast circle)))
            (setcdr (car torus-torus) circle)
            (torus--jump))
        (message torus--message-empty-circle (car (car torus-torus))))
    (message torus--message-empty-torus)))

(defun torus-next-location ()

  "Jump to the next location."

  (interactive)

  (if torus-torus
  (if (> (length (car torus-torus)) 1)
      (let ((circle (cdr (car torus-torus))))
        (torus--update)
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

  (cond
   ((equal current-prefix-arg '(4))
    (split-window-below)
    (other-window 1))
   ((equal current-prefix-arg '(16))
    (split-window-right)
    (other-window 1)))

  (torus--update)

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

  (cond
   ((equal current-prefix-arg '(4))
    (split-window-below)
    (other-window 1))
   ((equal current-prefix-arg '(16))
    (split-window-right)
    (other-window 1)))

  (torus--update)

  (let* ((circle (cdr (car torus-torus)))
         (index (position location-name circle
                          :test #'torus--equal-concise))
         (before (subseq circle 0 index))
         (after (subseq circle index (length circle))))
    (setcdr (car torus-torus) (append after before)))

  (torus--jump))

;;; Searching
;;; ------------

(defun torus-search (location-name)

  "Search LOCATION-NAME in the torus.
Go to the first matching circle and switch to the file."

  (interactive
   (list
    (completing-read
     "Search location : "
     (mapcar #'torus--concise torus-index) nil t)))

  (let* ((location-circle
          (find
           location-name torus-index
           :test #'torus--equal-concise))
         (location (car location-circle))
         (circle (cdr location-circle)))
    (torus--switch location-circle)))

;;; History
;;; ------------

(defun torus-history-newer ()

  (interactive)

  (when torus-history
    (setq torus-history (append (last torus-history) (butlast torus-history)))
    (torus--switch (car torus-history))))

(defun torus-history-older ()

  (interactive)

  (when torus-history
    (setq torus-history (append (cdr torus-history) (list (car torus-history))))
    (torus--switch (car torus-history))))

(defun torus-search-history (location-name)

  (interactive
   (list
    (completing-read
     "Search location : "
     (mapcar #'torus--concise torus-history) nil t)))

  (when torus-history
    (let* ((index (position location-name torus-history
                            :test #'torus--equal-concise))
           (before (subseq torus-history 0 index))
           (element (nth index torus-history))
           (after (subseq torus-history (1+ index) (length torus-history))))
      (setq torus-history (append (list element) before after)))
    (torus--switch (car torus-history))))

(defun torus-alternate ()

  (interactive)

  (when (and torus-history (>= (length torus-history) 2))
    (setq torus-history (append
                         (list (car (cdr torus-history)))
                         (list (car torus-history))
                         (nthcdr 2 torus-history)))
    (torus--switch (car torus-history))))

;;; Splitting
;;; ------------

(defun torus-split-horizontally ()

  "Split horizontally to view all buffers in current circle.

Split until `torus-max-horizontal-split' is reached.
Note: the current location in torus will be on the bottom."

  (interactive)

  (let* ((circle (cdr (car torus-torus)))
         (numsplit (1- (min (length circle) torus-max-horizontal-split))))
    (dolist (i (number-sequence 1 numsplit))
      (message "i = %d" i)
      (split-window-below)
      (other-window 1)
      (torus-next-location)))
  (balance-windows)
  (other-window 1))

(defun torus-split-vertically ()

  "Split vertically to view all buffers in current circle.

Split until `torus-max-vertical-split' is reached.
Note: the current location in torus will be on the right."

  (interactive)

  (let* ((circle (cdr (car torus-torus)))
         (numsplit (1- (min (length circle) torus-max-vertical-split))))
    (dolist (i (number-sequence 1 numsplit))
      (message "i = %d" i)
      (split-window-right)
      (other-window 1)
      (torus-next-location)))
  (balance-windows)
  (other-window 1))

;;; File R/W
;;; ------------

(defun torus-write ()

  "Write torus to a file."

  (interactive)

  (torus--update)
  (setq torus-filename (read-file-name "Torus file : " torus-dirname))

  (let
      ((file-prefix (file-name-nondirectory torus-filename))
       (buffer (find-file-noselect torus-filename)))
    (unless (member file-prefix torus-input-history)
      (push file-prefix torus-input-history))
    (with-current-buffer buffer
      (erase-buffer)
      (pp torus-torus buffer)
      (save-buffer)
      (kill-buffer))))

(defun torus-read ()

  "Read torus from a file.
Replace the old Torus."

  (interactive)

  (setq torus-filename (read-file-name "Torus file : " torus-dirname))

  (let ((file-prefix (file-name-nondirectory torus-filename))
        (buffer (find-file-noselect torus-filename)))
    (unless (member file-prefix torus-input-history)
      (push file-prefix torus-input-history))
    (with-current-buffer buffer
      (setq torus-torus (read buffer))
      (kill-buffer)))

  (torus--build-index)
  (torus--jump))

(defun torus-prefix-circles (quoted-torus)

  "Add a prefix to circle names of QUOTED-TORUS.

Ask for a prefix to apply to the names of the circles of
QUOTED-TORUS.

A prefix history is available."

  (interactive)

  (let ((my-torus (symbol-value quoted-torus))
        (prefix)
        (prompt))
    (setq prompt
          (format "Prefix for the circle names of %s (leave blank for none) ? "
                  (symbol-name quoted-torus)))
    (setq prefix (read-string prompt nil 'torus-input-history))
    (delete-dups torus-input-history)
    (unless (or (= (length prefix) 0) (member prefix torus-input-history))
      (push prefix torus-input-history))
    (if (> (length prefix) 0)
        (progn
          (message "Prefix is %s" prefix)
          (mapcar
           #'(lambda (el)
               (append
                (list (concat prefix torus-prefix-separator (car el)))
                (cdr el)))
           my-torus))
      (message "Prefix is blank")
      my-torus)))

(defun torus-prefix-circles-of-current-torus ()

  "Add a prefix to circle names of `torus-torus'."

  (interactive)

  (setq torus-torus (torus-prefix-circles 'torus-torus)))

(defun torus-read-append ()

  "Read torus from a file and append it to the existing one.

Ask for a prefix to apply to the names of the existing circles,
then for another prefix to apply to the names of the added
circles.

A prefix history is available."

  (interactive)

  (torus--update)
  (setq torus-filename (read-file-name "Torus file : " torus-dirname))

  (let ((file-prefix (file-name-nondirectory torus-filename))
        (buffer (find-file-noselect torus-filename)))
    (unless (member file-prefix torus-input-history)
      (push file-prefix torus-input-history))
    (with-current-buffer buffer
      (setq torus-added (read buffer))
      (kill-buffer))
    (setq torus-torus (torus-prefix-circles 'torus-torus))
    (setq torus-added (torus-prefix-circles 'torus-added))
    (setq torus-torus (append torus-torus torus-added))
    (setq torus-torus
          (remove-duplicates torus-torus
                             :test #'(lambda (a b)
                                       (equal (car a) (car b))))))

  (torus--build-index)
  (torus--jump))

;; ------------------------------

(provide 'torus)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; torus.el ends here
