;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; torus.el --- Torus : Manage Groups of Buffers in Emacs
;;; ------------------------------------------------------------

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

;;; Commentary:
;;; ------------------------------

;; A circle is a group of buffers
;; A torus is a group of circles, a kind of session if you will
;;
;; See README.org in the code repository for more details

;;; Code:
;;; ------------------------------------------------------------

;;; Version
;;; ------------------------------

(defvar torus/version "1.0"

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
  :prefix "torus/"
  :group 'environment
  :group 'extensions
  :group 'convenience)

(defcustom torus/dirname "~/.emacs.d/"

  "The directory where the torus are read and written."

  :type 'string
  :group 'torus)

(defcustom torus/filename "torus"

  "Filename where the last torus has been saved or read."

  :type 'string
  :group 'torus)

(defcustom torus/save-on-exit nil

  "Whether to ask to save torus on exit of Emacs.

If set to t `torus/init' will install saving of torus on exit.
The function `torus/quit' is placed on `kill-emacs-hook'."

  :type 'boolean
  :group 'torus)

;;; Variables
;;; ------------------------------

(defvar torus/torus nil

  "List of circles.

A circle is in the form :

\"name\" (lists of (file . position))

Most recent entries are in the beginning of the lists.")

(defvar torus/markers nil

  "Alist containing markers to opened files.

It is of the form :
\((file . position) . marker)
Contain only the files opened in buffers.")

(defvar torus/added nil

  "Last torus added from a file.")

(defvar torus/input-history nil

  "History of user input.")

(defvar torus/prefix-key (kbd "s-t")

  "Prefix key for the torus key mappings.")

(defvar torus/prefix-separator " - "

  "String between the prefix and the circle names.

When a torus read from a file is append to the existing one,
the name of the new circles will be of the form :

user_input_prefix torus/prefix-separator name_of_the_added_circle

without the spaces. So, if you want some spacing between the
prefix and the circle name, just put it in
torus/prefix-separator.

If the user enter a blank prefix, the added circle names remain
untouched.")

;;; Keymap with prefix
;;; ------------------------------

(define-prefix-command 'torus/map)

;;; Functions
;;; ------------------------------

(defun torus/update ()

  "Update position in current element if file matches current buffer."

  (if (> (length (car torus/torus)) 1)

      (let* ((element (car (cdr (car torus/torus))))
             (bookmark (assoc element torus/markers)))

        (progn
          (when (equal (car element) (buffer-file-name (current-buffer)))
            (progn
              (setf (cdr (car (cdr (car torus/torus)))) (point))
              (if bookmark
                  (setcdr (assoc element torus/markers) (point-marker))
                (push (cons element (point-marker)) torus/markers))))))
    (message "No element found in circle %s" (car (car torus/torus)))))

(defun torus/jump ()

  "Jump to current element (buffer & position) in torus."

  (if (> (length (car torus/torus)) 1)
      (let* ((element (car (cdr (car torus/torus))))
             (pointmark (cdr (assoc element torus/markers)))
             (bufmark (when pointmark (marker-buffer pointmark))))
        (progn
          (if (and pointmark bufmark (buffer-live-p bufmark))
              (progn
                (message "Found %s in torus/markers" pointmark)
                (switch-to-buffer bufmark)
                (goto-char pointmark))
              (message "Found %s in torus" element)
              (setq torus/markers (assoc-delete-all element torus/markers))
              (pp torus/markers)
              (find-file (car element))
              (goto-char (cdr element))
              (push (cons element (point-marker)) torus/markers))))
    (message "No element found in circle %s" (car (car torus/torus)))))

(defun torus/quit ()

  "Write torus before quit."

  (when (and
         torus/torus
         (y-or-n-p "Write torus ? "))
    (torus/write)))

;;; Commands : interactive functions
;;; ------------------------------------------

(defun torus/install-default-bindings ()

  "Install default keybindings."

  (interactive)

  (global-set-key torus/prefix-key 'torus/map)

  (define-key torus/map (kbd "i") 'torus/init)
  (define-key torus/map (kbd "z") 'torus/zero)
  (define-key torus/map (kbd "P") 'torus/print)
  (define-key torus/map (kbd "p") 'torus/print-circle)
  (define-key torus/map (kbd "c") 'torus/add-circle)
  (define-key torus/map (kbd "e") 'torus/add-element)
  (define-key torus/map (kbd "m") 'torus/rename-circle)
  (define-key torus/map (kbd "d") 'torus/delete-element)
  (define-key torus/map (kbd "x") 'torus/delete-circle)
  (define-key torus/map (kbd "D") 'torus/delete-current-element)
  (define-key torus/map (kbd "X") 'torus/delete-current-circle)
  (define-key torus/map (kbd "h") 'torus/previous-circle)
  (define-key torus/map (kbd "l") 'torus/next-circle)
  (define-key torus/map (kbd "j") 'torus/next-element)
  (define-key torus/map (kbd "k") 'torus/previous-element)
  (define-key torus/map (kbd "<left>") 'torus/previous-circle)
  (define-key torus/map (kbd "<right>") 'torus/next-circle)
  (define-key torus/map (kbd "<up>") 'torus/previous-element)
  (define-key torus/map (kbd "<down>") 'torus/next-element)
  (define-key torus/map (kbd "SPC") 'torus/switch-circle)
  (define-key torus/map (kbd "=") 'torus/switch-element)
  (define-key torus/map (kbd "r") 'torus/read)
  (define-key torus/map (kbd "w") 'torus/write)
  (define-key torus/map (kbd "f")
    '(lambda () (interactive) (setq torus/torus (torus/prefix-circles 'torus/torus))))
  (define-key torus/map (kbd "a") 'torus/read-append)

  )

(defun torus/zero ()

  "Reset main variables."

  (interactive)
  (message "Torus, Markers -> nil")
  (setq torus/torus nil)
  (setq torus/markers nil)

  )

(defun torus/init ()

  "Initialize torus, add hooks."

  (interactive)
  (torus/zero)
  (if torus/save-on-exit (add-hook 'kill-emacs-hook 'torus/quit))
  (unless (file-exists-p torus/dirname) (make-directory torus/dirname))

  )

(defun torus/print ()

  "Print torus and markers in opened files."

  (interactive)
  (message "--> Torus :\n")
  (pp torus/torus)
  (message "--> Markers :\n")
  (pp torus/markers)

  )

(defun torus/print-circle ()

  "Print circle name and elements."

  (interactive)

  (let* ((circle (car torus/torus))
	 (prettylist
	  (mapcar
	   #'(lambda (el)
	       (cons
		(file-name-nondirectory (car el))
		(cdr el)))
         (cdr circle))))
      (message "%s : %s" (car circle) prettylist)))

;;; Adding
;;; ------------

(defun torus/add-circle (name)

  "Add circle named NAME to torus."

  (interactive "sName for the new circle : ")

  (if (assoc name torus/torus)
      (message "Circle %s already exists in torus" name)
    (message "Adding circle %s to torus" name)
    (push (list name) torus/torus)))

(defun torus/add-element ()

  "Add current file and point to current circle."

  (interactive)

  (let* ((circle (car torus/torus))
	 (pointmark (point-marker))
	 (element (cons (buffer-file-name) (marker-position pointmark)))
	 (element-marker (cons element pointmark))
	 (element-buffer (cons element (current-buffer))))
    (progn
      (if (member element (cdr circle))
          (message "Element %s already exists in circle %s" element (car circle))
        (message "Adding %s to circle %s" element (car circle))
        (if (> (length circle) 1)
            (setf (cdr circle) (append (list element) (cdr circle)))
          (setf circle (append circle (list element))))
        (setf (car torus/torus) circle)
        (when (not (member element-marker torus/markers))
          (push element-marker torus/markers))))))

;;; Renaming
;;; ------------

(defun torus/rename-circle (name)

  "Rename current circle as NAME."

  (interactive "sNew name for the circle : ")
  (setcar (car torus/torus) name))

;;; Deleting
;;; ------------

(defun torus/delete-circle (circle-name)

  "Delete circle given by CIRCLE-NAME."

  (interactive
   (list
    (completing-read "Delete circle : "
                     (mapcar #'car torus/torus) nil t)))

  (when (y-or-n-p (format "Delete circle %s ? " circle-name))
      (setq torus/torus (assoc-delete-all circle-name torus/torus))
      (torus/jump)))

(defun torus/delete-element (element-name)

  "Delete element given by ELEMENT-NAME."

  (interactive
   (list
    (completing-read
     "Delete element : "
     (mapcar #'prin1-to-string (cdr (car torus/torus))) nil t)))

  (if (and
       (> (length (car torus/torus)) 1)
       (y-or-n-p
        (format
         "Delete %s from circle %s ? "
         element-name
         (car (car torus/torus)))))

      (let* ((circle (cdr (car torus/torus)))
             (index (position
                     element-name circle
                     :test
                     #'(lambda (a b) (or (equal (prin1-to-string a) b)
                                    (equal a (prin1-to-string b))))))
           (element (nth index circle)))
        (progn
          (setf (cdr (car torus/torus )) (delete element circle))
          (setq torus/markers (assoc-delete-all element torus/markers))
          (torus/jump)))

    (message "No element in current circle")))

(defun torus/delete-current-circle ()

  "Delete current circle."

  (interactive)
  (torus/delete-circle (car (car torus/torus))))

(defun torus/delete-current-element ()

  "Remove current element from current circle."

  (interactive)
  (torus/delete-element (car (cdr (car torus/torus)))))

;;; Moving
;;; ------------

(defun torus/previous-circle ()

  "Jump to the previous circle."

  (interactive)

  (torus/update)
  (setf torus/torus (append (cdr torus/torus) (list (car torus/torus))))
  (torus/jump))

(defun torus/next-circle ()

  "Jump to the next circle."

  (interactive)

  (torus/update)
  (setf torus/torus (append (last torus/torus) (butlast torus/torus)))
  (torus/jump))

(defun torus/previous-element ()

  "Jump to the previous element."

  (interactive)

  (if (> (length (car torus/torus)) 1)
      (let ((circle (cdr (car torus/torus))))
        (progn
          (torus/update)
          (setf circle (append (cdr circle) (list (car circle))))
          (setf (cdr (car torus/torus)) circle)
          (torus/jump)))
    (message "No element found in circle %s" (car (car torus/torus)))))

(defun torus/next-element ()

  "Jump to the next element."

  (interactive)

  (if (> (length (car torus/torus)) 1)
      (let ((circle (cdr (car torus/torus))))
        (progn
          (torus/update)
          (setf circle (append (last circle) (butlast circle)))
          (setf (cdr (car torus/torus)) circle)
          (torus/jump)))
    (message "No element found in circle %s" (car (car torus/torus)))))

(defun torus/switch-circle (circle-name)

  "Jump to CIRCLE-NAME circle."

  (interactive
   (list (completing-read
          "Go to circle : "
          (mapcar #'car torus/torus) nil t)))

  (torus/update)

  (let* ((circle (assoc circle-name torus/torus))
       (index (position circle torus/torus :test #'equal))
       (before (subseq torus/torus 0 index))
       (after (subseq torus/torus index (length torus/torus))))
    (setq torus/torus (append after before)))

  (torus/jump))

(defun torus/switch-element (element-name)

  "Jump to ELEMENT-NAME element."

  (interactive
   (list
    (completing-read
     "Go to element : "
     (mapcar #'prin1-to-string (cdr (car torus/torus))) nil t)))

  (torus/update)

  (let* ((circle (cdr (car torus/torus)))
         (index (position
                 element-name circle
                 :test
                 #'(lambda (a b) (or (equal (prin1-to-string a) b)
                                (equal a (prin1-to-string b))))))
	 (before (subseq circle 0 index))
	 (after (subseq circle index (length circle))))
    (setf (cdr (car torus/torus)) (append after before)))

  (torus/jump))

;;; File R/W
;;; ------------

(defun torus/write ()

  "Write torus to a file."

  (interactive)

  (torus/update)
  (setq torus/filename (read-file-name "Torus file : " torus/dirname))

  (let
      ((file-prefix (file-name-nondirectory torus/filename))
       (buffer (find-file-noselect torus/filename)))
    (progn
      (unless (member file-prefix torus/input-history)
        (push file-prefix torus/input-history))
      (with-current-buffer buffer
        (erase-buffer)
        (pp torus/torus buffer)
        (save-buffer)
        (kill-buffer)))))

(defun torus/read ()

  "Read torus from a file.
Replace the old Torus."

  (interactive)

  (setq torus/filename (read-file-name "Torus file : " torus/dirname))

  (let ((file-prefix (file-name-nondirectory torus/filename))
	(buffer (find-file-noselect torus/filename)))
    (progn
      (unless (member file-prefix torus/input-history)
        (push file-prefix torus/input-history))
      (with-current-buffer buffer
        (setq torus/torus (read buffer))
        (kill-buffer))))

  (torus/jump))

(defun torus/prefix-circles (torus-symbol)

  "Add a prefix to circle names of TORUS-SYMBOL.

Ask for a prefix to apply to the names of the circles of
torus-symbol.

A prefix history is available."

  (interactive)

  (let ((my-torus (symbol-value torus-symbol))
       (prefix)
       (prompt))
    (progn
      (setq prompt
            (format "Prefix for the circle names of %s (leave blank for none) ? "
                    (symbol-name torus-symbol)))
      (setq prefix (read-string prompt nil 'torus/input-history))
      (unless (member prefix torus/input-history)
        (push prefix torus/input-history))
      (if (> (length prefix) 0)
        (progn
          (message "Prefix is %s" prefix)
          (mapcar
           #'(lambda (el)
               (append
                (list (concat prefix torus/prefix-separator (car el)))
                (cdr el)))
           my-torus))
        (message "Prefix is blank")
        my-torus))))

(defun torus/read-append ()

  "Read torus from a file and append it to the existing one.

Ask for a prefix to apply to the names of the existing circles,
then for another prefix to apply to the names of the added
circles.

A prefix history is available."

  (interactive)

  (torus/update)
  (setq torus/filename (read-file-name "Torus file : " torus/dirname))

  (let ((file-prefix (file-name-nondirectory torus/filename))
       (buffer (find-file-noselect torus/filename)))
    (progn
      (unless (member file-prefix torus/input-history)
        (push file-prefix torus/input-history))
      (with-current-buffer buffer
        (setq torus/added (read buffer))
        (kill-buffer))
      (setq torus/torus (torus/prefix-circles 'torus/torus))
      (setq torus/added (torus/prefix-circles 'torus/added))
      (setq torus/torus (append torus/torus torus/added))
      (setq torus/torus
	    (remove-duplicates
             torus/torus
             :test
             #'(lambda (a b)
		 (equal (car a) (car b)))))))

  (torus/jump))

;; ------------------------------

(provide 'torus)

;;; torus.el ends here
