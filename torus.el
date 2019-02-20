;; -*- lisp -*-

;;; Torus : Personal version of MTorus, from scratch

;; TODO : almost everything

;; License
;; ------------------------------

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

;; Idea
;; ------------------------------
;;
;; A circle is a group of buffers
;; A torus is a group of circles, a kind of session if you will
;;
;; See README.org in the code repository for more details

;; Version
;; ------------------------------

(defvar torus/version "0.1"
  "Version number of torus.")

;; Custom group
;; ------------------------------

(defgroup torus nil

  "An interface to navigating groups of buffers.
This code should work fast so intuition is a very important
matter. Some of the customizable variables are switches to tune
the behavior of the functions to your habits. Hopefully it is
possible to find good settings for many people."

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

  "Filename where the last torus has been saved."

  :type 'string
  :group 'torus)

(defcustom torus/save-on-exit nil

  "*If set to t `torus/init' will install saving of torus on exit.
The function `torus/quit' is placed on `kill-emacs-hook'."

  :type 'boolean
  :group 'torus)

;; Variables
;; ------------------------------

(defvar torus/torus nil)

(defvar torus/markers nil)

(defvar torus/prefix-key (kbd "s-t"))

;; Keymap with prefix
;; ------------------------------

(define-prefix-command 'torus/map)

;; Functions
;; ------------------------------

(defun torus/quit ()

;; (torus/write)

  )

;; Commands : interactive functions
;; ------------------------------------------

(defun torus/install-default-bindings ()

  (interactive)

  (global-set-key torus/prefix-key 'torus/map)

  (define-key torus/map (kbd "i") 'torus/init)

  (define-key torus/map (kbd "p") 'torus/print)

  (define-key torus/map (kbd "c") 'torus/add-circle)
  (define-key torus/map (kbd "e") 'torus/add-element)

  (define-key torus/map (kbd "h") 'torus/previous-circle)
  (define-key torus/map (kbd "l") 'torus/next-circle)

  (define-key torus/map (kbd "j") 'torus/next-element)
  (define-key torus/map (kbd "k") 'torus/previous-element)

  (define-key torus/map (kbd "r") 'torus/read)
  (define-key torus/map (kbd "w") 'torus/write)

  (define-key torus/map (kbd "a") 'torus/read-append)

  )

(defun torus/init ()

  "Initialize torus
Add hooks"

  (interactive)

  (setq torus/torus nil)

  (setq torus/markers nil)

  (if torus/save-on-exit (add-hook 'kill-emacs-hook 'torus/quit))

  )

(defun torus/print ()

  (interactive)

  (message "--> Torus : ")

  (pp torus/torus)

  ;; (message "--> Markers : ")

  ;; (pp torus/markers)

  )

;; Adding
;; ------------

(defun torus/add-circle (name)

  "Add circle to torus"

  (interactive "sName for the new circle : ")

  (if (assoc name torus/torus)
      (message "Circle %s already exists in torus" name)
    (progn
      (message "Adding circle %s to torus" name)
      (push (list name) torus/torus)))

  )

(defun torus/add-element ()

  "Add current file and point to current circle"

  (interactive)

  (let*
      (
       (circle (car torus/torus))
       (pointmark (point-marker))
       (element (cons (buffer-file-name) (marker-position pointmark)))
       )
    (progn

      (if (member element (second circle))

	  (message "Element %s already exists in circle %s" element (car circle))

	(progn

	  (message "Adding %s to circle %s" element (car circle))

	  (if (> (length circle) 1)

	      (progn
		(setf (second circle) (append (list element) (second circle)))

		)

	    (progn
	      (setf circle (cons (car circle) (list (list element))))

	      )
	    )

	  (setf (car torus/torus) circle)

	  (push (cons element pointmark) torus/markers)

	  )
	)
      )
    )
  )

;; Moving
;; ------------

(defun torus/next-circle ()

  (interactive)

  (setf torus/torus (append (cdr torus/torus) (list (car torus/torus))))

  )

(defun torus/previous-circle ()

  (interactive)

  (setf torus/torus (append (last torus/torus) (butlast torus/torus)))

  )

(defun torus/next-element ()

  (interactive)

  (when (> (length (car torus/torus)) 1)
  (let
      (
       (circle (second (car torus/torus)))
       )

    (progn

      (setf circle (append (cdr circle) (list (car circle))))

      (setf (second (car torus/torus)) circle)

     )
    )
  )

  )

(defun torus/previous-element ()

  (interactive)

  (when (> (length (car torus/torus)) 1)
      (let
	  (
	   (circle (second (car torus/torus)))
	   )

	(progn

	  (setf circle (append (last circle) (butlast circle)))

	  (setf (second (car torus/torus)) circle)

	  )

	)
    )

  )

(defun torus/switch-circle ()

  "Change the current circle"

  (interactive)

  (let

      (
       (circle (completing-read "Go to circle : " torus/torus nil t))
       )

    (

     )
    )

  )

(defun torus/switch-element ()

  (interactive)

  (let

      (
       (element (completing-read "Go to element : " (second (car torus/torus)) nil t))
       )

    (

     )
    )

  )

;; File R/W
;; ------------

(defun torus/write ()

  "Write torus to a file"

  (interactive)

  (setq torus/filename (read-file-name "Torus file : " torus/dirname))

  (let
      (
       (buffer (find-file-noselect torus/filename))
       )

    (save-excursion

      (set-buffer buffer)
      (erase-buffer)

      (pp torus/torus buffer)

      (save-buffer)
      (kill-buffer)

      )
    )
  )

(defun torus/read ()

  "Read torus from a file
Replace the old Torus"

  (interactive)

  (setq torus/filename (read-file-name "Torus file : " torus/dirname))

    (let
      (
       (buffer (find-file-noselect torus/filename))
       )

    (save-excursion

      (set-buffer buffer)
      (setq torus/torus (read buffer))
      (kill-buffer)

      )
    )
  )

(defun torus/read-append ()

  "Read torus from a file
Append to the old Torus"

  (interactive)

  (setq torus/filename (read-file-name "Torus file : " torus/dirname))

    (let
      (
       (buffer (find-file-noselect torus/filename))
       (new-torus)
       )
      (progn
	(save-excursion

	  (set-buffer buffer)
	  (setq new-torus (read buffer))
	  (kill-buffer)

	  )
	(setf torus/torus (append torus/torus new-torus))
	)
    )

    )

;; ------------------------------

(provide 'torus)

;;; torus.el ends here
