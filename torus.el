;; -*- mode: elisp; -*-

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

  "Version number of torus."

  )

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

(defvar torus/torus nil

  "Circle of circles of buffers
Most recent entries are in the beginning of the lists"

  )

(defvar torus/markers nil)

(defvar torus/buffers nil)

(defvar torus/prefix-key (kbd "s-t"))

;; Keymap with prefix
;; ------------------------------

(define-prefix-command 'torus/map)

;; Functions
;; ------------------------------

(defun torus/update ()

"Update position in current element if buffer matches current buffer"

(if (> (length (car torus/torus)) 1)

    (let*
	(
	 (element (car (cdr (car torus/torus))))
	 (bookmark (assoc element torus/markers))
	 (buffer (assoc element torus/buffers))
	 )

      (progn

	(when (equal (car element) (buffer-file-name (current-buffer)))

	  (progn

	    (setf (cdr (car (cdr (car torus/torus)))) (point))

	    (if bookmark

		(setcdr (assoc element torus/markers) (point-marker))

	      (push (cons element (point-marker)) torus/markers)

	      )

	    (if buffer

		(setcdr (assoc element torus/buffers) (current-buffer))

	      (push (cons element (current-buffer)) torus/buffers)

	      )
	    )

	  )
	)
      )

  (message "No element found in circle %s" (car (car torus/torus)))

  )

)

(defun torus/jump ()

"Jump to current element (buffer & position) in torus"

(if (> (length (car torus/torus)) 1)

    (let* (
	   (element (car (cdr (car torus/torus))))
	   (pointmark (cdr (assoc element torus/markers)))
	   (bufmark (when pointmark (marker-buffer pointmark)))
	   (buffer (cdr (assoc element torus/buffers)))
	  )

      (progn

	    (if (and pointmark bufmark (buffer-live-p bufmark))

		(progn

		  (message "Found %s in torus/markers\n" pointmark)

		  (switch-to-buffer bufmark)
		  (goto-char pointmark)

		  )

	      (if (and buffer (buffer-live-p buffer))

		  (progn

		    (message "Found %s in torus/buffers\n" buffer)

		    (setq torus/markers (assoc-delete-all element torus/markers))

		    (switch-to-buffer buffer)
		    (goto-char (cdr element))

		    (push (cons element (point-marker)) torus/markers)

		    )

		(progn

		  (message "Found %s in torus\n" element)

		  (setq torus/markers (assoc-delete-all element torus/markers))
		  (setq torus/buffers (assoc-delete-all element torus/buffers))

		  (pp torus/markers)

		  (find-file (car element))
		  (goto-char (cdr element))

		  (push (cons element (point-marker)) torus/markers)
		  (push (cons element (current-buffer)) torus/buffers)

		  )
		)
	      )
	  )
	)

   (message "No element found in circle %s" (car (car torus/torus)))

   )

)

(defun torus/quit ()

  "Write torus before quit"

  (torus/write)

  )

;; Commands : interactive functions
;; ------------------------------------------

(defun torus/install-default-bindings ()

  (interactive)

  (global-set-key torus/prefix-key 'torus/map)

  (define-key torus/map (kbd "i") 'torus/init)
  (define-key torus/map (kbd "z") 'torus/zero)

  (define-key torus/map (kbd "p") 'torus/print)

  (define-key torus/map (kbd "c") 'torus/add-circle)
  (define-key torus/map (kbd "e") 'torus/add-element)

  (define-key torus/map (kbd "m") 'torus/rename-circle)

  (define-key torus/map (kbd "X") 'torus/delete-element)
  (define-key torus/map (kbd "D") 'torus/delete-circle)

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

  (define-key torus/map (kbd "a") 'torus/read-append)

  )

(defun torus/zero ()

  (interactive)

  (setq torus/torus nil)

  (setq torus/markers nil)

  (setq torus/buffers nil)

  )

(defun torus/init ()

  "Initialize torus
Add hooks"

  (interactive)

  (torus/zero)

  (if torus/save-on-exit (add-hook 'kill-emacs-hook 'torus/quit))

  )

(defun torus/print ()

  (interactive)

  (message "--> Torus :\n")

  (pp torus/torus)

  (message "--> Markers :\n")

  (pp torus/markers)

  (message "--> Buffers :\n")

  (pp torus/buffers)

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
       (element-marker (cons element pointmark))
       (element-buffer (cons element (current-buffer)))
       )
    (progn

      (if (member element (cdr circle))

	  (message "Element %s already exists in circle %s" element (car circle))

	(progn

	  (message "Adding %s to circle %s" element (car circle))

	  (if (> (length circle) 1)

	      (progn

		(setf (cdr circle) (append (list element) (cdr circle)))

		)

	    (progn

	      (setf circle (append circle (list element)))

	      )
	    )

	  (setf (car torus/torus) circle)

	  (when (not (member element-marker torus/markers))
	    (push element-marker torus/markers))

	  (when (not (member element-buffer torus/buffers))
	    (push element-buffer torus/buffers))

	  )
	)
      )
    )
  )

;; Renaming
;; ------------

(defun torus/rename-circle ()

  (interactive)



  )

;; Deleting
;; ------------

(defun torus/delete-element ()

  (interactive)



  )

(defun torus/delete-circle ()

  (interactive)



  )

;; Moving
;; ------------

(defun torus/previous-circle ()

  (interactive)

  (torus/update)

  (setf torus/torus (append (cdr torus/torus) (list (car torus/torus))))

  (torus/jump)

  )

(defun torus/next-circle ()

  (interactive)

  (torus/update)

  (setf torus/torus (append (last torus/torus) (butlast torus/torus)))

  (torus/jump)

  )

(defun torus/previous-element ()

  (interactive)

  (if (> (length (car torus/torus)) 1)
      (let
	  (
	   (circle (cdr (car torus/torus)))
	   )

	(progn

	  (torus/update)

	  (setf circle (append (cdr circle) (list (car circle))))

	  (setf (cdr (car torus/torus)) circle)

	  (torus/jump)

	  )
	)

    (message "No element found in circle %s" (car (car torus/torus)))

    )

  )

(defun torus/next-element ()

  (interactive)

  (if (> (length (car torus/torus)) 1)
      (let
	  (
	   (circle (cdr (car torus/torus)))
	   )

	(progn

	  (torus/update)

	  (setf circle (append (last circle) (butlast circle)))

	  (setf (cdr (car torus/torus)) circle)

	  (torus/jump)

	  )

	)

    (message "No element found in circle %s" (car (car torus/torus)))

    )

  )

(defun torus/switch-circle (circle)

  "Change the current circle"

  (interactive (list (completing-read "Go to circle : " (mapcar #'car torus/torus) nil t)))

  (torus/update)

  (print circle)

  (torus/jump)

  )

(defun torus/switch-element (element)

  (interactive (list (completing-read "Go to element : " (cdr (car torus/torus)) nil t)))

  (torus/update)

  (print element)

  (torus/jump)

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

    (with-current-buffer buffer

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

    (with-current-buffer buffer

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
	(with-current-buffer buffer

	  (setq new-torus (read buffer))
	  (kill-buffer)

	  )

	(setf torus/torus (append torus/torus new-torus))
	(delete-dups torus/torus)

	)
    )

    )

;; ------------------------------

(provide 'torus)

;;; torus.el ends here
