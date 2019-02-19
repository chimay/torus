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
;; A ring is a group of buffers
;; A torus is a group of ring, a kind of session if you will
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

(defvar torus/prefix-key (kbd "<f6>"))

;; Keymap with prefix
;; ------------------------------

(define-prefix-command 'torus/map)

;; Functions
;; ------------------------------

(defun torus/quit ()



  )

;; Commands
;; ------------------------------

;; Interactive functions

(defun torus/install-default-bindings ()

  (interactive)

  (global-set-key torus/prefix-key 'torus/map)

  (define-key torus/map (kbd "i") 'torus/init)

  (define-key torus/map (kbd "p") 'torus/print)

  (define-key torus/map (kbd "a r") 'torus/add-ring)
  (define-key torus/map (kbd "a e") 'torus/add-element)

  (define-key torus/map (kbd "r") 'torus/read)
  (define-key torus/map (kbd "w") 'torus/write)

  )

(defun torus/init ()

  "Initialize torus
Add hooks"

  (interactive)

  (setq torus/torus nil)

  (if torus/save-on-exit (add-hook 'kill-emacs-hook 'torus/quit))

  )

(defun torus/print ()

  (interactive)

  (pp torus/torus)

  )

(defun torus/add-ring (name)

  "Add ring to torus"

  (interactive "sName for the new ring : ")

  (push (list name) torus/torus)

  )

(defun torus/add-element ()

  "Add current file and point to current ring"

  (interactive)

  (let
      (
       (ring (car torus/torus))
       (element (cons (buffer-file-name) (marker-position (point-marker))))
       )
    (progn

      (if (> (length ring) 1)

	 (progn
	   (setf (second ring) (append (list element) (second ring)))

	   )

       (progn
	 (setf ring (cons (car ring) (list (list element))))

	 )
       )

      )

    (setf (car torus/torus) ring)
    )

  )

(defun torus/change-element ()

  (interactive)



  )

(defun torus/change-ring ()

  "Change the current ring"

  (interactive)

  (let

      (
       (ring (completing-read "Go to ring : " torus/torus nil t))
       )

    (

     )
    )

  )

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

      ;; (print torus/torus buffer)

      ;; Better with pretty print

      (pp torus/torus buffer)

      (save-buffer)
      (kill-buffer)

      )
    )
  )

(defun torus/read ()

  "Read torus from a file"

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

;; ------------------------------

(provide 'torus)

;;; torus.el ends here
