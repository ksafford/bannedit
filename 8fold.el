;;; eightfold.el --- 8-step process for editing  -*- lexical-binding: t -*-

;; Copyright (C) 2021 by Kevin Safford
;;
;; Author: Kevin Safford
;; URL: https://github.com/ksafford/bannedit
;; Package-Requires: ((emacs "26.1"))
;; Package-Version: 1.0.0
;; Keywords: 

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or any later version.
;;
;; This program is distributed in the hope -that- it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; This minor mode was inspired by ...

;;; Code:
;; TODO: This only works if org-list-use-circular-motion is nil. Otherwise
;;  org-next-item will cycle forever. Fix that.

(require 'org)

(defvar 8fold-section-template "* \n  - \n  - \n  - \n  - \n  - \n  - \n  - \n  - \n  - \n  - \n")
(defvar 8fold-steps-alist
      '((0 . "Welcome to 8fold.")
        (1 . "Write outline in headlines.")
        (2 . "Write about 10 sentences per section.")
        (3 . "Re-write each sentence")
        (4 . "Read each sentence aloud and select one to keep.")
        (5 . "Re-order the paragraphs.")
        (6 . "Read through the draft.")
        (7 . "Without looking at the current draft, write a new outline.")
        (8 . "Copy sentences from the draft into the new outline. Delete as needed.")))

(defvar 8fold-current-step 1)

(defun 8fold-now-what ()
  "Inform user about the current step."
  (interactive)
  (message
   (format-message "Next: %s" 
                   (alist-get 8fold-current-step 8fold-steps-alist))))

(defun 8fold-add-section ()
  "Add a section to an outline."
  (interactive)
  (goto-char (point-max))
  (insert 8fold-section-template))

(defun 8fold-create-outline (filename &optional n)
  "Create an outline structure with N headlines, 3 being the default.  Save it to FILENAME."
  (interactive "BEnter the name of the outline:
nEnter the number of sections:")
  (let* ((n (or n 3))
        (inserted n))
    (create-file-buffer filename)
    (switch-to-buffer filename)
    (while (> inserted 0)
      (call-interactively '8fold-add-section)
      (setq inserted (- inserted 1)))
    (beginning-of-buffer)
    (forward-char)
    (write-file filename)))

(defun 8fold-add-next-list-item ()
  "Add a single list item.  Return a value indicating if we're still in a list.  Used by 8fold-interleave."
  (end-of-line)
  (org-insert-item)
  (org-next-item)
  (org-in-item-p))

(defun 8fold-interleave-in-heading ()
  "Add a new line after each sentence in a single heading."
  (let ((users-org-list-use-circular-motion org-list-use-circular-motion)
        (8fold-still-in-list-p t))
    (setq org-list-use-circular-motion nil)
    (org-beginning-of-item-list)
    (while 8fold-still-in-list-p
      (setq 8fold-still-in-list-p (8fold-add-next-list-item)))
    ;; Reset org-next-item to whatever the user had it before invoking.
    (setq org-list-use-circular-motion users-org-list-use-circular-motion)))

;; TODO mark the location and return to it after interleave, somehow
(defun 8fold-interleave ()
  "Interleave all headings."
  (interactive)
  (beginning-of-buffer)
  
  (while 't
    (progn
      (next-line)
      (8fold-interleave-in-heading)
      (call-interactively #'org-next-visible-heading))))

;; TODO improve this function: list items might be indicated by:
;; arrows, number with a dot like 1. or a paren like 1), etc.
;; but the regex for empty-item-p only checks for a dash
;; Perhaps a clever use of org-item-re would help
(defun 8fold-heading-or-item-is-blank-p ()
  "Is the org heading or item on this line blank?"
  (interactive)
  (let(
       (empty-heading-p (and (org-at-heading-p) (not (nth 4 (org-heading-components)))))
       (empty-item-p (and (org-at-item-p) (org-in-regexp "^[[:blank:]]*\-[[:blank:]]$"))))
    
    (message (format-message "returns: %s empty-heading-p: %s empty-item-p: %s"
                             (or empty-heading-p empty-item-p)
                             (or empty-heading-p "nil")
                             (or empty-item-p "nil")))  
    (or empty-heading-p empty-item-p)))

;; TODO mark location and return to it after, somehow
(defun 8fold-cleanup ()
  "Remove unused headlines and list items."
  (interactive)
  (beginning-of-buffer)
  (while 't
    (if (8fold-heading-or-item-is-blank-p) 
        (kill-whole-line)
      (forward-line))))

(defun 8fold-convert-to-paragraphs ()
  "Take the org structured document and convert it to paragraphs."
  (interactive)
  (end-of-buffer)
  (while (not (eq (point) (point-min)))
    (if (org-at-heading-p) (progn
                            (beginning-of-line)
                            (delete-char 2)
                            (newline))
        ())
    (if (org-at-item-p) (progn 
                          (org-delete-indentation)
                          (delete-char 2))
      (forward-line -1))))

(defun 8fold-archive-line ()
  "Send the current line to the archive.")

(defun 8fold-iterate-and-select ()
  "Iterate through pairs of sentences and select one from the pair to keep. Archive the other.")

(defvar 8fold-step-actions-alist
  '((1 . (call-interactively #'8fold-create-outline))
    (2 . (call-interactively #'8fold-cleanup))
    (3 . (call-interactively #'8fold-interleave))
    (4 . (call-interactively #'8fold-iterate-and-select))
    (5.  (call-interactively #'8fold-reordser))
    (6 . (call-interactively #'8fold-convert-to-paragraphs-and-read))
    (7 . (call-interactively #'8fold-rewrite-outline))
    (8 . (call-interactively #'8fold-fillin-new-from-old))))

(defvar 8fold-step-from-to-alist
  '((0 . 1)
    (1 . 2)
    (2 . 3)
    (3 . 4)
    (4 . 5)
    (5 . 6)
    (6 . 7)
    (7 . 8)
    (8. 2)))

(defun 8fold-goto-step ()
  "Go to the Nth 8fold-step."
  (interactive) 
  (setq 8fold-current-step
        (string-to-number (read-string
                          (apply 'format "Select a step to jump to:\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s" 8fold-steps-alist))))
  (message (format-message "%s" (alist-get 8fold-current-step 8fold-steps-alist))))

(defun 8fold-step ()
  "Perform the next step in the 8fold process."
  (interactive)
  
  (let ((current-function (alist-get 8fold-current-step 8fold-step-actions-alist)))
    (eval current-function)
    (setq 8fold-current-step (alist-get 8fold-current-step 8fold-step-from-to-alist)))
  
  (message (format-message "%s" (alist-get 8fold-current-step 8fold-steps-alist))))

;;;###autoload
(define-minor-mode 8fold-mode
  "An 8-step editing process."
  :lighter " 8")

;;; 8fold.el ends here
