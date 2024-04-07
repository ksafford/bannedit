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
  (insert 8fold-section-template))

(defun 8fold-add-archive-section ()
  "Add an archive for sentences that have been replaced or removed."
  (goto-char (point-max))
  (insert "* Archive"))

(defun 8fold-create-outline (name &optional n)
  "Create an outline structure with N headlines, 3 being the default.  Save it to FILENAME."
  (interactive "BEnter the name of the outline:
nEnter the number of sections:")
  (let* ((n (or n 3))
        (inserted 0)
        (name (file-name-with-extension name "org")))
    (create-file-buffer name)
    (switch-to-buffer name)
    (set-auto-mode)
    (while (< inserted n)
      (call-interactively #'8fold-add-section)
      (setq inserted (+ inserted 1)))
    (8fold-add-archive-section)
    (beginning-of-buffer)
    (forward-char)
    (write-file name)))

(defun 8fold-add-next-list-item ()
  "Add a single list item.  Return a value indicating if we're still in a list.  Used by 8fold-interleave."
  (end-of-line)
  (org-insert-item)
  (org-next-item)
  (org-in-item-p))

(defun 8fold-interleave-in-heading ()
  "Add a new line after each sentence in a single heading. Do nothing if in the Archive."
  (interactive)
  (if (8fold-in-archive)
      ()
    (if (org-at-heading-p) 
        (next-line)
      (org-beginning-of-item-list))
    (while (and (org-in-item-p) (not (eobp)))
      (if (8fold-heading-or-item-is-blank-p)
          (next-line)
        (progn
          (end-of-line)
          (org-insert-item)
          (next-line))))))

(defun 8fold-interleave ()
  "Interleave all headings."
  (interactive)
  (org-map-entries (lambda () (8fold-interleave-in-heading)))
  (beginning-of-buffer))

;; TODO improve this function: list items might be indicated by:
;; arrows, number with a dot like 1. or a paren like 1), etc.
;; but the regex for empty-item-p only checks for a dash
;; Perhaps a clever use of org-item-re would help
(defun 8fold-heading-or-item-is-blank-p ()
  "Is the org heading or item on this line blank?"
  (interactive)
  (let ((empty-heading-p (and (org-at-heading-p) (not (nth 4 (org-heading-components)))))
        (empty-item-p (and (org-at-item-p) (org-in-regexp "^[[:blank:]]*\-[[:blank:]]$"))))
    
    (message (format-message "returns: %s empty-heading-p: %s empty-item-p: %s"
                             (or empty-heading-p empty-item-p)
                             (or empty-heading-p "nil")
                             (or empty-item-p "nil")))  
    (or empty-heading-p empty-item-p)))

(defun 8fold-in-archive ()
  "Check if the cursor is in the Archive tree."
  (let ((this-heading (org-get-heading 't 't 't 't)))
    (string-equal "Archive" this-heading)))

(defun 8fold-cleanup ()
  "Remove unused headlines and list items."
  (interactive)
  (let ((starting-point (point)))
    (beginning-of-buffer)
    (while (not (8fold-in-archive-p))
      (if (8fold-heading-or-item-is-blank-p) 
          (kill-whole-line)
        (forward-line)))
    (goto-char starting-point)))

(defun 8fold-convert-to-paragraphs ()
  "Take the org structured document and convert it to paragraphs."
  (interactive)
  (end-of-buffer)
  
  (while (8fold-in-archive-p)
    (forward-line -1))

  (while (not (eq (point) (point-min)))
    (if (org-at-heading-p) (progn
                             (beginning-of-line)
                             (delete-char 2)
                             (newline)))
    (if (org-at-item-p) (progn 
                          (org-delete-indentation)
                          (delete-char 2))
      (forward-line -1))))

(defun 8fold-archive-line ()
  "Send the current line to the archive, if it exists, create it and send the line if it doesn't."
  (interactive)
  (let ((start-point (point))
        (item (thing-at-point 'line t)))
    (if (org-at-item-p)
        (progn
          (kill-whole-line)
          (if (search-forward-regexp "^\\*+ Archive$" nil t)
              (org-end-of-subtree) ; Move to the end of the Backup subtree
            (progn
              (goto-char (point-max)) ; Go to the end of the buffer
              (insert "\n* Archive\n"))) ; Create Backup headline
          ;; Paste the item under the Backup headline
          (open-line 1)
          (forward-line 1)
          (yank))
      (message "Not on an org-mode item!"))
    (goto-char start-point)))

(defun 8fold-keep-or-archive ()
  "Prompt to either keep the current sentence or send it to the archive."
  (interactive)
  (let ((keep-or-archive (completing-read "Keep this sentence, or Archive it: " '("Keep" "Archive"))))
    (if (string-equal-ignore-case keep-or-archive "Archive")
        (call-interactively #'8fold-archive-line))))

(defun 8fold-iterate-and-select ()
  "Iterate through pairs of sentences and select one from the pair to keep. Archive the other."
  (interactive)
  (beginning-of-buffer)
  (while (not (8fold-in-archive-p))
    (if (org-at-item-p)
        (progn
          (8fold-keep-or-archive)
          (forward-line))
      (forward-line))))

(defun 8fold-reorder ()
  "Reorder the paragraphs."
  (interactive)
  (message "Reorder the paragraphs by moving the headlines around."))

(defvar 8fold-step-actions-alist
  '((1 . (progn
           (call-interactively #'8fold-create-outline)
           (message "Write the topic for each paragraph as headlines. Write each sentence on a separate list item. Add new sections as needed with 8fold-add-section. When the first draft is done, continue to the next step.")))
    (2 . (progn
           (call-interactively #'8fold-cleanup)
           (call-interactively #'8fold-interleave)
           (message "Now, rewrite each sentence in the newly created blank list items, then progress to the next step.")))
    (3 . (progn
           (message "Select which versions of each sentence to keep or send to the Archive.")
           (call-interactively #'8fold-iterate-and-select)))
    (4 . (call-interactively #'8fold-reorder))
    (5 . (progn
           (call-interactively #'8fold-convert-to-paragraphs)
           (message "Read what you've written aloud.")))
    (6 . (call-interactively #'8fold-rewrite-outline))
    (7 . (call-interactively #'8fold-fillin-new-from-old))))

(defvar 8fold-step-from-to-alist
  '((0 . 1)
    (1 . 2)
    (2 . 3)
    (3 . 4)
    (4 . 5)
    (5 . 6)
    (6 . 7)
    (7 . 2)))

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
