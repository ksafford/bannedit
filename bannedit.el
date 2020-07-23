;;; bannedit.el --- Highlight banned words and summarily expel them -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Kevin Safford
;;
;; Author: Ric Lister
;; URL: https://github.com/ksafford/bannedit
;; Package-Requires: ((emacs "24"))
;; Version: 1.0
;; Keywords: matching wp

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.
;;
;;; Commentary:
;;
;; This was inspired by thie post by Nat Eliason:
;;  https://www.nateliason.com/blog/better-writer
;; The idea is to highlight and commonly used weak words for easy
;; identification


;;; Code:
;; TODO: Edit the regex so "e.g." will match
;; TODO: Setup customize group to accept a face to use and the list of banned words

(defun bannedit-phrase-to-case-insensitive-regex (phrase)
  "Transform PHRASE into a regex string that will match PHRASE with any capitalization."
  ()
  (mapconcat (lambda (c) (concat "\\(" (upcase (char-to-string c)) "\\|"  (char-to-string c) "\\)" )) phrase ""))

(defun bannedit-match-phrase-exactly (phrase)
  "Transform PHRASE into a regex string wrapped by word boundaries."
  ()
  (concat "\\b" (bannedit-phrase-to-case-insensitive-regex phrase)  "\\b"))

(bannedit-match-phrase-exactly "therefore")

(defun bannedit-highlight-exact-phrase-in-blue (p)
  "Really just a partial function for 'highlight-phrase' taking only P, but with a FACE selected."
  ()
       (highlight-phrase (bannedit-match-phrase-exactly p) 'anzu-match-1))

(defun bannedit-highlight-banned-words (badwords)
  "Core function. Highlight all words in the list BADWORDS."
  ()
  (mapcar #'bannedit-highlight-exact-phrase-in-blue badwords))

(defun bannedit-unhighlight (p)
  "Inverse of 'bannedit-highlight-exact-phrase'. Unhighlights occurrences of P."
  ()
  (unhighlight-regexp (bannedit-match-phrase-exactly p)))

(defun bannedit-unhighlight-all (badwords)
  "Inverse of 'bannedit-highlight-banned-words'. Unhighlights all phrases in BADWORDS."
  ()
  (mapcar #'bannedit-unhighlight badwords))

(defun bannedit-toggle-bannedit ()
  "Toggle highlighting of the bannedit-words list. In case you want to keep the mode active but remove or re-enable highlighting."
  (interactive)
       (if bannedit-switch
           (progn
             (bannedit-unhighlight-all bannedit-words)
             (setq bannedit-switch nil))
         (progn
           (bannedit-highlight-banned-words bannedit-words)
           (setq bannedit-switch t))))

(define-minor-mode bannedit-mode
  "Highlight banned words and remove them with extreme prejudice."
  :lighter " bannedit"

  (let ((bannedit-face 'anzu-match-1)
        (bannedit-words '(
                          "just"
                          "that"
                          "already"
                          "actual"
                          "actually"
                          "think"
                          "pretty"
                          "really"
                          "to be"
                          "great"
                          "around"
                          "a lot"
                          "very"
                          "thing"
                          "much"
                          "nice"
                          "e\.g\."
                          "therefore"
                          "again"
                          "I think"
                          "I believe"
                          "it seems"
                          "to be"))))

  (if bannedit-mode (progn
                      (let ((bannedit-switch t)))
                      (bannedit-highlight-banned-words bannedit-words))
    (progn
      (let ((bannedit-switch nil)))
      (bannedit-unhighlight-all bannedit-words))))

(provide 'bannedit)

;;; bannedit.el ends here
