;;; bannedit.el --- Highlight banned words and summarily expel them.

;; Copyright (C) 2020 by Kevin Safford
;;
;; Author: Ric Lister
;; URL: https://github.com/ksafford/bannedit
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
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

;; -*- lexical-binding: t -*-

;; TODO: Make the matching case insensitive
;; TODO: Figure out how to use let instead of setq
;; TODO: Edit the regex so "e.g." will match
;; TODO: Setup customize group to accept a face to use and the list of banned words
;; TODO: Figure out how to import this to spacemacs

(defun bannedit-match-phrase-exactly (phrase) ()
       (concat "\\b" phrase "\\b"))

(defun bannedit-highlight-exact-phrase-in-blue (p) ()
       (highlight-phrase (bannedit-match-phrase-exactly p) 'anzu-match-1))

(defun bannedit-highlight-banned-words (badwords) ()
       (mapcar #'bannedit-highlight-exact-phrase-in-blue badwords))

(defun bannedit-unhighlight (p) ()
       (unhighlight-regexp (bannedit-match-phrase-exactly p)))

(defun bannedit-unhighlight-all (badwords) ()
     (mapcar #'bannedit-unhighlight badwords))

(defun bannedit-toggle-bannedit () (interactive)
       (if bannedit-switch
           (progn
             (bannedit-unhighlight-all bannedit-words)
             (setq bannedit-switch nil))
         (progn
           (bannedit-highlight-bad-words bannedit-words)
           (setq bannedit-switch t))))

(define-minor-mode bannedit-mode
  "Highlight banned words and remove them with extreme prejudice."
  :lighter " bannedit"

  (setq bannedit-face 'anzu-match-1)
  (setq bannedit-words '("just" "that" "already" "actual" "actually" "think" "pretty" "really" "to be" "great" "around" "a lot" "very" "thing" "much" "nice" "e\.g\." "therefor" "again" "I think" "I believe" "it seems" "to be"))

  (if bannedit-mode (progn
                      (setq bannedit-switch t)
                      (bannedit-highlight-banned-words bannedit-words))
    (progn
      (setq bannedit-switch nil)
      (bannedit-unhighlight-all bannedit-words))))

(provide 'bannedit)
