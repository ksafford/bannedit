;;; bannedit.el --- Highlight banned words and summarily expel them.

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
       (unhighlight-regexp (match-phrase-exactly p)))

(defun bannedit-unhighlight-all (badwords) ()
     (mapcar #'bannedit-unhighlight badwords))

(defun bannedit-toggle-bannedit () (interactive)
       (if bannedit-switch
           (progn
             (bannedit-unhighlight-all badwords)
             (setq bannedit-switch nil))
         (progn
           (bannedit-highlight-bad-words badwords)
           (setq bannedit-switch t))))

(define-minor-mode bannedit-mode
  "Highlight banned words and remove them with extreme prejudice."
  :lighter " bannedit"

  (if bannedit-mode (progn
                      (setq bannedit-switch t)
                      (setq bannedit-face 'anzu-match-1)
                      (setq bannedit-words '("just" "that" "already" "actual" "actually" "think" "pretty" "really" "to be" "great" "around" "a lot" "very" "thing" "much" "nice" "e.g." "therefor" "again" "I think" "I believe" "it seems" "to be"))
                      (bannedit-highlight-banned-words bannedit-words))
    (progn
      (setq bannedit-mode-switch nil)
      (bannedit-unhighlight-all bannedit-words))))

(provide 'bannedit)
