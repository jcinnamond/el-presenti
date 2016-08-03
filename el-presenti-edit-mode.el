;;; el-presenti-edit-mode.el --- major mode for editing el-presenti slides

;; Copyright 2016 John Cinnamond

;; Author: John Cinnamond
;; Version: 1.0.1

;;; Commentary:
;;
;; el-presenti is a presentation mode for emacs. This major mode
;; allows slides to be edited, for slides to be created, and for the
;; presentation to be started.

;;; License: see the file LICENSE.

;;; Code:
(defvar el-presenti-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e p") 'el-presenti-start-presentation)
    map)
  "Keymap for `el-presenti-edit-mode'.")

(defvar el-presenti-font-lock-keywords
  '(("^-\\{3,\\}+[[:space:]]*slide\\(:[^[:space:]]+\\)?[[:space:]]?-\\{3,\\}" . font-lock-comment-face)
    ("--[[:space:]]?[^[:space:]]+" . font-lock-keyword-face))
   "Keyword highlighting specification for `sample-mode'.")

(define-derived-mode el-presenti-edit-mode fundamental-mode
  "El Presenti Edit"
  "A major mode for editing el-presenti-mode"
  (setq-local font-lock-defaults '(el-presenti-font-lock-keywords))
  (use-local-map el-presenti-edit-mode-map))

(defun el-presenti-start-presentation ()
  (interactive)
  (el-presenti-start (el-presenti--build-slides)))

(defun el-presenti--build-slides ()
  (let ((slides ()))
    (dolist (slide (el-presenti--find-slides))
      (let ((type (car slide))
	    (content (cdr slide)))
	(cl-case type
	  ('slide (push (cons 'slide (el-presenti--build-slide-content content)) slides))
	  (t (push slide slides)))))
    (reverse slides)))

(defun el-presenti--find-slides ()
  (save-excursion
    (goto-char (point-min))
    (let ((last-pos nil)
	  (slide-contents ()))
      (while (search-forward-regexp "^-\\{3,\\}[[:space:]]*slide\\(?::\\([^:]+\\):\\([^[:space:]]+\\)\\)?[[:space:]]-\\{3,\\}" nil t)
	(if last-pos
	    (push (cons 'slide (buffer-substring-no-properties last-pos (match-beginning 0))) slide-contents))
	(if (match-beginning 1) ;; if we matched an instruction after slide
	    (let ((type (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
		  (value (buffer-substring-no-properties (match-beginning 2) (match-end 2))))
	      (push (cons (intern type) value) slide-contents)
	      (setq last-pos nil))
	  (setq last-pos (point))))
      (if last-pos
	  (push (cons 'slide (buffer-substring-no-properties last-pos (point-max))) slide-contents))
      (reverse slide-contents))))

(defun el-presenti--build-slide-content (str)
  (let ((contents ()))
    (dolist (line (split-string str "\n"))
      (if (string-match "-- [[:space:]]?\\([^[:space:]]+\\)\\(?:[[:space:]]\\(.*\\)\\)?" line)
	  (let ((type (substring line (match-beginning 1) (match-end 1)))
		(content (if (match-beginning 2)
			     (substring line (match-beginning 2) (match-end 2))
			   "\n")))
	    (push (list (intern type) content) contents))))
    (reverse contents)))

(provide 'el-presenti-edit-mode)
