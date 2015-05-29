(define-minor-mode el-presenti-mode
  "Toggle el-presenti-mode."

  :init-value nil
  :lighter " El Presenti"
  :global t

  :keymap
  '(([prior] . el-presenti-previous-buffer)
    ([next] . el-presenti-next-buffer)
    ("C-c C-p" . el-presenti-stop))
  :group 'el-presenti)

(defun el-presenti-previous-buffer ()
  "Shows the previous buffer"
  (interactive)
  (let ((prev-buffer (el-presenti--cycle-back)))
    (if (null prev-buffer)
	(message "Already on the first buffer")
      (set-window-buffer nil el-presenti--current))))

(defun el-presenti-next-buffer ()
  "Shows the next buffer"
  (interactive)
  (let ((next-buffer (el-presenti--cycle-forward)))
    (if (null next-buffer)
	(message "Already on the last buffer")
      (set-window-buffer nil el-presenti--current))))

(defun el-presenti--cycle-forward ()
  "Return the next buffer in the list, or nil if there are no more buffers."
  (let ((item (pop el-presenti--next)))
    (if (not (null item))
	(progn
	  (setq el-presenti--previous (append el-presenti--previous (list el-presenti--current)))
	  (setq el-presenti--current item)))
    item))

(defun el-presenti--cycle-back ()
  "Return the previous buffer in the list, or nil if on the first buffer."
  (let ((item (car (last el-presenti--previous))))
    (if (not (null item))
	(progn
	  (setq el-presenti--next (cons el-presenti--current el-presenti--next))
	  (setq el-presenti--current item)
	  (setq el-presenti--previous (butlast el-presenti--previous))))
    item))

(defun el-presenti--add-to-next (item)
  "updates the global el-presenti next buffer state"
  (let ((safe-item (if (listp item) item (list item))))
    (setq el-presenti--next (cons safe-item el-present--next))))

(defun el-presenti-start (contents)
  "Set up the buffer list and start the el-presenti minor mode"
  (interactive)
  (let (buffers)
    (dolist (content contents buffers)
      (let ((buffer (generate-new-buffer (generate-new-buffer-name "el-presenti-slide"))))
	(with-current-buffer buffer
	  (insert content))
	(setq buffers (append buffers (list buffer)))))
    (setq el-presenti--opened-buffers (copy-list buffers))
    (setq el-presenti--current (pop buffers))
    (setq el-presenti--next buffers)
    (setq el-presenti--previous ()))
  (setq el-presenti--last-buffer (current-buffer))
  (delete-other-windows)
  (set-window-buffer nil el-presenti--current)
  (el-presenti-mode t))

(defun el-presenti-stop ()
  "Stop an el-presenti presentation and close any associated buffers"
  (interactive)
  (dolist (b el-presenti--opened-buffers)
    (kill-buffer b))
  (show-buffer nil el-presenti--last-buffer)
  (setq el-presenti--last-buffer nil)
  (el-presenti-mode nil))


(defun el-presenti--create-slide (content)
  "creates a slide, returns a buffer"
  (let ((buffer (generate-new-buffer (generate-new-buffer-name "el-presenti-slide"))))
    (with-current-buffer buffer
      (dolist (item content)
	(el-presenti--insert-slide-item item))
      buffer)))

(defun el-presenti--insert-slide-item (item)
  "Inserts an item into a slide.

The item should be a list of (type content) where type is one (title, subtitle)"
  (case (car item)
    ('title (el-presenti--insert-title (cdr item)))))

(defun el-presenti--insert-title (title)
  "Inserts a title into the buffer at point and sets the text properties"
  (let ((start (point))
	(end (+ (point) (length title))))
    (put-text-property 0 (length title) 'face '(:height 360 :foreground "#bb3333") title))
    (insert title))
