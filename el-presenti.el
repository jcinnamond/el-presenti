(defface el-presenti-title-face
  '((t :family "Helvetica" :height 1600))
  "The face to display titles"
  :group :el-presenti)

(defface el-presenti-subtitle-face
  '((t :family "Helvetica" :height 360))
  "The face to display subtitles"
  :group :el-presenti)

(defface el-presenti-code-face
  '((t :height 240))
  "The face to display subtitles"
  :group :el-presenti)

(define-minor-mode el-presenti-mode
  "Toggle el-presenti-mode."

  :init-value nil
  :lighter " [Presentation]"
  :global t

  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<prior>") 'el-presenti-previous-buffer)
            (define-key map (kbd "<next>") 'el-presenti-next-buffer)
            (define-key map (kbd "C-c C-p") 'el-presenti-stop)
            map)
  :group 'el-presenti)


(defun el-presenti--show-buffer (buffer-pair)
  (let ((buffer (car buffer-pair))
	(buffer-type (cdr buffer-pair)))
    (set-window-buffer nil buffer)
    (with-current-buffer buffer
      (if (eq 'slide buffer-type)
	  (el-presenti--hide-cursor)
	(el-presenti--show-cursor)))))

(defun el-presenti-previous-buffer ()
  "Shows the previous buffer"
  (interactive)
  (let ((prev-buffer (el-presenti--cycle-back)))
    (if (null prev-buffer)
	(message "Already on the first slide")
      (el-presenti--show-buffer el-presenti--current))))

(defun el-presenti-next-buffer ()
  "Shows the next buffer"
  (interactive)
  (let ((next-buffer (el-presenti--cycle-forward)))
    (if (null next-buffer)
	(message "Already on the last slide")
      (el-presenti--show-buffer el-presenti--current))))

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

(defun el-presenti--frame-property (prop)
  (cdr (assoc prop (frame-parameters))))

(defun el-presenti--hide-cursor ()
  (modify-frame-parameters (selected-frame) (list (cons 'cursor-type nil))))

(defun el-presenti--show-cursor ()
  (modify-frame-parameters (selected-frame) (list (cons 'cursor-type el-presenti--previous-cursor))))

(defun el-presenti--hide-emacs ()
  (setq el-presenti--previous-cursor (el-presenti--frame-property 'cursor-type))
  (setq el-presenti--background-color (el-presenti--frame-property 'background-color))
  (set-background-color "Black")
  (setq el-presenti--previous-fringe fringe-mode)
  (set-fringe-mode 0))

(defun el-presenti--restore-emacs ()
  (el-presenti--show-cursor)
  (set-fringe-mode el-presenti--previous-fringe)
  (set-background-color el-presenti--background-color))

(defun el-presenti-start (slides)
  "Set up the buffer list and start the el-presenti minor mode"
  (let (buffers)
    (dolist (slide-content slides buffers)
      (let ((type (car slide-content))
	    (content (cdr slide-content)))
	(case type
	  ('file (add-to-list 'buffers (el-presenti--load-file content)))
	  ('slide (add-to-list 'buffers (el-presenti--create-slide content))))))
    (setq buffers (reverse buffers))
    (setq el-presenti--opened-buffers (copy-list buffers))
    (setq el-presenti--current (pop buffers))
    (setq el-presenti--next buffers)
    (setq el-presenti--previous ()))
  (setq el-presenti--last-buffer (current-buffer))
  (el-presenti--hide-emacs)
  (delete-other-windows)
  (el-presenti--show-buffer el-presenti--current)
  (el-presenti-mode t))

(defun el-presenti-stop ()
  "Stop an el-presenti presentation and close any associated buffers"
  (interactive)
  (dolist (b el-presenti--opened-buffers)
    (kill-buffer (car b)))
  (el-presenti--restore-emacs)
  (show-buffer nil el-presenti--last-buffer)
  (setq el-presenti--last-buffer nil)
  (el-presenti-mode 0))

(defun el-presenti--load-file (filename)
  (let ((existing-buffer (find-buffer-visiting filename)))
    (if existing-buffer
	(with-current-buffer existing-buffer
	  (cons (clone-indirect-buffer filename nil) 'file))
      (cons (find-file-noselect filename) 'file))))

(defun el-presenti--create-slide (content)
  "creates a slide, returns a buffer"
  (let ((buffer (generate-new-buffer (generate-new-buffer-name "el-presenti-slide"))))
    (with-current-buffer buffer
      (setq mode-line-format nil)
      (dolist (item content)
	(el-presenti--insert-slide-item item)))
    (cons buffer 'slide)))

(defun el-presenti--insert-slide-item (item)
  "Inserts an item into a slide.

The item should be a list of (type content) where type is one (title, subtitle)"
  (let ((face (intern (concat "el-presenti-" (symbol-name (car item)) "-face"))))
    (el-presenti--insert-with-face (cadr item) face)))

(defun el-presenti--insert-with-face (text face)
  "Inserts text into a buffer at point and sets the face"
  (let ((start (point))
	(end (+ (point) (length text))))
    (put-text-property 0 (length text) 'face face text))
  (insert text)
  (newline))
