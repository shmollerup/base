;;; yic.el --- Buffer switching functions
;;
;; Filename: yic.el
;; Description: Buffer switching functions
;; Author: shm
;;
;;; Commentary:
;;
;; The code was found on the internet sometime/somewhere - now it
;; lives here.
;;
;; Provides functions for easy switching between buffers.
;;
;;; Code:

(defun yic-prev-buffer ()
  "Switch to previous buffer in current window."
  (interactive)
  (yic-next (reverse (buffer-list))))

(defun yic-next-buffer ()
  "Switch to the other buffer (2nd in list-buffer) in current window."
  (interactive)
  (bury-buffer (current-buffer))
  (yic-next (buffer-list)))

(defun yic-next (ls)
  "Switch to next buffer in ls skipping unwanted ones."
  (let* ((ptr ls)
         bf bn go)
    (while (and ptr (null go))
      (setq bf (car ptr)  bn (buffer-name bf))
      (if (null (yic-ignore bn))        ;skip over
          (setq go bf)
        (setq ptr (cdr ptr))))
    (if go
        (switch-to-buffer go))))

(defun yic-ignore (str)
  (or ;;buffers I don't want to switch to
   (string-match "\\*.+\\*" str)
   (string-match "^ " str)

   ;;Test to see if the window is visible on an existing visible frame.
   ;;Because I can always ALT-TAB to that visible frame, I never want to
   ;;Ctrl-TAB to that buffer in the current frame.  That would cause
   ;;a duplicate top-level buffer inside two frames.
   (memq str
         (mapcar
          (lambda (x)
            (buffer-name
             (window-buffer
              (frame-selected-window x))))
          (visible-frame-list)))))

(provide 'yic)
;;; yic.el ends here
