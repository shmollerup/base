;;; editor-functions.el --- Editor functions
;;
;; Filename: editor-functions.el
;; Description: Editor functions
;; Author: shm
;;
;;; Commentary:
;;
;; Editor functions
;;
;;; Code:

(defun iwb ()
  " indent whole buffer "
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun kill-prev ()
  " Like C-k, only kills from line start to point. "
  (interactive)
  (set-mark-command nil)
  (beginning-of-line)
  (kill-region (point) (mark)))

(defun mark-whole-word()
  " Marks whole word "
  (interactive)
  (forward-word)
  (set-mark-command nil)
  (backward-word))

(defun toggle-comment (arg)
  "Comment or uncomment region accordingly"
  (interactive "*P")
  (if mark-active
      (comment-dwim arg)
    (save-excursion
      (let ((has-comment? (progn (beginning-of-line) (looking-at (concat "s-*" (regexp-quote comment-start))))))
        (push-mark (point) nil t)
        (end-of-line)
        (if has-comment?
            (uncomment-region (mark) (point))
          (comment-region (mark) (point)))))))

(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p" )))

(provide 'editor-functions)
;;; editor-functions.el ends here
