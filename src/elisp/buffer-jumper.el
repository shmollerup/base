;;; buffer-jumper.el --- Functions to switch to specific buffer and back
;;
;; Filename: buffer-jumper.el
;; Description: Functions to switch to specific buffer and back
;; Author: shm
;;
;;; Commentary:
;;
;; Provide Functions to switch to specific buffer and back with one keystroke
;;
;;  Examples of usage:
;;
;;   (defun toggle-messages-buffer () "Toggles messages buffer" (interactive) (toggle-buffer "*Messages*"))
;;   (defun toggle-python-buffer () "Toggles python buffer" (interactive) (toggle-buffer "*python*" 'python-mode))
;;
;;; Code:

(defvar buffer-jumper-list () "Contains all active buffer jumps")

(defun clear-buffer-jumper-list ()
  "Reinitializes buffer-jumper-list"
  (interactive)
  (setq buffer-jumper-list ())
  (message "buffer-jumper-list cleared"))

(defun toggle-buffer (buf &optional mode)
  "Toggles to buffer BUF, and sets up buffer-jumper-list to jump back"
  (if (string= buf (buffer-name))
      (buffer-jumper-back buf)
    (buffer-jumper-forward buf mode)))

(defun buffer-jumper-back (buf)
  "Jumps back from BUF to original buffer"
  (when (get-buffer (cadr (assoc buf buffer-jumper-list)))
    (switch-to-buffer (cadr (assoc buf buffer-jumper-list)))
    (setq buffer-jumper-list (delq (assoc buf buffer-jumper-list) buffer-jumper-list))))

(defun buffer-jumper-forward (buf &optional mode)
  "Jumps forward to buffer BUF"
  (setq buffer-jumper-list (append buffer-jumper-list (list (list buf (buffer-name)))))
  (switch-to-buffer (get-buffer-create buf))
  (when mode
    (funcall mode)))

(provide 'buffer-jumper)
;;; buffer-jumper.el ends here
