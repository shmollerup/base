;;; print-functions.el --- Print functions
;;
;; Filename: print-functions.el
;; Description: Print functions
;; Author: shm
;;
;;; Commentary:
;;
;; Print functions
;;
;;; Code:

(defun print-printer () "The printer to use" "minolta")

(defun print-ps ()
  "Prints buffer to ps file"
  (interactive)
  (create-ps (generate-unique-filename (concat (expand-file-name (buffer-name)) ".ps"))))

(defun print-pdf ()
  "Prints buffer to pdf file"
  (interactive)
  (let ((ps-file (create-ps (make-temp-file "emacs-printing_")))
        (pdf-file (generate-unique-filename (concat (expand-file-name (buffer-name)) ".pdf"))))
    (call-process "ps2pdf14" nil nil nil ps-file pdf-file)
    (delete-file ps-file)
    (message "Wrote %s" pdf-file)))

(defun print-single ()
  "Prints buffer single page"
  (interactive)
  (let ((ps-file (create-ps (make-temp-file "emacs-printing_"))))
    (call-process "lpr" nil nil nil (concat "-P" (print-printer)) ps-file)
    (delete-file ps-file)
    (message "Printed %s" (buffer-name))))

(defun print-duplex ()
  "Prints buffer duplex"
  (interactive)
  (let ((ps-file (create-ps (make-temp-file "emacs-printing_"))))
    (call-process "lpr" nil nil nil (concat "-P" (print-printer) " -o Duplex=DuplexNoTumble") ps-file)
    (delete-file ps-file)
    (message "Printed %s" (buffer-name))))

(defun theme-wrapper (theme func)
  "wraps FUNC call in THEME"
  (let ((current-theme (car custom-enabled-themes)))
    (load-theme theme)
    (funcall func)
    (when (not (eq current-theme theme))      
      (load-theme current-theme))))

(defun create-ps (file)
  "Creates postscript buffer and save it to FILE"
  (theme-wrapper 'whiteboard
                 (lambda ()
                   (ps-spool-buffer-with-faces)
                   (switch-to-buffer "*PostScript*")
                   (write-file file)
                   (kill-buffer (current-buffer))))
  file)

(defun generate-unique-filename (basename)
  "generate unique filename based on BASENAME"
  (while (file-exists-p basename)
    (if (string-match "\\(.+\\)(\\(\[0-9\]+\\))$"  basename)
        (setq basename (concat (match-string 1 basename) "("
                               (number-to-string (+ (string-to-number(match-string 2 basename)) 1)) ")"))
      (setq basename (concat basename "(1)"))))
  basename)

(provide 'print-functions)
;;; print-functions.el ends here
