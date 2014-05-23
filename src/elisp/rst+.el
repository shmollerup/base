;;; rst+.el --- Provide extra functions for rst mode
;;
;; Filename: rst+.el
;; Description: Provide extra functions for rst mode
;; Author: shm
;;
;;; Commentary:
;;
;; Provide pdf and print functions to rst-mode.
;;
;;  Examples of usage:
;;
;;   (require 'rst+)
;;   (add-hook 'rst-mode-hook 'rst+-keymap)
;;
;;; Code:
(require 'base)
(require 'print-functions)

(defun rst+-keymap ()
  "Shortcuts for functions provided in rst+"
  (define-key rst-mode-map [(control c) (e)]              'rst-expand-header)
  (define-key rst-mode-map [(control c) (r)]              'rst-expand-header-center)
  (define-key rst-mode-map [(control c) (control c) (z)]  'rst-to-pdf-print-single)
  (define-key rst-mode-map [(control c) (control c) (x)]  'rst-to-pdf-print-duplex)
  (define-key rst-mode-map [(control c) (control c) (c)]  'rst-to-pdf-file)
  (define-key rst-mode-map [(control c) (control c) (v)]  'rst-to-pdf-file-preview))

(defun rst-expand-header ()
  "Expands header based on char at beginning of line"
  (interactive)
  (rst-expand t) )

(defun rst-expand-header-center ()
  "Expands header markup above and below header based on char at beginning of line"
  (interactive)
  (rst-expand))

(defun rst-to-pdf-file-preview ()
  "Translate rst in buffer to pdf,prints to file (with a pdf extension) and opens okular"
  (interactive)
  (rst-to-pdf nil t t))

(defun rst-to-pdf-file ()
  "Translate rst in buffer to pdf and prints to file (with a pdf extension)"
  (interactive)
  (rst-to-pdf nil t))

(defun rst-to-pdf-print-single ()
  "Translate rst in buffer to pdf and print the content"
  (interactive)
  (rst-to-pdf nil t))

(defun rst-to-pdf-print-duplex ()
  "Translate rst in buffer to pdf and print the content as duplex"
  (interactive)
  (rst-to-pdf nil t))

(defun rst-to-pdf (&optional print-mode &optional file &optional preview)
  "Converts rst to pdf"
  (let ((pdf-file (if file
                      (concat (file-name-sans-extension (expand-file-name (buffer-name))) ".pdf")
                    (make-temp-file "emacs-rst-printing_" nil ".pdf"))))

    (call-process "rst2pdf" nil nil nil (expand-file-name (buffer-name)) "-o" pdf-file)
    (if file
        (progn
          (message "Written pdf to %s" pdf-file)
          (when preview
            (start-process "okular" "*okular*" "okular" pdf-file)))
      (progn
        (if (string= "duplex" print-mode)
            (call-process "lpr" nil nil nil (concat "-P" (print-printer) " -o Duplex=DuplexNoTumble") pdf-file)
          (call-process "lpr" nil nil nil (concat "-P" (print-printer)) pdf-file))
        (delete-file pdf-file)
        (message "Printed %s to printer %s" (buffer-name) (print-printer))))))

(defun rst-expand (&optional center)
  "Expand header"
  (let ((p (point))
        (c (rst-char-at-beginning-of-line))
        (l (rst-len-of-previous-line)))
    (when (= c 10) (error "No expansion char found at begining of line"))
    (beginning-of-line 0)
    (when (= (rst-char-at-beginning-of-line) 10)
      (goto-char p)
      (error "No headline found"))
    (when center
      (rst-insert-header c l))
    (beginning-of-line 2)
    (rst-remove-line)
    (rst-insert-header c l)))

(defun rst-remove-line ()
  (end-of-line)
  (push-mark)
  (beginning-of-line)
  (delete-region (point)(mark)))

(defun rst-char-at-beginning-of-line  ()
  (save-excursion(progn (beginning-of-line) (char-after))))

(defun rst-len-of-previous-line ()
  (save-excursion (- (progn (end-of-line 0) (point))
                     (progn (beginning-of-line) (point)))))

(defun rst-insert-header (c l)
  (insert (apply 'concat (make-list l (char-to-string c))))
  (insert "\n"))

(provide 'rst+)
;;; rst+.el ends here
