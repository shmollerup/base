;;; base.el --- Provide basic functions
;;
;; Filename: base.el
;; Description: Provide basic functions
;; Author: shm
;;
;;; Commentary:
;;
;; Provide basic functions
;;
;;; Code:

(or (fboundp 'replace-in-string)
    (defun replace-in-string (target old new)
      (replace-regexp-in-string old new  target)))

(defun chomp (str)
  "perl like chomp"
  (let ((s (if (symbolp str)(symbol-name str) str)))
    (save-excursion
      (while (and
              (not (null (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
              (> (length s) (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
        (setq s (replace-match "" t nil s)))
      (while (and
              (not (null (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
              (> (length s) (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
        (setq s (replace-match "" t nil s))))
    s))


(defun filter (condp lst)
  "Classic filter"
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun generate-unique-filename (basename)
  "generate unique filename based on BASENAME"
  (while (file-exists-p basename)
    (if (string-match "\\(.+\\)(\\(\[0-9\]+\\))$"  basename)
        (setq basename (concat (match-string 1 basename) "("
                               (number-to-string (+ (string-to-number(match-string 2 basename)) 1)) ")"))
      (setq basename (concat basename "(1)"))))
  basename)

(defun lpad (pad-char str-length str)
  (if (<= 0 (- str-length (length str)))
      (concat (apply 'concat (make-list (- str-length (length str)) pad-char)) str)
    str))

(defun rpad (pad-char str-length str)
  (if (<= 0 (- str-length (length str)))
      (concat str (apply 'concat (make-list (- str-length (length str)) pad-char)))
    str))


(provide 'base)
;;; base.el ends here
