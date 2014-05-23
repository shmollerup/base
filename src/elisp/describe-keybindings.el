;;; describe-keybindings.el --- Describes keybinding function
;;
;; Filename: describe-keybindings.el
;; Description: Contains macro that can describe a keybinding function
;; Author: shm
;;
;;; Commentary:
;;
;; The describe-keybindings macro creates a new buffer, with a binding
;; on each line. The binding function is links to the relevant
;; documentation. If a format call are found instead of a define-key
;; call, the result of the format call is inserted in the describe
;; buffer
;;
;;; Code:

(defmacro describe-keybindings (key-map)
  "Creates buffer where KEY-MAP and markup is inserted"
  (let ((key-bindings-buffer-name (format "*%s-keybindings*" key-map))
        (max-key-len
         (apply 'max
                (mapcar
                 '(lambda (key-binding)
                    (if (eq (nth 0 key-binding) 'define-key)
                        (length (format "%s"(nth 2 key-binding)))
                      0))
                 (nthcdr 2 (symbol-function key-map))))))

    (get-buffer-create key-bindings-buffer-name)
    (switch-to-buffer key-bindings-buffer-name)
    (insert (format "%s keymap\n" key-map))

    (describe-keybindings-print (nthcdr 2 (symbol-function key-map)) max-key-len)
    (push-mark)
    (insert "[back]")
    (make-button (mark) (point) 'action `(lambda (x) (kill-buffer ,key-bindings-buffer-name)))

    (setq buffer-read-only t)
    (goto-char (point-min))))

(defun describe-keybindings-print (bindings max-len)
  "Iterate and output each keybindings line"
  (when bindings
    (describe-keybindings-insert (car bindings) max-len)
    (describe-keybindings-print (cdr bindings) max-len)))

(defun describe-keybindings-insert (key-binding max-len)
  "inserts keybinding in buffer"
  (if (eq (nth 0 key-binding) 'format)
      (insert (format "\n%s\n" (eval key-binding)))
    (when (eq (nth 0 key-binding) 'define-key)
      (insert (format "%s%s" (nth 2 key-binding)
                      (apply 'concat (make-list (- (+ 6 max-len)
                                                   (length (format "%s" (nth 2 key-binding)))) " "))))
      (push-mark)
      (insert (format "%s\n" (nth 1 (nth 3 key-binding))))
      (make-button (mark) (point) 'action
                   `(lambda (x) (describe-function ,(nth 3 key-binding)))))))

(provide 'describe-keybindings)
;;; describe-keybindings.el ends here
