;;; display-functions.el --- Display functions
;;
;; Filename: display-functions.el
;; Description: Display functions
;; Author: shm
;;
;;; Commentary:
;;
;; Display functions
;;
;;; Code:

(defun toggle-line-wrapping ()
  "Toggles line wrapping in the current buffer."
  (interactive)
  (if (eq truncate-lines nil)
      (progn
        (setq truncate-lines t)
        (redraw-display)
        (message "Setting truncate-lines to t"))
    (setq truncate-lines nil)
    (redraw-display)
    (message "Setting truncate-lines to nil")))

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(defun dark-theme ()
  "Dark color theme"
  (interactive)
  (load-theme 'deeper-blue)
  (set-face-background 'hl-line "#131313"))

(defun light-theme ()
  "Light color theme"
  (interactive)
  (load-theme 'whiteboard)
  (set-face-background 'hl-line "#ededed"))

(provide 'display-functions)
;;; display-functions.el ends here
