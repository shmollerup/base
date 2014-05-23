;;; nav.el --- Navigation functions
;;
;; Filename: nav.el
;; Description: Navigation functions
;; Author: shm
;;
;;; Commentary:
;;
;; Navigation functions
;;
;;; Code:

(defun nav-scroll-up-1 ()
  " Scroll buffer 1 line up 'in place' "
  (interactive)
  (scroll-up 1))

(defun nav-scroll-down-1 ()
  " Scroll buffer 1 line down 'in place' "
  (interactive)
  (scroll-down 1))

(defun nav-scroll-up-10 ()
  " Scroll buffer 10 lines up 'in place' "
  (interactive)
  (scroll-up 10))

(defun nav-scroll-down-10 ()
  " Scroll buffer 10 lines down 'in place' "
  (interactive)
  (scroll-down 10))

(defun nav-scroll-other-window-up-1 ()
  " Scroll other buffer 1 line up "
  (interactive)
  (scroll-other-window 1))

(defun nav-scroll-other-window-down-1 ()
  " Scroll other buffer 1 line down "
  (interactive)
  (scroll-other-window -1))

(provide 'nav)
;;; nav.el ends here
