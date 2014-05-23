;;; default-keybindings.el --- Default keybindings
;;
;; Filename: default-keybindings.el
;; Description: Default keybindings
;; Author: shm
;;
;;; Commentary:
;;
;; Contains default keybindings.
;; The format of the default-keybindings functions can be described
;; through `describe-keybindings'.
;;
;;; Code:
(require 'describe-keybindings)
(require 'display-functions)

;; -- Functions wrapping lambda calls for documentation purposes
(defun toggle-scratch-buffer () "Toggles scratch buffer" (interactive) (toggle-buffer "*scratch*"))
(defun toggle-messages-buffer () "Toggles messages buffer" (interactive) (toggle-buffer "*Messages*"))
(defun toggle-python-buffer () "Toggles python buffer" (interactive) (toggle-buffer "*python*" 'python-mode))
(defun popup-yank () "Creates popup menu with kill-ring content" (interactive) (popup-menu 'yank-menu))
(defun hippie-unexpand () "Cancels hippie expansion" (interactive) (hippie-expand 0))

(defun toggle-dictionary ()
  "Toggles between danish and american dictionarys"
  (interactive)
  (if (string= ispell-local-dictionary "dansk")
      (ispell-change-dictionary "american")
    (ispell-change-dictionary "dansk")))

(defun default-keybindings ()
  "Default keybindings for emacs"

  (format "General")
  (define-key global-map [(control x)(control r)]    'recentf-open-files)
  (define-key global-map [(control x)(S)]            'save-current-configuration)
  (define-key global-map [(control x)(F)]            'resume)
  (format "Display")
  (define-key global-map [(shift insert)]            'toggle-line-wrapping)
  (define-key global-map [insert]                    'toggle-fullscreen) ;; switch to overwrite mode.
  
  (define-key global-map [(meta insert)]             '(lambda () (describe-keybindings default-keybindings)))

  (format "Navigation")
  (define-key global-map [(meta up)]                 'nav-scroll-down-1)
  (define-key global-map [(meta down)]               'nav-scroll-up-1)
  (define-key global-map [(meta shift up)]           'nav-scroll-other-window-down-1)
  (define-key global-map [(meta shift down)]         'nav-scroll-other-window-up-1)

  (define-key global-map [(meta left)]               'yic-prev-buffer)
  (define-key global-map [(meta right)]              'yic-next-buffer)

  (define-key global-map [(meta control left)]       'windmove-left)
  (define-key global-map [(meta control right)]      'windmove-right)
  (define-key global-map [(meta control up)]         'windmove-up)
  (define-key global-map [(meta control down)]       'windmove-down)
  (define-key global-map [(meta g)]                  'goto-line) ;; override facemenu-keymap

  (define-key global-map [(f12)]                     'toggle-scratch-buffer)
  (define-key global-map [(shift f12)]               'toggle-messages-buffer)
  (define-key global-map [(meta f12)]                'toggle-python-buffer)
  (define-key global-map [(control shift f12)]       'clear-buffer-jumper-list)

  (format "Edit functions")
  (define-key global-map [(control /)]               'toggle-comment)
  (define-key global-map [(meta backspace)]          'undo)
  (define-key global-map [(meta shift backspace)]    'redo)
  (define-key global-map [(control shift k)]         'kill-prev)
  (define-key global-map [(meta r)]                  'revert-buffer) ;; override move-to-window-line
  (define-key global-map [(meta return)]             'hippie-expand)
  (define-key global-map [(meta shift return)]       'hippie-unexpand)
  (define-key global-map [(control x)(control p)]    'just-one-space)
  (define-key global-map [(control c)(y)]            'popup-yank)

  (format "Search")
  (define-key isearch-mode-map (kbd "C-q")           'isearch-occur)
  (define-key isearch-mode-map (kbd "C-e")           'isearch-forward-at-point)

  (format "Spelling")
  (define-key global-map [(meta p)]                  'ispell-buffer)
  (define-key global-map [(meta control p)]          'toggle-dictionary) ;; override backward-list

  (format "Bookmarks")
  (define-key global-map [(f6)]                      'bookmark-jump)
  (define-key global-map [(shift f6)]                'bookmark-set)
  (define-key global-map [(meta f6)]                 'bookmark-bmenu-list)

  (global-unset-key "\C-z");; C-z dont minimize emacs

  ;; (define-key global-map [(f8)]                   'shell)
  ;; (define-key global-map [f9]                     'compile)
  ;; (define-key global-map [f11]                    'grep)
  )

(provide 'default-keybindings)
;;; default-keybindings.el ends here
