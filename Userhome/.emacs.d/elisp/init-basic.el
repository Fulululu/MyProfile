;;; Package --- summary
;;; Commentary:
;; Codes here are all about Emacs basic setting.

;;; Code:
;; Something according to existance of window-system
(if (equal window-system nil)
    ;; Character-only environment
    ;; Turn off menu bar
    (progn
      (menu-bar-mode 0)
      (add-to-list 'default-frame-alist '(background-color . "black"))
     )
  ;; Window environment
  ;; Set full screen when open emacs
  (setq initial-frame-alist (quote ((fullscreen . maximized))))
  )

;; Default coding system
;; FIXME: There is a bug that this setting does take effect when using emacsclient.
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

(provide 'init-basic)
;;; init-basic.el ends here
