;;; Package --- summary
;;; Commentary:
;; Codes here are all about Emacs UI.


;;; Code:
;; hide the startup message
(setq inhibit-splash-screen t)

;; turn off tool bar
(tool-bar-mode 0)

;; turn off menu bar
(menu-bar-mode 0)

;; Set Full Screen when open emacs
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; global show linum
(global-linum-mode t)

;; Highlight current line
;(global-hl-line-mode t)

;; Highlight line color
;(set-face-background 'hl-line' "#333333")

;; Modify cursor type
(setq-default cursor-type 'bar)

;; Highlight Matching Parentheses
(show-paren-mode t)

;; Lock when scroll
;(scroll-lock-mode t)


(provide 'init-ui)
;;; init-ui.el ends here
