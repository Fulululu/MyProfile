;;; Package --- summary
;;; Commentary:
;; Codes here are all about Emacs basic setting.

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
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
;; Highlight current line
;;(global-hl-line-mode t)
;; Highlight line color
;(set-face-background 'hl-line' "#333333")
;; Modify cursor type
(setq-default cursor-type 'bar)
;; Highlight Matching Parentheses
(show-paren-mode t)
;; Disable scroll bar
(scroll-bar-mode -1)
;; Lock when scroll
;;(scroll-lock-mode t)
;; set backup directory
(setq backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
;; Disable auto make backup file
;;(setq make-backup-files nil)
;; disable auto-save
(setq auto-save-default nil)
;; enable recent files record
(recentf-mode t)
;; 将删除功能配置成:当你选中一段文字之后输入一个字符会替换掉你选中部分的文字。
(delete-selection-mode t)
;; Enable Recursive Minibuffers
;;(setq enable-recursive-minibuffers t)
;; revert when files changed
(global-auto-revert-mode t)

(provide 'init-default)
;;; init-default.el ends here
