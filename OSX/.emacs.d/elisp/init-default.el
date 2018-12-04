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
(global-linum-mode t)
;; Highlight current line
;;(global-hl-line-mode t)
;; Highlight line color
;(set-face-background 'hl-line' "#333333")
;; Modify cursor type
(setq-default cursor-type 'bar)
;; Highlight Matching Parentheses
(show-paren-mode t)
;; Lock when scroll
;;(scroll-lock-mode t)
;; Set the font size = 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
(set-face-attribute 'default nil :height 160)
;; set backup directory
(setq backup-directory-alist (quote (("." . "~/.emacs/backups"))))
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
;; semantic setting
(require 'semantic/sb)
;; turn off abbrev mode in some mode
(defun turn-off-abbrev-mode()
  "my function, using in C or other mode to turn off abbrev-mode"
  (interactive)
  (abbrev-mode 0))
(add-hook 'c-mode-hook 'turn-off-abbrev-mode)
(add-hook 'c++-mode-hook 'turn-off-abbrev-mode)
;; Tab setting
(setq c-default-style "linux"
      c-basic-offset 4)


(provide 'init-default)
;;; init-default.el ends here
