;;; Package --- summary
;;; Commentary:
;; Codes here are all about Emacs basic setting.


;;; Code:
;; Set the font size = 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
(set-face-attribute 'default nil :height 160)

;; set backup directory
(setq backup-directory-alist (quote (("." . "~/.emacs/backups"))))
;; Disable auto make backup file
;;(setq make-backup-files nil)

;; disable auto-save
;;(setq auto-save-default nil)

;; enable recent files record
(recentf-mode t)

;; 将删除功能配置成:当你选中一段文字之后输入一个字符会替换掉你选中部分的文字。
(delete-selection-mode t)

;; Enable Recursive Minibuffers
;;(setq enable-recursive-minibuffers t)

;; hs-minor-mode setting
(require 'hideshow)
(setq hs-allow-nesting t)
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)

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
