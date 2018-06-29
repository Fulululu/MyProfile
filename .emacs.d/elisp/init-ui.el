;;; Package --- summary
;;; Commentary:
;; Codes here are all about Emacs UI.


;;; Code:
;; 关闭启动帮助画面
(setq inhibit-splash-screen t)

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode 0)

;; Set Full Screen when open emacs
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; 关闭文件滑动控件
(scroll-bar-mode 0)

;; 显示行号
(global-linum-mode t)

;; Highlight current line
(global-hl-line-mode t)

;; Highlight line color
(set-face-background 'hl-line' "#333333")

;; 更改光标的样式
(setq-default cursor-type 'bar)

;; Highlight Matching Parentheses
(show-paren-mode t)


(provide 'init-ui)
;;; init-ui.el ends here
