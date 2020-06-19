;;; package --- summary

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/elisp")

;;; commentary:
;; version:1.0

;;; code:
;;------------------------------------- Basic Setting ---------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(delete-selection-mode t)
 '(display-buffer-alist nil)
 '(global-auto-revert-mode t)
 '(global-display-line-numbers-mode t)
 '(inhibit-default-init t)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-echo-area-message "")
 '(inhibit-startup-screen t)
 '(mouse-wheel-progressive-speed nil)
 '(package-selected-packages (quote (company-lsp use-package diminish bind-key)))
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'init-basic)
;;------------------------------------- Package Setting--------------------------------------
(require 'init-packages)
;;------------------------------------- Short Key Setting -----------------------------------
(require 'init-shortkey)

(provide 'init)
;;; init.el ends here

