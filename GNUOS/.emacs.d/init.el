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
(require 'init-default)
;;------------------------------------- Package Setting--------------------------------------
(require 'init-packages)
;;------------------------------------- Short Key Setting -----------------------------------
(require 'init-shortkey)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(add-hook (quote asm-mode-hook) t)
 '(company-auto-complete nil)
 '(company-auto-complete-chars (quote (32 41 46)))
 '(company-idle-delay 0.1)
 '(company-show-numbers t)
 '(package-selected-packages
   (quote
    (flycheck counsel-projectile wn-mode better-defaults)))
 '(python-shell-interpreter "python3")
 '(sp-escape-quotes-after-insert nil))
 ;; temporary way to fix a bug that smartparens automatic escaping single quote in c-mode
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(provide 'init)
;;; init.el ends here
