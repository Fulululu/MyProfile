;;; Package --- summary
;;; Commentary:
;; Codes here are all about pluging packages.

;;; Code:
(when (>= emacs-major-version 24)
     (require 'package)
     (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			      ("melpa" . "http://elpa.emacs-china.org/melpa/"))))
;;elpa.emacs-china.org is a mirror of ELPA

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
                      better-defaults
		      company
                      company-c-headers
		      hungry-delete
		      swiper
		      counsel
		      smartparens
		      flycheck ;;better than flymake
		      yasnippet
		      ;;yasnippet-snippets
		      popwin
		      wn-mode
		      ;;cscope
		      ;;ecb
		      ;;counsel-projectile
                      helm-gtags
		      ;;-------denpendency-------
		      epl
		      ivy
                      helm
		      dash
		      pkg-info
		      ))

;; Associate my/packages with package-autoremove function in order to uninstall packages
(setq package-selected-packages my/packages)

;; implement of packages autoinstall
 (defun my/packages-installed-p ()
     (loop for pkg in my/packages
	   when (not (package-installed-p pkg)) do (return nil)
	   finally (return t)))

 (unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
	 (package-install pkg))))

;; better-defaults setting
(require 'better-defaults)

;; company setting
(require 'company)
(global-company-mode t)

;; company-c-headers setting
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

;; hungry-delete setting
(autoload global-hungry-delete-mode "hungry-delete-autoloads")
(global-hungry-delete-mode t)

;; ivy setting
(autoload ivy-mode "ivy-autoloads")
(ivy-mode t)
(setq ivy-use-virtual-buffers t)

;; smartparens setting
(require 'smartparens-config)
(smartparens-global-mode t)

;; flycheck setting
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)

;; yasnippet and snippets setting
;;(autoload 'yas-minor-mode "yasnippet-autoloads")
;;(add-hook 'c-mode-hook 'yas-minor-mode)

;; popwin setting
(require 'popwin)
(popwin-mode t)

;; wn-mode setting
(wn-mode t)

;; helm-gtags setting
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; cscope setting
;;(require 'xcscope)
;;(cscope-setup)

;; Emacs Codes Browser setting
;;(add-to-list 'load-path "~/.emacs.d/elpa/ecb-master")
;;(require 'ecb)
;;(setq ecb-tip-of-the-day nil)
;;(add-hook 'c-mode-hook 'ecb-minor-mode)
;;(add-hook 'c++-mode-hook 'ecb-minor-mode)

(provide 'init-packages)
;;; init-packages.el ends here
