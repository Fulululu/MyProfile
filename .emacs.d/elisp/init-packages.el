;;; Package --- summary
;;; Commentary:
;; Codes here are all about pluging packages.

;;; Code:
(when (>= emacs-major-version 24)
     (require 'package)
     (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			      ("melpa" . "http://elpa.emacs-china.org/melpa/")
			      ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/"))))
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
		      flycheck ;;flymake
		      yasnippet
		      ivy-yasnippet
		      yasnippet-snippets
		      popwin
		      wn-mode
		      ;;cscope
		      ;;ecb
		      ;;counsel-projectile
                      helm-gtags
		      elpy
		      py-autopep8
		      ;;sublimity
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

;; company setting
(global-company-mode t)

;; company-c-headers setting
(add-to-list 'company-backends 'company-c-headers)

;; hungry-delete setting
(global-hungry-delete-mode t)

;; ivy setting
(ivy-mode t)
(setq ivy-use-virtual-buffers t)

;; smartparens setting
(smartparens-global-mode t)

;; flycheck setting
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'python-mode 'flycheck-mode)

;; yasnippet and snippets setting
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
	))
(add-hook 'c-mode-hook 'yas-minor-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)

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
;;(cscope-setup)

;; Emacs Codes Browser setting
;;(add-to-list 'load-path "~/.emacs.d/elpa/ecb-master")
;;(require 'ecb)
;;(setq ecb-tip-of-the-day nil)
;;(add-hook 'c-mode-hook 'ecb-minor-mode)
;;(add-hook 'c++-mode-hook 'ecb-minor-mode)

;; elpy setting
(add-hook 'python-mode-hook 'elpy-mode)
(add-hook 'elpy-mode-hook 'flycheck-mode)
;(defun elpy-use-flycheck()
;  "my function, it used to replace flymake with flycheck in elpy mode"
;  (interactive)
;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))
;(eval-after-load 'elpy-mode 'elpy-use-flycheck)

;; py-autopep8
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(provide 'init-packages)
;;; init-packages.el ends here
