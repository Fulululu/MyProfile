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
;; fix compile error:rx-constituents is void
(require 'rx)

;; Add Packages
(defvar my/packages '(
		      use-package
		      diminish
                      better-defaults
		      hungry-delete
		      smartparens
		      popwin
		      wn-mode
		      pbcopy
		      company
                      company-c-headers
		      ivy
		      swiper
		      counsel
		      flycheck ;;flymake
		      exec-path-from-shell
		      counsel-gtags ;;with patch from elipeLema/emacs-counsel-gtags.git
		      projectile
		      yasnippet
		      yasnippet-snippets
		      ivy-yasnippet
		      anaconda-mode
		      company-anaconda
		      pyenv-mode
		      ))

;; Associate my/packages with package-autoremove function in order to uninstall packages
(setq package-selected-packages my/packages)

;; implement of packages autoinstall
(defun my/packages-installed-p ()
  "Implement of packages autoinstall."
  (loop for pkg in my/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package datpabase...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; ================================ package setting ==================================
;; use-package setting
(eval-when-compile
  (require 'use-package))
(use-package diminish
  :diminish eldoc-mode)
;; built-in hideshow setting
(use-package hideshow
  :diminish hs-minor-mode
  :init
  (add-hook 'c-mode-hook 'hs-minor-mode)
  (add-hook 'c++-mode-hook 'hs-minor-mode)
  :config (setq hs-allow-nesting t))
;; hungry-delete setting
(use-package hungry-delete
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode t))
;; smartparens setting
(use-package smartparens
  :diminish smartparens-mode
  :config (smartparens-global-mode t))
;; popwin setting
(use-package popwin
  :config (popwin-mode t))
;; wn-mode setting
(use-package wn-mode
  :diminish wn-mode
  :config (wn-mode t))
(use-package pbcopy)
;; company setting
(use-package company
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode))
;; ivy setting
(use-package ivy
  :demand
  :diminish ivy-mode
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d ")
  :bind (:map ivy-mode-map
	      ("C-x C-b" . ivy-switch-buffer)
	      ("C-x b" . ivy-switch-buffer)))
;; swiper setting
(use-package swiper
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))
;; counsel setting
(use-package counsel
  :diminish counsel-mode
  :config (counsel-mode t)
  :bind (:map counsel-mode-map
	      ("C-h f" . 'counsel-describe-function)
	      ("C-h v" . 'counsel-describe-variable)
	      ("C-h k" . 'counsel-descbinds)
	      ("M-x" . counsel-M-x)
	      ("C-x C-r" . counsel-recentf)))
;; flycheck setting
(use-package flycheck
  :diminish flycheck-mode
  :init
  (add-hook 'emacs-lisp-mode-hook  #'flycheck-mode)
  (add-hook 'asm-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook #'flycheck-mode)
  (add-hook 'c++-mode-hook #'flycheck-mode)
  (add-hook 'python-mode-hook #'flycheck-mode))
;; exec-path-from-shell setting
(use-package exec-path-from-shell
  ;;:if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))
;; counsel-gtags setting
(use-package counsel-gtags
  :diminish counsel-gtags-mode
  :init
  (add-hook 'asm-mode-hook 'counsel-gtags-mode)
  (add-hook 'c-mode-hook 'counsel-gtags-mode)
  (add-hook 'c++-mode-hook 'counsel-gtags-mode)
  :bind (:map counsel-gtags-mode-map
	      ("C-c d" . 'counsel-gtags-find-definition)
	      ("C-c r" . 'counsel-gtags-find-reference)
	      ("C-c s" . 'counsel-gtags-find-symbol)
	      ("C-c f" . 'counsel-gtags-find-file)
	      ("M-," . 'counsel-gtags-go-backward)
	      ("M-." . 'counsel-gtags-go-forward)))
;; projectile setting
(use-package projectile
  :diminish projectile-mode
  :pin melpa-stable
  :init
  (add-hook 'emacs-lisp-mode-hook  'projectile-mode)
  (add-hook 'asm-mode-hook 'projectile-mode)
  (add-hook 'c-mode-hook 'projectile-mode)
  (add-hook 'c++-mode-hook 'projectile-mode)
  (add-hook 'python-mode-hook 'projectile-mode)
  :config (setq projectile-completion-system 'ivy)
  :bind (:map projectile-mode-map
	      ("s-p" . 'projectile-command-map)
	      ("C-c p" . 'projectile-command-map)))
;; yasnippet and snippets setting
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (add-hook 'c-mode-hook 'yas-minor-mode)
  (add-hook 'c++-mode-hook 'yas-minor-mode)
  (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'asm-mode-hook 'yas-minor-mode)
  :config (yas-reload-all)
  :bind (:map yas-minor-mode-map
	      ("C-c y" . 'ivy-yasnippet)))
;; anaconda-mode setting
(use-package anaconda-mode
  :diminish (anaconda-mode anaconda-eldoc-mode)
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :bind (:map anaconda-mode-map
	      ("C-M-i" . 'anaconda-mode-complete)))
;; pyenv-mode setting
(use-package pyenv-mode
  :diminish pyenv-mode
  :config (pyenv-mode t))


;; ================================ function setting ==================================
;; company-c-headers setting
(defun my/c-mode-hook-func ()
  "Add company-c-headers to company-backends."
  (add-to-list 'company-backends 'company-c-headers))
(add-hook 'c-mode-hook 'my/c-mode-hook-func)
;; company-anaconda setting
(defun my/python-mode-hook-func ()
  "Add company-anaconda to company-backends."
  (add-to-list 'company-backends 'company-anaconda))
(add-hook 'python-mode-hook 'my/python-mode-hook-func)

;; (defun my-annotation-function (candidate)
;;   (let ((description (get-text-property 0 'description candidate)))
;;     (when description
;;       (concat "<" (substring description 0 1) ">"))))
;; (setq company-anaconda-annotation-function 'my-annotation-function)

(provide 'init-packages)
;;; init-packages.el ends here
