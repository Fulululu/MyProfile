;;; Package --- summary
;;; Commentary:
;; Codes here are all about pluging packages.

;;; Code:
(when (>= emacs-major-version 24)
     (require 'package)
     (setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
			      ("melpa" . "http://elpa.emacs-china.org/melpa/")
			      ("melpa-stable" . "http://elpa.emacs-china.org/melpa/stable")
			      )))
;;elpa.emacs-china.org is a mirror of ELPA

;; cl - Common Lisp Extension
(require 'cl)
;; fix compile error:rx-constituents is void
(require 'rx)

;; Packages List
(defvar my/packages '(
		      ;; Common
		      use-package
		      diminish
                      better-defaults
		      hungry-delete
		      smartparens
		      popwin
		      wn-mode
		      pbcopy
		      ivy
		      swiper
		      counsel
		      ;; For completion and programeing features
		      company
		      flycheck ;;flymake
		      projectile
		      yasnippet
		      yasnippet-snippets
		      ivy-yasnippet
		      magit ;; (include git-commit, transient, with-editor and async)
		      lsp-mode
		      company-lsp
		      lsp-ui
		      ;; For Shell
		      exec-path-from-shell
		      ;; For C/C++
		      ccls
		      google-c-style
		      ;; For Python
		      ;; For CMake
		      cmake-mode
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

;; ================================ Package Setting ==================================
;; use-package setting
(eval-when-compile
  (require 'use-package))
(use-package diminish
  :commands diminish
  :diminish eldoc-mode)

;; built-in hideshow setting
(use-package hideshow
  :diminish hs-minor-mode
  :hook ((c-mode c++mode) . hs-minor-mode)
  :config (setq hs-allow-nesting t))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode t))

(use-package smartparens
  :diminish smartparens-mode
  :config (smartparens-global-mode t))

(use-package popwin
  :config (popwin-mode t))

(use-package wn-mode
  :diminish wn-mode
  :config (wn-mode t))

(use-package pbcopy)

(use-package company
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode))

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

(use-package swiper
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))

(use-package counsel
  :diminish counsel-mode
  :init (counsel-mode t)
  :bind (:map counsel-mode-map
	      ("C-h f" . 'counsel-describe-function)
	      ("C-h v" . 'counsel-describe-variable)
	      ("C-h k" . 'counsel-descbinds)
	      ("M-x" . counsel-M-x)
	      ("C-x r" . counsel-recentf)))

(use-package flycheck
  :commands flycheck-mode
  :hook ((emacs-lisp-mode asm-mode c-mode c++-mode python-mode) . flycheck-mode))

(use-package projectile
  :commands projectile-mode
  :diminish projectile-mode
  :pin melpa-stable
  :hook ((emacs-lisp-mode c-mode c++-mode python-mode asm-mode) . projectile-mode)
  :config (setq projectile-completion-system 'ivy)
  :bind (:map projectile-mode-map
	      ("s-p" . 'projectile-command-map)
	      ("C-c p" . 'projectile-command-map)))

(use-package yasnippet
  :commands yas-minor-mode
  :diminish yas-minor-mode
  :hook ((c-mode c++-mode python-mode asm-mode sh-mode) . yas-minor-mode)
  :config (yas-reload-all)
  :bind (:map yas-minor-mode-map
	      ("C-c y" . 'ivy-yasnippet)))

(use-package magit
  :config (setq magit-view-git-manual-method 'man))

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package lsp-mode
  :commands lsp
  :hook (sh-mode . lsp)
  :init (setq lsp-prefer-flymake nil)
  :bind (:map lsp-mode-map
	      ("C-c d" . 'lsp-find-definition)
	      ("C-c r" . 'lsp-find-references)
	      ("C-c i" . 'lsp-find-implementation)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :commands company-lsp
  :init (add-to-list 'company-backends 'company-lsp))

(use-package ccls
  :init
  (setq ccls-executable "/usr/local/bin/ccls")
  (setq ccls-initialization-options `(:compilationDatabaseDirectory "build"))
  (setq ccls-sem-highlight-method 'font-lock) ;; enable semantic highlighting
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode) . (lambda () (require 'ccls) (lsp))))

(use-package google-c-style
  :hook ((c-mode c++-mode) . google-set-c-style))

;; ================================ Custom Functions ==================================
;; Shell mode
;(use-package shell)
;; C/C++ mode
;(use-package cc-mode)
;; Python mode
;(use-package python)

(provide 'init-packages)
;;; init-packages.el ends here
