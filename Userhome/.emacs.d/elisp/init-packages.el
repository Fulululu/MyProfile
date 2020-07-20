;;; Package --- summary
;;; Commentary:
;; Codes here are all about pluging packages.

;;; Code:
;; 1. Setting package source to mirror of GNU and ELPA
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.emacs-china.org/gnu/")
			 ("melpa" . "https://elpa.emacs-china.org/melpa/")))

;; 2. Setting package manager
(require 'cl)
(defvar my/packages '(use-package diminish bind-key))

;; Associate my/packages with package-autoremove function in order to uninstall packages
(setq package-selected-packages my/packages)

;; Implement of packages autoinstall
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

;; use-package setting
(eval-when-compile
  (require 'use-package)
  (require 'diminish)
  (require 'bind-key))

;; ================================ Built-in Package Setting ==================================
(use-package hideshow
  :diminish hs-minor-mode
  :hook ((c-mode c++mode) . hs-minor-mode)
  :config (setq hs-allow-nesting t))

(use-package flymake
  ;; Disable "Warning [flymake init.el]: Disabling backend flymake-proc-legacy-flymake because
  ;; (error Can’t find a suitable init function) " in *Flymake log* buffer
  :config (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

;; ================================ 3th-party Package Setting ==================================
(use-package diminish
  :commands diminish
  :diminish (eldoc-mode abbrev-mode))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode t))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config (smartparens-global-mode t))

(use-package popwin
  :ensure t
  :config (popwin-mode t))

(use-package wn-mode
  :ensure t
  :diminish wn-mode
  :config (wn-mode t))

(use-package pbcopy
  :ensure t)

(use-package company
  :ensure t
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 1
	company-backends '(company-capf))
)

(use-package ivy
  :ensure t
  :pin melpa
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
  :ensure t
  :bind (("C-s" . swiper)
  	 ("C-r" . swiper)))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :init (counsel-mode t)
  :bind (:map counsel-mode-map
	      ("C-h f" . 'counsel-describe-function)
	      ("C-h v" . 'counsel-describe-variable)
	      ("C-h b" . 'counsel-descbinds)
	      ("M-x" . counsel-M-x)
	      ("C-x r" . counsel-recentf)))

(use-package projectile
  :ensure t
  :commands projectile-mode
  :diminish projectile-mode
  :hook ((emacs-lisp-mode c-mode c++-mode python-mode asm-mode) . projectile-mode)
  :config (setq projectile-completion-system 'ivy)
  :bind (:map projectile-mode-map
	      ("s-p" . 'projectile-command-map)
	      ("C-c p" . 'projectile-command-map))
  )

(use-package yasnippet
  :ensure t
  :pin melpa
  :commands yas-minor-mode
  :diminish yas-minor-mode
  :hook ((c-mode c++-mode python-mode asm-mode sh-mode go-mode) . yas-minor-mode)
  :config (yas-reload-all)
  :bind (:map yas-minor-mode-map
	      ("C-c & y" . 'ivy-yasnippet))
  )

(use-package yasnippet-snippets
  :ensure t)

(use-package magit
  :ensure t
  :config (setq magit-view-git-manual-method 'man))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((sh-mode go-mode) . lsp-deferred)
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  )

(use-package ccls
  :ensure t
  :init
  (setq ccls-executable "/usr/local/bin/ccls")
  ;; using "clang -v -fsyntax-only -x c++ /dev/null" to get extraArgs
  (if (equal system-type 'darwin)
      (setq ccls-initialization-options `(:cache (:directory "/tmp/ccls-cache")
					  :index (:comments 2)
					  :compilationDatabaseDirectory "build"
					  :completion (:detailedLabel t)
					  :clang (:extraArgs ["-isystem/usr/local/include"
							      "-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1/"
							      "-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/11.0.0/include"
							      "-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
							      "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/System/Library/Frameworks"
							      "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"])))
    (if (equal system-type 'gnu/linux))
    )
  (setq ccls-sem-highlight-method 'font-lock) ;; enable semantic highlighting
  :hook ((c-mode c++-mode) . (lambda () (require 'ccls) (lsp))))

(use-package google-c-style
  :ensure t
  :hook ((c-mode c++-mode) . google-set-c-style))

(use-package go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook (lambda()
			    (add-hook 'before-save-hook #'lsp-format-buffer t t)
			    (add-hook 'before-save-hook #'lsp-organize-imports t t)
			    (lsp-register-custom-settings '(("gopls.completeUnimported" t t) ("gopls.staticcheck" t t))))))

(use-package dockerfile-mode
  :ensure t
  :init
  (add-to-list 'load-path "~/.emacs.d/elpa/dockerfile-mode/")
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package docker-compose-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package cmake-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(provide 'init-packages)
;;; init-packages.el ends here
