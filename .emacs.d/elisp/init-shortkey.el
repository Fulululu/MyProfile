;;; Package --- summary
;;; Commentary:
;; Codes here are all about custom shortkey.


;;; Code:
;; binding open-init-file to <f12>.
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )
(global-set-key (kbd "<f12>") 'open-init-file)

;; short key for swiper
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)

;; short key for yasnippet
(global-set-key (kbd "C-c y") 'yas-minor-mode)

;; shortkey for counsel
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h k") 'counsel-descbinds)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

;; shortkey for ecb
(global-set-key (kbd "<f11>") 'ecb-minor-mode)

(with-eval-after-load 'helm-gtags
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "C-c C-p") 'helm-gtags-pop-stack)
  )

;; shortkey for projectile



(provide 'init-shortkey)
;;; init-shortkey.el ends here
