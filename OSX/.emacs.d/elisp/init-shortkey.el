;;; Package --- summary
;;; Commentary:
;; Codes here are all about custom shortkey.

;;; Code:
;; ;; binding open-init-file to <f12>.
;; (defun open-init-file()
;;   (interactive)
;;   (find-file "~/.emacs.d/init.el")
;;   )
;; (global-set-key (kbd "<f12>") 'open-init-file)

;; ;; ecb
;; ;(global-set-key (kbd "<f11>") 'ecb-minor-mode)

;; comment/uncomment
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-c c") 'toggle-comment-on-line)
;; comment/uncomment area
(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'cc-mode
  (define-key c-mode-map  (kbd "C-c C-c") 'comment-or-uncomment-region)
  (define-key c++-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'makefile-mode
  (define-key makefile-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'python-mode
  (define-key python-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))

;; kill buffer
(defun close-all-buffers ()
  "Kill all buffer."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
	(delq (current-buffer)
	      (remove-if-not '(lambda (x) (or (buffer-file-name x) (eq 'dired-mode (buffer-local-value 'major-mode x)))) (buffer-list)))))

(provide 'init-shortkey)
;;; init-shortkey.el ends here
