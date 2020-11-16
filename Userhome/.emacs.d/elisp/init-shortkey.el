;;; Package --- summary
;;; Commentary:
;; Codes here are all about custom shortkey.

;;; Code:

;; Comment/uncomment
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-c c") 'toggle-comment-on-line)
;; Comment/uncomment area
(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'cc-mode
  (define-key c-mode-map  (kbd "C-c C-c") 'comment-or-uncomment-region)
  (define-key c++-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'makefile-mode
  (define-key makefile-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'python-mode
  (define-key python-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'cmake-mode
  (define-key cmake-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'php-mode
  (define-key php-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'dockerfile-mode
  (define-key dockerfile-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'docker-compose-mode
  (define-key docker-compose-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'protobuf-mode
  (define-key protobuf-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'json-mode
  (define-key json-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'yaml-mode
  (define-key yaml-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))

;; Kill buffer
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

;; Enable disabled shortkey
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init-shortkey)
;;; init-shortkey.el ends here
