;;; Package --- summary
;;; Commentary:
;; Codes here are all about custom shortkey.

;;; Code:

;; comment/uncomment single line. FIXME: May need to move to init-package.el when there is a conflict with key "C-c c".
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-c c") 'toggle-comment-on-line)

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
