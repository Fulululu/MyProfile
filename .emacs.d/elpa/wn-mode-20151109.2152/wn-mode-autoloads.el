;;; wn-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "wn-mode" "wn-mode.el" (23371 25252 998978
;;;;;;  918000))
;;; Generated autoloads from wn-mode.el

(autoload 'wn-select-nth "wn-mode" "\
Select window number N in current frame.

\(fn N &optional SWAP-BUFFERS)" t nil)

(defvar wn-mode nil "\
Non-nil if Wn mode is enabled.
See the `wn-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `wn-mode'.")

(custom-autoload 'wn-mode "wn-mode" nil)

(autoload 'wn-mode "wn-mode" "\
A minor mode that enables quick selection of windows.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; wn-mode-autoloads.el ends here
