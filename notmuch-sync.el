;; shell command output no window
(defun no-output-shell-run (command)
  "Run shell COMMAND without displaying the output.  First ARG is COMMAND."
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

;; sync notmuch using lieer and notmuch new -- old version using a script
;; (defun notmuch-sync ()
;;   "Syncs notmuch using lieer.  ARG empty: Configuration file is /Users/+++/Mail/gmi-sync.sh."
;;   (interactive)
;;   (let ((lnr (line-number-at-pos))) ;; register cursor line number
;;     (no-output-shell-run "bash /Users/+++/Mail/gmi-sync.sh /Users/+++/Mail")
;;     ;;(shell-command "bash /Users/+++/Mail/gmi-sync.sh") ;; new window output
;;     (notmuch-refresh-all-buffers) ;; refresh all not much buffers
;;     (goto-line lnr))) ;; go to registered line number

;; sync notmuch using lieer and notmuch new -- without the script
(defun notmuch-sync ()
  "Syncs notmuch using lieer.  ARG empty: Configuration file is /Users/+++/Mail/gmi-sync.sh."
  (interactive)
  (let ((lnr (line-number-at-pos))) ;; register cursor line number
    (no-output-shell-run "pushd /Users/+++/Mail ; gmi sync ; popd ; notmuch new")
    ;; "bash /Users/+++/Mail/gmi-sync.sh /Users/+++/Mail") ;; using the script
    ;;(shell-command "bash /Users/+++/Mail/gmi-sync.sh") ;; run shell script
    (notmuch-refresh-all-buffers) ;; refresh all not much buffers
    (goto-line lnr) ;; go to registered line number
    (message "notmuch & lieer on sync")))

;; (global-set-key (kbd "C-l s") 'notmuch-sync) ;; global
;; mode-specific, notmuch-search-mode
(add-hook 'notmuch-search-mode-hook
          '(lambda ()
             (define-key notmuch-search-mode-map (kbd ".") 'notmuch-sync)))
