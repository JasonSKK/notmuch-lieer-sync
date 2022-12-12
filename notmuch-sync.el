;;; notmuch-lieer-sync.el --- Easy Notmuch - lieer sync Emacs

;; Copyright (C) 2022 Iason SK

;; Author: Iason SK <jason.skk98[at]gmail[dot]>
;; Keywords: notmuch, lieer

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a setup configuration for effortless syncing between notmuch and lieer using a simple Elisp function
;; it nerved me a lot that every time I wanted to fetch new email using lieer I had to leave Emacs (I do not use shell within Emacs) and then run “gmi sync”.  So, I wrote this.

;;; Code:

;; shell command output no window
(defun no-output-shell-run (command)
  "Run shell COMMAND without displaying the output.  First ARG is COMMAND."
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

;; sync notmuch using lieer and notmuch new
(defun notmuch-sync ()
  "Syncs notmuch using lieer.  ARG empty: Configuration file is /Users/+++/Mail/gmi-sync.sh."
  (interactive)
  (let ((lnr (line-number-at-pos))) ;; register cursor line number
    (no-output-shell-run "pushd /Users/+++/Mail ; gmi sync ; popd ; notmuch new")
    (notmuch-refresh-all-buffers) ;; refresh all not much buffers
    (goto-line lnr) ;; go to registered line number
    (message "notmuch & lieer on sync")))

;; (global-set-key (kbd "C-l s") 'notmuch-sync) ;; global
;; mode-specific, notmuch-search-mode
(add-hook 'notmuch-search-mode-hook
          '(lambda ()
             (define-key notmuch-search-mode-map (kbd ".") 'notmuch-sync)))
;; mode specific notmuch-hello-mode
;; fetch email every X sec | period is defined in notmuch-sync-period
(add-hook 'notmuch-hello-mode-hook
          '(lambda ()
             (run-with-timer 0 notmuch-sync-period  ;; every 25 sec fetch email
                             '(lambda ()
                                (if
                                    (get-buffer "*notmuch-hello*")  ;; if notmuch buffer exists fetch email
                                    (no-output-shell-run "pushd /Users/+++/Mail && gmi sync && popd && notmuch new")
                                  (cancel-function-timers "no-output-shell-run"))))))  ;; cancel timer if buffer does not exist

;; add this to your init to define fetching period
;; (setq notmuch-sync-period 25) ;; every 25 sec


(provide 'notmuch-sync)
;;; notmuch-sync.el ends here
