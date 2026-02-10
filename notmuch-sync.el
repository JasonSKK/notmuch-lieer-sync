;;; -*- lexical-binding: t; -*-
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

;; This file provides an asynchronous Elisp setup for syncing notmuch with lieer.
;; Email sync is triggered directly from Emacs.
;; The function is not blocking Emacs via simply starting an async process in the background.
;; The implementation avoids the old shell wrapper approach (863164d25b3e5589c8b76f6fca1f055afcc991f9).
;; It relies on the standard `notmuch new` philosophy together with the pre-new hook for lieer.
;; Debug output and errors are now shown.
;; Ahh... finally done correctly

;;; Code:
;; lexical binding
(require 'subr-x)

(defvar notmuch-sync--process nil
  "Current notmuch sync process.")

  (defun notmuch-sync ()
    "Run `notmuch new` from notmuch-serch buffer asynchronously with proper error reporting."
    (interactive)
    (when (process-live-p notmuch-sync--process)
      (user-error "Notmuch sync already running"))

    (let* ((lnr (line-number-at-pos))
           (srcbuf (current-buffer))
           (outbuf (get-buffer-create "*notmuch-sync*")))
      (with-current-buffer outbuf
        (erase-buffer)
        (insert (format "[%s] starting: notmuch new\n\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S"))))

      (setq notmuch-sync--process
            (make-process
             :name "notmuch-sync"
             :buffer outbuf
             :command (list "notmuch" "new")
             :noquery t
             :sentinel
             (let ((outbuf outbuf)
                   (srcbuf srcbuf)
                   (lnr lnr))
               (lambda (proc event)
                 (when (memq (process-status proc) '(exit signal))
                   (let ((code (process-exit-status proc)))
                     (with-current-buffer outbuf
                       (goto-char (point-max))
                       (insert (format "\n[%s] finished: %s (exit %d)\n"
                                       (format-time-string "%Y-%m-%d %H:%M:%S")
                                       (string-trim event)
                                       code)))
                     (setq notmuch-sync--process nil)
                     (if (eq code 0)
                         (when (buffer-live-p srcbuf)
                           (with-current-buffer srcbuf
                             (notmuch-refresh-this-buffer)
                             (goto-line lnr)
                             (message "Notmuch sync OK at %s"
                                      (format-time-string "%Y-%m-%d %H:%M:%S"))))
                       (display-buffer outbuf)
                       (message "Notmuch sync FAILED, see *notmuch-sync*"))))))))))

;; notmuch-sync hook
;; Sync inside a notmuch-search buffer via "." key
(add-hook 'notmuch-search-mode-hook
          '(lambda ()
             (define-key notmuch-search-mode-map (kbd ".") 'notmuch-sync)))

(provide 'notmuch-sync)
;;; notmuch-sync.el ends here
