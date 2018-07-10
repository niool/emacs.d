;; Sandbox area to test some ideas of Emacs
(defun hello (name)
  (insert (format "Sandbox: %s\n" name)))

;; Translate current words by youdao
(defvar base-youdao-url "http://fanyi.youdao.com/openapi.do?keyfrom=emacs-yd-pub&key=527593631&type=data&doctype=json&version=1.1&q=")

;; Get yourself an API KEY here: http://fanyi.youdao.com/openapi?path=data-mode
(defun youdao-fanyi ()
  "Translate current word (en->cn, cn->en), prompt to input if no word here"
  (interactive)
  (let* ((word (or (thing-at-point 'word) (read-string "Translate> ")))
         (full-url (concat base-youdao-url word)))
    (with-current-buffer (url-retrieve-synchronously full-url)
      (unwind-protect
          (progn
            (goto-char (point-min))
            (re-search-forward "^$")
            (delete-region (point) (point-min)) ;strip headers
            (message
             (elt (cdar ;we just want the straight one
                   (json-read-from-string
                    (decode-coding-string
                     (buffer-string) 'utf-8)))
                  0)))
        (kill-buffer)))))
(global-set-key "\C-c\ \C-f" 'youdao-fanyi)

; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)