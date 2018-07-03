;; This is my 3rd time to trigger Emacs.
;; I like the philosohy of Emacs. But the good starter would be GTD practice based on
;; org-mode.
;;
;; I followed: http://doc.norang.ca/org-mode.html to setup my own working env.

;; ============================================================================
;; Org mode
(setq org-directory "~/WorkSpace/GTD/")
(setq org-agenda-files (list "~/WorkSpace/GTD/MyGTD.org"
                             "~/WorkSpace/GTD/myagendas.org"
                             "~/WorkSpace/GTD/tasks.org"
                             "~/WorkSpace/GTD/finished.org"
                             "~/WorkSpace/GTD/projects.org" ))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :wight bold)
              ("NEXT" :foreground "blue" :wight bold)
              ("DONE" :foreground "forest green" :wight bold)
              ("WAITING" :foreground "orange" :wight bold)
              ("HOLD" :foreground "magenta" :wight bold)
              ("CANCELLED" :foreground "forest green" :wight bold)
              ("PHONE" :foreground "forest green" :wight bold)
              ("MEETING" :foreground "forest green" :wight bold)
              )))

;; Triggers for auto tagging altering of task status
;; - Moving a task to CANCELLED adds a CANCELLED tag
;; - Moving a task to WAITING adds a WAITING tag
;; - Moving a task to HOLD adds WAITING and HOLD tags
;; - Moving a task to a done state removes WAITING and HOLD tags
;; - Moving a task to TODO removes WAITING, CANCELLED and HOLD tags
;; - Moving a task to NEXT removes WAITING, CANCELLED and HOLD tags
;; - Moving a task to DONE removes WAITING, CANCELLED and HOLD tags

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("CANCELLED" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))
              )))

;; ============================================================================
;; Org-Capture mode
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates nil)

(add-to-list 'org-capture-templates
             '("n" "Notes" entry (file "~/WorkSpace/GTD/inbox.org")
               "* %^{heading} %t %^g\n  %?\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
             '("i" "Inbox" entry (file "~/WorkSpace/GTD/inbox.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
             '("r" "respond" entry (file "~/WorkSpace/GTD/MyGTD.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t))
(add-to-list 'org-capture-templates
             '("b" "Book Reading Task" entry (file+olp "~/WorkSpace/GTD/MyGTD.org" "Reading" "Book")
               "* TODO %^{Book Name}\n%u\n%a\n" :clock-in t :clock-resume t))

(add-to-list 'org-capture-templates
             '("w" "org-protocol" entry (file "~/WorkSpace/GTD/inbox.org")
               "* TODO Review %c\n%U\n" :immediate-finish t))

(add-to-list 'org-capture-templates
             '("m" "Meeting" entry (file "~/WorkSpace/GTD/inbox.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
              '("p" "Phone call" entry (file "~/WorkSpace/GTD/inbox.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
             '("h" "Habit" entry (file "~/WorkSpace/GTD/inbox.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))

(add-to-list 'org-capture-templates
             '("t" "Work Task" entry (file+headline "~/WorkSpace/GTD/inbox.org" "Capture Tasks")
               "* TODO %^{Task Name}\n\tCreated:%u\n%a\n" :clock-in t :clock-resume t))

(add-to-list 'org-capture-templates
             '("j" "Journal" entry (file+function "~/WorkSpace/GTD/journals.org" find-month-tree)
               "** %U - %^{heading}\n  %?" :clock-in t :clock-resume t :kill-buffer t))
(add-to-list 'org-capture-templates
             '("b" "Billing" plain (file+function "~/WorkSpace/GTD/billing.org" find-month-tree)
               " | %U | %^{类别} | %^{描述} | %^{金额} |" :kill-buffer t))

(defun get-year-and-month ()
  (list (format-time-string "%Y Year") (format-time-string "%m Month")))

(defun find-month-tree ()
  (let* ((path (get-year-and-month))
         (level 1)
         end)
    (unless (derived-mode-p 'org-mode)
      (error "Target buffer \"%s\" should be in Org mode" (current-buffer)))
    (goto-char (point-min))             ;移动到 buffer 的开始位置
    ;; 先定位表示年份的 headline，再定位表示月份的 headline
    (dolist (heading path)
      (let ((re (format org-complex-heading-regexp-format
                        (regexp-quote heading)))
            (cnt 0))
        (if (re-search-forward re end t)
            (goto-char (point-at-bol))  ;如果找到了 headline 就移动到对应的位置
          (progn                        ;否则就新建一个 headline
            (or (bolp) (insert "\n"))
            (if (/= (point) (point-min)) (org-end-of-subtree t t))
            (insert (make-string level ?*) " " heading "\n"))))
      (setq level (1+ level))
      (setq end (save-excursion (org-end-of-subtree t t))))
    (org-end-of-subtree)))

(setq org-default-notes-file (concat org-directory "/inbox.org"))


;; ============================================================================
;; Org refilling tasks / configuration

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)


;; ============================================================================
;; Org-Agenda mode
(setq org-agenda-exporter-settings
      '((ps-number-of-columns 1)
        (ps-landscape-mode t)
        (htmlize-output-type 'css)))

(setq org-agenda-custom-commands
'(

("P" "Projects"
((tags "PROJECT")))

("H" "Office and Home Lists"
     ((agenda)
          (tags-todo "OFFICE")
          (tags-todo "HOME")
          (tags-todo "FEIER")
          (tags-todo "READING")))

("D" "Daily Action List"
     (
          (agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)
                      ))))
)
)

(defun gtd ()
    (interactive)
    (find-file "~/WorkSpace/GTD/MyGTD.org")
    )

(defun myinbox ()
    (interactive)
    (find-file "~/WorkSpace/GTD/inbox.org")
    )

(global-set-key (kbd "C-c g") 'gtd)
(global-set-key (kbd "C-c i") 'myinbox)

(add-hook 'org-agenda-mode-hook 'hl-line-mode)


;; ============================================================================
;; MobileOrg Configuration
;; https://blog.csdn.net/lujun9972/article/details/46002799

(setq org-mobile-directory "~/Emacs/syncdirs/org")
(setq org-mobile-files (list "~/WorkSpace/GTD/MyGTD.org"
                             "~/WorkSpace/GTD/myagendas.org" ))

;; 当要把MobileOrg所做的修改同步到电脑端Org时,电脑端Org会先把MobileOrg的修改动作记录到
;; 该变量指定的文件中,然后再根据该文件中所记录的操作对电脑端Org进行修改
(setq org-mobile-inbox-for-pull "~/WorkSpace/GTD/inbox.org")

(defcustom org-mobile-checksum-binary (or (executable-find "~/emacs/bin/md5sums.exe"))
 "Executable used for computing checksums of agenda files."
 :group 'org-mobile
 :type 'string)

;; ============================================================================
;; Org-Mode Project Publish Configuration

(require 'ox-publish)
(setq org-publish-project-alist
      '(("note-org"
         :base-directory "~/WorkSpace/GTD/"
         :publishing-directory "~/Emacs/public_html"
         :base-extension "org"
         :recursive t
         :publishing-function org-publish-org-to-html
         :auto-index nil
         :index-filename "index.org"
         :index-title "index"
         :link-home "index.html"
         :section-numbers nil
         :style "<link rel=\"stylesheet\" href=\"./style/emacs.css\" type=\"text/css\"/>")

        ("note-static"
         :base-directory "~/WorkSpace/GTD/"
         :publishing-directory "~/Emacs/public_html"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|swf\\|zip\\|gz\\|txt\\|el"
         :publishing-function org-publish-attachment)

        ("note"
         :components ("note-org" "note-static")
         :author "JiangHua")

        ("hello-world"
         :base-directory "~/Emacs/SandBox/"
         :publishing-directory "~/Emacs/public_html"
         :base-extension "org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-index nil
         :index-filename "SandBox.org"
         :index-title "index"
         :link-home "sandbox.html"
         :section-numbers nil
         :style "<link rel=\"stylesheet\" href=\"./style/emacs.css\" type=\"text/css\"/>")

        ))
