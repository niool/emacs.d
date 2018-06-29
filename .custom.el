;; Please note the color theme's name is "molokai"
(when (or (display-graphic-p)
          (string-match-p "256color"(getenv "TERM")))
  (load-theme 'molokai t))

(setq default-frame-alist
      '((height . 50) (width . 100) (menu-bar-lines . 20) (tool-bar-lines . 0)))

;; ============================================================================
;; Set all coding global parameter to utf-8
;; Fix issue "Bad file encoding" in MobileOrg
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; ============================================================================
;; Org mode
(setq org-directory "~/Emacs/mybook/org/")
(setq org-agenda-files (list "~/Emacs/mybook/org/MyGTD.org"
                             "~/Emacs/mybook/org/myagendas.org"
                             "~/Emacs/mybook/org/inbox.org"
                             "~/Emacs/mybook/org/task.org"
                             "~/Emacs/mybook/org/finished.org"
                             "~/Emacs/mybook/org/project.org" ))


;; ============================================================================
;; Org-Capture mode
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates nil)

(add-to-list 'org-capture-templates
             '("n" "Notes" entry (file "~/Emacs/mybook/org/inbox.org")
               "* %^{heading} %t %^g\n  %?\n"))
(add-to-list 'org-capture-templates
             '("j" "Journal" entry (file "~/Emacs/mybook/org/journals.org")
               "* %U - %^{heading}\n  %?"))
(add-to-list 'org-capture-templates
             '("i" "Inbox" entry (file "~/Emacs/mybook/org/inbox.org")
               "* %U - %^{heading} %^g\n %?\n"))

(add-to-list 'org-capture-templates
             '("r" "Book Reading Task" entry
               (file+olp "~/Emacs/mybook/org/MyGTD.org" "Reading" "Book")
               "* TODO %^{Book Name}\n%u\n%a\n" :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
             '("t" "Work Task" entry
               (file+headline "~/Emacs/mybook/org/MyGTD.org" "Capture Tasks")
               "* TODO %^{Task Name}\n\tCreated:%u\n%a\n" :clock-in t :clock-resume t))

(add-to-list 'org-capture-templates
             '("b" "Billing" plain
               (file+function "~/Emacs/mybook/org/billing.org" find-month-tree)
               " | %U | %^{类别} | %^{描述} | %^{金额} |" :kill-buffer t))

(defun get-year-and-month ()
  (list (format-time-string "%Y年") (format-time-string "%m月")))

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
          (tags-todo "COMPUTER")
          (tags-todo "DVD")
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
    (find-file "~/Emacs/mybook/org/MyGTD.org")
)
(global-set-key (kbd "C-c g") 'gtd)

(add-hook 'org-agenda-mode-hook 'hl-line-mode)


 ;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; ============================================================================
;; MobileOrg Configuration
;; https://blog.csdn.net/lujun9972/article/details/46002799

(setq org-mobile-directory "~/Emacs/syncdirs/org")
(setq org-mobile-files (list "~/Emacs/mybook/org/MyGTD.org"
                             "~/Emacs/mybook/org/myagendas.org" ))

;; 当要把MobileOrg所做的修改同步到电脑端Org时,电脑端Org会先把MobileOrg的修改动作记录到该变量指定的文件中,然后再根据该文件中所记录的操作对电脑端Org进行修改
(setq org-mobile-inbox-for-pull "~/Emacs/mybook/org/inbox.org")

(defcustom org-mobile-checksum-binary (or (executable-find "~/emacs/bin/md5sums.exe"))
 "Executable used for computing checksums of agenda files."
 :group 'org-mobile
 :type 'string)
