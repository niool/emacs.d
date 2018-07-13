;; NOTE: Make the .custom.el working, you MUST copy it from Git Repo to $HOME

;; Please note the color theme's name is "molokai"
;; (when (or (display-graphic-p)
;;           (string-match-p "256color"(getenv "TERM")))
;;   (load-theme 'molokai t))

(setq default-frame-alist
      '((height . 52) (width . 104) (menu-bar-lines . 20) (tool-bar-lines . 0)))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; ============================================================================
;; Set all coding global parameter to utf-8
;; Fix issue "Bad file encoding" in MobileOrg
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Latex Support Chinese
(require 'ox-latex)

(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode %f"))
(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-latex-classes
             '("cnlatex"
               "\\documentclass{ctexart}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; My configruation categorized by topics
(if (file-exists-p "~/.emacs.d/myconfig/orgGTD.el") (load-file "~/.emacs.d/myconfig/orgGTD.el"))
(if (file-exists-p "~/.emacs.d/myconfig/newsFeed.el") (load-file "~/.emacs.d/myconfig/newsFeed.el"))

;; Sandbox Area
;; You can test come configuration quickly here, but be sure to move it to myconfig dir
;; as soon as it stable.
;; Good luck!!!

(defun my-execute-sandbox ()
  (interactive)
  (load-file "~/.sandbox.el"))

(if (file-exists-p "~/.sandbox.el") (load-file "~/.sandbox.el"))
(global-set-key (kbd "C-c e") 'my-execute-sandbox)

;; File End
