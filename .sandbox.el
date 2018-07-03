;; Sandbox area to test some ideas of Emacs

(defun hello (name)
  (insert (format "Sandbox: %s\n" name)))

(hello "Enter sandbox...")

(hello "Quit sandbox!!!")
