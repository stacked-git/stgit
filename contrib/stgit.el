;; stgit.el: An emacs mode for StGit
;;
;; Copyright (C) 2007 David Kågedal <davidk@lysator.liu.se>
;;
;; To install: put this file on the load-path and place the following
;; in your .emacs file:
;;
;;    (require 'stgit)
;;
;; To start: `M-x stgit'

(defun stgit (dir)
  "Manage stgit patches"
  (interactive "DDirectory: \n")
  (switch-to-stgit-buffer dir)
  (stgit-refresh))

(defun switch-to-stgit-buffer (dir)
  "Switch to a (possibly new) buffer displaying StGit patches for DIR"
  (setq dir (file-name-as-directory dir))
  (let ((buffers (buffer-list)))
    (while (and buffers
                (not (with-current-buffer (car buffers)
                       (and (eq major-mode 'stgit-mode)
                            (string= default-directory dir)))))
      (setq buffers (cdr buffers)))
    (switch-to-buffer (if buffers
                          (car buffers)
                        (create-stgit-buffer dir)))))

(defun create-stgit-buffer (dir)
  "Create a buffer for showing StGit patches.
Argument DIR is the repository path."
  (let ((buf (create-file-buffer (concat dir "*stgit*")))
        (inhibit-read-only t))
    (with-current-buffer buf
      (setq default-directory dir)
      (stgit-mode)
      (setq buffer-read-only t))
    buf))

(defmacro stgit-capture-output (name &rest body)
  "Capture StGit output and show it in a window at the end"
  `(let ((output-buf (get-buffer-create ,(or name "*StGit output*")))
         (stgit-dir default-directory)
         (inhibit-read-only t))
     (with-current-buffer output-buf
       (erase-buffer)
       (setq default-directory stgit-dir)
       (setq buffer-read-only t))
     (let ((standard-output output-buf))
       ,@body)
     (with-current-buffer output-buf
       (set-buffer-modified-p nil)
       (setq buffer-read-only t)
       (if (< (point-min) (point-max))
           (display-buffer output-buf t)))))
(put 'stgit-capture-output 'lisp-indent-function 1)

(defun stgit-run (&rest args)
  (apply 'call-process "stg" nil standard-output nil args))

(defun stgit-refresh ()
  "Update the contents of the stgit buffer"
  (interactive)
  (let ((inhibit-read-only t)
        (curline (line-number-at-pos))
        (curpatch (stgit-patch-at-point)))
    (erase-buffer)
    (insert "Branch: ")
    (stgit-run "branch")
    (stgit-run "series" "--description")
    (if curpatch
        (stgit-goto-patch curpatch)
      (goto-line curline))))

(defvar stgit-mode-hook nil
  "Run after `stgit-mode' is setup.")

(defvar stgit-mode-map nil
  "Keymap for StGit major mode.")

(unless stgit-mode-map
  (setq stgit-mode-map (make-keymap))
  (suppress-keymap stgit-mode-map)
  (define-key stgit-mode-map "?"   'stgit-help)
  (define-key stgit-mode-map "h"   'stgit-help)
  (define-key stgit-mode-map "p"   'previous-line)
  (define-key stgit-mode-map "n"   'next-line)
  (define-key stgit-mode-map "g"   'stgit-refresh)
  (define-key stgit-mode-map "r"   'stgit-rename)
  (define-key stgit-mode-map "\C-r"   'stgit-repair)
  (define-key stgit-mode-map "C"   'stgit-commit)
  (define-key stgit-mode-map "U"   'stgit-uncommit)
  (define-key stgit-mode-map ">"   'stgit-push-next)
  (define-key stgit-mode-map "<"   'stgit-pop-next)
  (define-key stgit-mode-map "P"   'stgit-push-or-pop)
  (define-key stgit-mode-map "G"   'stgit-goto)
  (define-key stgit-mode-map "="   'stgit-show))

(defun stgit-mode ()
  "Major mode for interacting with StGit.
Commands:
\\{stgit-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq mode-name "StGit"
        major-mode 'stgit-mode
        goal-column 2)
  (use-local-map stgit-mode-map)
  (set (make-local-variable 'list-buffers-directory) default-directory)
  (set-variable 'truncate-lines 't)
  (run-hooks 'stgit-mode-hook))

(defun stgit-patch-at-point ()
  "Return the patch name on the current line"
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[>+-] \\([^ ]*\\)")
        (match-string 1)
      nil)))

(defun stgit-goto-patch (patch)
  "Move point to the line containing PATCH"
  (let ((p (point)))
    (goto-char (point-min))
    (if (re-search-forward (concat "[>+-] " (regexp-quote patch) " ") nil t)
        (progn (move-to-column goal-column)
               t)
      (goto-char p)
      nil)))

(defun stgit-rename (name)
  "Rename the patch under point"
  (interactive (list (read-string "Patch name: " (stgit-patch-at-point))))
  (let ((old-name (stgit-patch-at-point)))
    (unless old-name
      (error "No patch on this line"))
    (stgit-capture-output nil
      (stgit-run "rename" old-name name))
    (stgit-refresh)
    (stgit-goto-patch name)))

(defun stgit-repair ()
  "Run stg repair"
  (interactive)
  (stgit-capture-output nil
   (stgit-run "repair"))
  (stgit-refresh))

(defun stgit-commit ()
  "Run stg commit."
  (interactive)
  (stgit-capture-output nil (stgit-run "commit"))
  (stgit-refresh))

(defun stgit-uncommit (arg)
  "Run stg uncommit. Numeric arg determines number of patches to uncommit."
  (interactive "p")
  (stgit-capture-output nil (stgit-run "uncommit" "-n" (number-to-string arg)))
  (stgit-refresh))

(defun stgit-push-next ()
  "Push the first unapplied patch"
  (interactive)
  (stgit-capture-output nil (stgit-run "push"))
  (stgit-refresh))

(defun stgit-pop-next ()
  "Pop the topmost applied patch"
  (interactive)
  (stgit-capture-output nil (stgit-run "pop"))
  (stgit-refresh))

(defun stgit-applied-at-point ()
  "Is the patch on the current line applied?"
  (save-excursion
    (beginning-of-line)
    (looking-at "[>+]")))

(defun stgit-push-or-pop ()
  "Push or pop the patch on the current line"
  (interactive)
  (let ((patch (stgit-patch-at-point))
        (applied (stgit-applied-at-point)))
    (stgit-capture-output nil
       (stgit-run (if applied "pop" "push") patch))
    (stgit-refresh)))

(defun stgit-goto ()
  "Go to the patch on the current line"
  (interactive)
  (let ((patch (stgit-patch-at-point)))
    (stgit-capture-output nil
       (stgit-run "goto" patch))
    (stgit-refresh)))

(defun stgit-show ()
  "Show the patch on the current line"
  (interactive)
  (stgit-capture-output "*StGit patch*"
    (stgit-run "show" (stgit-patch-at-point))
    (with-current-buffer standard-output
      (goto-char (point-min))
      (diff-mode))))

(defun stgit-help ()
  "Display help for the StGit mode."
  (interactive)
  (describe-function 'stgit-mode))

(provide 'stgit)
