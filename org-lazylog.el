;;; org-lazylog.el --- summary

;;; Commentary:

;;; Code:

(require 'org-id)
(require 'cl-lib)

;;(defvar org-lazylog-header)
(defvar org-lazylog-file "~/test-logger.org")
(defvar org-lazylog-id-locations nil)
(defvar org-lazylog-templates '(("s" "silent" plain
                               (file+function org-lazylog-file org-lazylog-write-datetree)
                               "%(org-lazylog-annotate)" :immediate-finish t)
                              ("p" "prompt" plain
                               (file+function org-lazylog-file org-lazylog-write-datetree)
                               "%(org-lazylog-annotate)")))
(defvar org-lazylog-todo-ignore-keywords nil)
(defvar org-lazylog-todo-format "[[id:%%s][%%s/%%s]] (-> %s)")
(defvar org-lazylog-command-targets nil)

(defun org-lazylog-format-header (&optional format)
  "."
  (let ((org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
        (path (string-join (org-get-outline-path t) "/"))
        (id (or (org-entry-get nil "CUSTOM_ID")
                (funcall 'org-id-get-create))))
    (add-to-list 'org-lazylog-id-locations (buffer-file-name))
    (format (or format "[[id:%s][%s/%s]]") id (buffer-name) path)
    ))

(defvar org-lazylog-annotate-pattern)
(defun org-lazylog-annotate ()
  )

(defun org-lazylog-extract-target (string)
  "."
  (or (and (string-match "\\[\\[id:.+\\]\\]" string)
           (match-string 0 string))
      string))

(cl-defun org-lazylog-write-datetree (&optional (date (calendar-current-date)))
  "."
  (org-datetree-find-date-create date)
  (let ((d (org-current-level)))
    (org-end-of-subtree)
    (if (string-match-p (org-lazylog-extract-target org-lazylog-header)
                        (org-lazylog-extract-target (nth 4 (org-heading-components))))
        nil
      (unless (bolp) (insert "\n"))
      (insert (make-string (+ d 1) ?*) " ")
      (org-time-stamp '(16) 'inactive)
      (insert " " org-lazylog-header)
      )))

(defun org-lazylog-update-id-locations ()
  "."
  (interactive)
  (org-id-update-id-locations org-lazylog-id-locations))

(defun org-lazylog-capture-message (&optional header annotate key)
  "."
  (interactive)
  (let ((org-capture-templates org-lazylog-templates)
        (org-capture-prepare-finalize-hook (remove 'org-lazylog-capture-entry org-capture-prepare-finalize-hook))
        (org-lazylog-header header)
        (k (or key (if annotate "p" "s"))))
    (org-capture nil k)
    ))

(defun org-lazylog-capture-entry (&optional header annotate key)
  "."
  (interactive)
  (unless (string= (buffer-file-name) (expand-file-name org-lazylog-file))
    (let ((org-capture-templates org-lazylog-templates)
          (org-capture-prepare-finalize-hook (remove 'org-lazylog-capture-entry org-capture-prepare-finalize-hook))
          (org-lazylog-header (org-lazylog-format-header header))
          (k (or key (if annotate "p" "s"))))
      (org-capture nil k)
      )))

;;; specific loggers
(defun org-lazylog-agenda-logger ()
  "."
  (org-agenda-with-point-at-orig-entry nil (org-lazylog-capture-entry)))

(defun org-lazylog-todo-logger ()
  "."
  (unless (member org-state org-lazylog-todo-ignore-keywords)
    (org-lazylog-capture-entry (format org-lazylog-todo-format org-state))))

(defun org-lazylog-command-logger (f &rest args)
  "."
  (let ((command (nth 1 args)))
    (if (member command org-lazylog-command-targets)
        (org-lazylog-capture-message (format "Command exected: %s" command))))
  (apply f args))

(defun org-lazylog-save-logger ()
  "."
  (if (string= buffer-file-name (expand-file-name org-lazylog-file))
      (message "not logged: %s %s" org-lazylog-file buffer-file-name)
    (message "logged: %s %s" org-lazylog-file buffer-file-name)
    ))

;; lazylog when saving file

;; lazylog when changing todo state
(add-hook 'org-after-todo-state-change-hook 'org-lazylog-todo-logger)

;; lazylog when touching from org-agenda
(add-hook 'org-agenda-after-show-hook 'org-lazylog-capture-entry)
(advice-add 'org-recur--agenda-finish :before 'org-lazylog-agenda-logger)
(advice-add 'org-agenda-kill :before 'org-lazylog-agenda-logger)

;; lazylog when capturing
(add-hook 'org-capture-prepare-finalize-hook 'org-lazylog-capture-entry)

;; lazylog when executing specific commands
(setq org-lazylog-command-targets '("mu4e" "erc"))
(advice-add 'execute-extended-command :around 'org-lazylog-command-logger)

(provide 'org-lazylog)
