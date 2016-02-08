(setq org-directory "~/org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (pair (list
               ;; add any keybindings here
               (cons "<f12>" 'org-agenda)
               (cons "<f5>" 'badi/org-todo-narrow-subtree) ; bh/org-todo
               (cons "<S-f5>" 'badi/org-todo-widen-subtree) ;bh/widen
               (cons "<f7>" 'badi/set-truncate-lines)
               (cons "<f8>" 'org-cycle-agenda-files)
               ;; (cons "<f9> <f9>" 'badi/show-org-agenda)
               (cons "<f9> c" 'calendar)
               (cons "<f9> g" 'gnus)
               ;; (cons "<f9> h" 'badi/hide-other)
               ;; (cons "<f9> n" 'badi/toggle-next-task-display)
               ;; (cons "<f9> I" 'badi/punch-in)
               ;; (cons "<f9> O" 'badi/punch-out)

               (cons "C-c l" 'org-store-link)
               (cons "C-c a" 'org-agenda)
               (cons "C-c b" 'org-iswitchb)
               (cons "C-c c" 'org-capture)))

  (lexical-let ((key (car pair))
                (fun (cdr pair)))
    (message "%s -> %s" key pair)
    (global-set-key (kbd key) fun)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; agenda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; all my .org encrypted files
(setq org-agenda-files
      (append
       (file-expand-wildcards "~/org/*.org")

       ;; add to the top of the .org.gpg, replacing <KEY> with your
       ;; gpg key name
       ;;
       ;; # -*- mode: org; epa-file-encrypt-to: ("<KEY>"); -*-
       ;; #+ARCHIVE: %s_archive.gpg::
       ;;
       ;; this is to ensure that tasks are archived to an encrypted
       ;; file

       (file-expand-wildcards "~/org/*.org.gpg")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tasks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq

 org-todo-keywords
 '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
   (sequence "WAITING(w@!)" "|" "CANCELLED(c@/!)"
             "PHONE" "MEETING"))

 org-todo-keywork-faces
 '(("TODO" :foreground "red" :weight bold)
   ("NEXT" :foreground "blue" :weight bold)
   ("DONE" :foreground "forest green" :weight bold)
   ("WAITING" :foreground "orange" :weight bold)
   ("HOLD" :foreground "magenta" :weight bold)
   ("CANCELLED" :foreground "forest green" :weight bold)
   ("MEETING" :foreground "forest green" :weight bold)
   ("PHONE" :foreground "forest green" :weight bold))


 ;;; The triggers break down to the following rules:
 ;;
 ;; - Moving a task to CANCELLED adds a CANCELLED tag
 ;; - Moving a task to WAITING adds a WAITING tag
 ;; - Moving a task to HOLD adds WAITING and HOLD tags
 ;; - Moving a task to a done state removes WAITING and HOLD tags
 ;; - Moving a task to TODO removes WAITING, CANCELLED, and HOLD tags
 ;; - Moving a task to NEXT removes WAITING, CANCELLED, and HOLD tags
 ;; - Moving a task to DONE removes WAITING, CANCELLED, and HOLD tags

 org-todo-state-tags-triggers
 '(("CANCELLED" ("CANCELLED" . t))
   ("WAITING" ("WAITING" . t))
   ("HOLD" ("WAITING") ("HOLD" . t))
   (done ("WAITING") ("HOLD"))
   ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
   ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
   ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))

 org-default-notes-file
 (concat (file-name-as-directory org-directory)  "refile.org.gpg")

 org-refile-file org-default-notes-file

 ;; any org file is a valid target
 org-refile-targets
 '((nil :maxlevel . 9)
   (org-agenda-files :maxlevel . 9))

 ;; full outline paths for targets
 org-refile-use-outline-path t

 ;; allow refile to create parent tasks
 org-refile-allow-creating-parent-nodes 'confirm

 ;; use current window for indirect buffer
 org-indirect-buffer-display 'current-window

 ;; exclude DONE tasks
 org-refile-target-verify-function 'badi/org-refile-target-verify

 ;;; Capture templates:
 ;; TODO tasks, Notes, Appointments, Phone calls, Meetings, Org
 ;; protocol

 org-capture-templates
 '(("t" "todo" entry (file "")
    "* TODO %?\n  %U\n  %a\n  ")
   ;; :clock-in t :clock-resume t)

   ("r" "respond" entry (file "")
    "* NEXT Respond to %:from about [%:subject]\n  SCHEDULED: %t\n  %U\n  %a\n  ")
   ;; :clock-in t :clock-resume t :immediate-finish t)

   ("n" "note" entry (file "")
    "* %? :NOTE:\n  %U\n  %a\n  ")
   ;; :clock-in t :clock-resume t)

   ("o" "org-protocol" entry (file "")
    "* TODO Review %c\n  %U\n  ")
   ;; :immediate-finish t)

   ("m" "Meeting" entry (file "")
    "* MEETING with %? :MEETING: \n  %U\n  ")
   ;; :clock-in t :clock-resume t)

   ("p" "Phone call" entry (file "")
    "* PHONE %? :PHONE:\n  %U\n  ")
   ;; :clock-in t :clock-resume t)))

   ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; my functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (defun badi/narrow-to-org-subtree ()
   "FIXME: documentation"
   (widen)
   (org-narrow-to-subtree)
   (save-restriction
     (org-agenda-set-restriction-lock)))

(defun badi/org-todo-narrow-subtree (arg)
  "FIXME: documentation"
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (badi/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (badi/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(defun badi/org-todo-widen-subtree ()
  "FIXME: documentation"
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(defun badi/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display"
  (interactive)
  (message "Toggling 'truncate-lines'")
  (setq truncate-lines (not truncate-lines))
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun badi/show-org-agenda ()
  "Switch to the currently open org-agenda buffer"
  (interactive)
  (message "Calling 'badi/show-org-agenda'")
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda ( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))

(defun badi/hide-other ()
  "FIXME: documentation"
  (interactive)
  (message "Calling 'badi/hide-other'")
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))


(defvar badi/hide-scheduled-and-waiting-next-tasks t)
(defun badi/toggle-next-task-display ()
  "FIXME: documentation"
  (interactive)
  (message "Calling 'badi/toggle-next-task-display'")
  (setq badi/hide-scheduled-and-waiting-next-tasks
        (not badi/hide-scheduled-and-waiting-next-tasks))
  (when (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks"
           (if badi/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun badi/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task. If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (message "Punch in %d" arg)
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (badi/clock-in-organization-task-as-default)))
    ;; else
    (save-restriction
      (widen)
                                        ; find tags on current task
      (if (and (equal major-mode 'org-mode)
               (not (org-before-first-heading-p))
               (eq arg 4))
          (org-clock-in '(16))
        (badi/clock-in-organization-task-as-default)))))

(defun badi/punch-out ())
;; (interactive)
;; (message "Calling 'badi/punch-out'")
;; (


(defun badi/org-refile-target-verify ()
  "Exclude DONE tasks"
  (not (member (nth 2 (org-heading-components))
               org-done-keywords)))
