(setq org-directory "~/org")

;; required, else setting key binding for org-agenda-mode-map fails
(require 'org-agenda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (pair (list
               ;; add any keybindings here
               (cons "<f12>" 'org-agenda)
               (cons "<f5>" 'badi/org-todo-narrow-subtree) ; bh/org-todo
               (cons "<S-f5>" 'badi/org-todo-widen-subtree) ;bh/widen
               (cons "<f7>" 'badi/set-truncate-lines)
               ;; (cons "<f9> <f9>" 'badi/show-org-agenda)
               (cons "<f9> c" 'calendar)
               (cons "<f9> v" 'visible-mode)
               (cons "<f9> l" 'org-toggle-link-display)
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


;; on agenda view
(define-key org-agenda-mode-map (kbd "M") #'org-agenda-month-view)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; agenda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 ;; match *.org and *.org.gpg files
 org-agenda-file-regexp "\\`[^.].*\\.org\\(\\.gpg\\)\\{0,1\\}\\'"
 org-agenda-files '("~/org"))

(setq org-agenda-sticky t
      ;; support multiple agenda frames

      ;; bring up agenda in full window, restore windows after quiting
      org-agenda-window-setup 'only-window
      org-agenda-restore-windows-after-quit t

      org-agenda-compact-blocks t
      org-agenda-todo-keyword-format "%-10s"
      org-agenda-timerange-leaders '("" "(%d/%d): ")
      org-agenda-scheduled-leaders '("" "Sched.%2dx: ")

      org-agenda-custom-commands
      '(("n" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels t)))

        ("r" "Refile" tags "REFILE"
         ((org-agenda-overriding-header "Tasks to Refile")
          (org-tags-match-list-sublevels nil)))

        ("x" "Unscheduled" tags-todo "-someday/!"
         ((org-agenda-overriding-header "Unscheduled in-progress tasks")
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))

        ;; (" " "Agenda"
        ;;  ((agenda " " nil)
        ;;   (tags "REFILE"
        ;;         ((org-agenda-overriding-header "Tasks to Refile")
        ;;          (org-tags-match-list-sublevels nil)))
        ;;   (tags-todo "-CANCELLED/!"
        ;;              ((org-agenda-overriding-header "Stuck Projects")
        ;;               (org-agenda-skip-function 'badi/skip-non-stuck-projects)
        ;;               (org-agenda-sorting-strategy '(category-keep))))
        ;;   (tags-todo "-HOLD-CANCELLED/!"
        ;;              ((org-agenda-overriding-header "Projects")
        ;;               (org-agenda-skip-function 'badi/skip-non-projects)
        ;;               (org-tags-match-list-sublevels 'indented)
        ;;               (org-agenda-sorting-strategy '(category-keep))))
        ;;   (tags-todo "-CANCELLED/!NEXT"
        ;;              ((org-agenda-sorting-strategy (concat "Project Next Tasks"
        ;;                                                    (if badi/hide-scheduled-and-waiting-next-tasks
        ;;                                                        ""
        ;;                                                      " (including WAITING and SCHEDULED tasks)")))
        ;;               (org-agenda-skip-function 'badi/skip-projects-and-single-tasks)
        ;;               (org-tags-match-list-sublevels t)
        ;;               (org-agenda-todo-ignore-scheduled badi/hide-scheduled-and-waiting-next-tasks)
        ;;               (org-agenda-todo-ignore-deadlines badi/hide-scheduled-and-waiting-next-tasks)
        ;;               (org-agenda-todo-ignore-with-date badi/hide-scheduled-and-waiting-next-tasks)
        ;;               (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
        ;;   (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
        ;;              ((org-agenda-overriding-header (concat "Project Subtasks"
        ;;                                                     (if badi/hide-scheduled-and-waiting-next-tasks
        ;;                                                         ""
        ;;                                                       " (including WAITING and SCHEDULED tasks")))
        ;;               (org-agenda-skip-function 'badi/skip-non-project-tasks)
        ;;               (org-agenda-todo-ignore-scheduled badi/hide-scheduled-and-waiting-next-tasks)
        ;;               (org-agenda-todo-ignore-deadlines badi/hide-scheduled-and-waiting-next-tasks)
        ;;               (org-agenda-todo-ignore-with-date badi/hide-scheduled-and-waiting-next-tasks)
        ;;               (org-agenda-sorting-strategy '(category-keep))))
        ;;   (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
        ;;              ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
        ;;                                                     (if badi/hide-scheduled-and-waiting-next-tasks
        ;;                                                         ""
        ;;                                                       " (including WAITING and SCHEDULED tasks)")))
        ;;               (org-agenda-skip-function 'badi/skip-non-tasks)
        ;;               (org-tags-match-list-sublevels nil)
        ;;               (org-agenda-todo-ignore-scheduled badi/hide-scheduled-and-waiting-next-tasks)
        ;;               (org-agenda-todo-ignore-deadlines badi/hide-scheduled-and-waiting-next-tasks)))
        ;;   (tags "-REFILE/"
        ;;         ((org-agenda-overriding-header "Tasks to Archive")
        ;;          (org-agenda-skip-function 'badi/skip-non-archivable-tasks)
        ;;          (org-tags-match-list-sublevels nil))))
        ;;  nil)
        ))




(defun my/org-agenda-exclude-function (tag)
  (and
   (cond
    ((equal tag "someday")
     t))
   (concat "-" tag)))

(setq org-agenda-(and )uto-exclude-function 'my/org-agenda-exclude-function)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tasks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq

 org-use-tag-inheritance t
 org-tags-exclude-from-inheritance '("project")

 org-todo-keywords
 '((sequence "TODO(t)" "|" "DONE(d!)")
   (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"
             "PHONE(p)" "MEETING(m)"))

 org-todo-keywork-faces
 '(("TODO" :foreground "red" :weight bold)
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
   ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))

 org-default-notes-file
 (concat (file-name-as-directory org-directory)  "refile.org")

 org-refile-file org-default-notes-file

 ;; any org file is a valid target
 org-refile-targets
 '((nil :maxlevel . 9)
   (org-agenda-files :maxlevel . 9))

 ;; full outline paths for targets
 org-refile-use-outline-path t

 ;; targets complete directly with IDO
 org-outline-path-complete-in-steps nil

 ;; allow refile to create parent tasks
 org-refile-allow-creating-parent-nodes 'confirm

 org-completion-use-ido t

 ;; use current window when visiting files and buffers with ido
 ido-default-file-method 'selected-window
 ido-default-buffer-method 'selected-window

 ;; use current window for indirect buffer
 org-indirect-buffer-display 'current-window

 ;; exclude DONE tasks
 org-refile-target-verify-function 'badi/org-refile-target-verify)


(setq
 ;;; Capture templates:
 ;; TODO tasks, Notes, Appointments, Phone calls, Meetings, Org
 ;; protocol

 org-capture-templates
 '(("t" "todo" entry (file "")
    "* TODO %?\n  %U\n  %a\n  "
    :clock-in t :clock-resume t)

   ("r" "respond" entry (file "")
    "* TODO [#A] Respond to %:fromname :EMAIL:\n  SCHEDULED: %T\n  %U\n  %a\n  [%:subject]\n  "
    :clock-in t :clock-resume t :immediate-finish t)

   ("n" "note" entry (file "notes.org")
    "* %?\n  %U\n  %a\n  "
    :clock-in t :clock-resume t)

   ;; add a note to the currently clocked item
   ("N" "clocked note" entry (clock)
    "* %?  :NOTE:\n  %U\n  %a\n  "
    :clock-in t :clock-resume t)

   ("o" "org-protocol" entry (file "")
    "* TODO Review %c\n  %U\n  "
    :immediate-finish t)

   ("m" "Meeting" entry (file "")
    "* MEETING with %? :MEETING: \n  %U\n  "
    :clock-in t :clock-resume t)

   ("p" "Phone call" entry (file "")
    "* PHONE %? :PHONE:\n  %U\n  "
    :clock-in t :clock-resume t)

   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq

 org-clocktable-defaults
 (list
  :maxlevel 3
  :scope nil
  :tstart "<-1w>"
  :tend "<now>"
  :step 'day
  :stepskip0 t
  :fileskip0 t)

 ;;; provide statistics
 org-provide-todo-statistics t
 ;; all children
 org-hierarchical-todo-statistics nil

 ;;; block changes to DONE that have incomplete dependencies
 org-enforce-todo-dependencies t
 org-enforce-todo-checkbox-dependencies t

 ;;; hide leading stars
 org-hide-leading-stars t)


(setq
  ;;; configure column view entries
 ;;; http://orgmode.org/manual/Column-attributes.html#Column-attributes
 ;;; http://orgmode.org/manual/Special-properties.html#Special-properties
 ;;; http://orgmode.org/worg/org-tutorials/org-column-view-tutorial.html
 ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;
 ;;; definition format:
 ;;;
 ;; %[width]property[(title)][{summary-type}]
 ;;;
 ;;;
 ;; width           An integer specifying the width of the column in characters.
 ;;                 If omitted, the width will be determined automatically.
 ;; property        The property that should be edited in this column.
 ;;                 Special properties representing meta data are allowed here
 ;;                 as well (see Special properties)
 ;; title           The header text for the column.  If omitted, the property
 ;;                 name is used.
 ;; {summary-type}  The summary type.  If specified, the column values for
 ;;                 parent nodes are computed from the children1.
 ;;                 Supported summary types are:
 ;;                 {+}       Sum numbers in this column.
 ;;                 {+;%.1f}  Like ‘+’, but format result with ‘%.1f’.
 ;;                 {$}       Currency, short for ‘+;%.2f’.
 ;;                 {min}     Smallest number in column.
 ;;                 {max}     Largest number.
 ;;                 {mean}    Arithmetic mean of numbers.
 ;;                 {X}       Checkbox status, ‘[X]’ if all children are ‘[X]’.
 ;;                 {X/}      Checkbox status, ‘[n/m]’.
 ;;                 {X%}      Checkbox status, ‘[n%]’.
 ;;                 {:}       Sum times, HH:MM, plain numbers are
 ;;                 hours2.
 ;;                 {:min}    Smallest time value in column.
 ;;                 {:max}    Largest time value.
 ;;                 {:mean}   Arithmetic mean of time values.
 ;;                 {@min}    Minimum age3 (in
 ;;                           days/hours/mins/seconds).
 ;;                 {@max}    Maximum age (in days/hours/mins/seconds).
 ;;                 {@mean}   Arithmetic mean of ages (in days/hours/mins/seconds).
 ;;                 {est+}    Add ‘low-high’ estimates.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;
 ;;; property options:
 ;;;
 ;; ALLTAGS      All tags, including inherited ones.
 ;; BLOCKED      "t" if task is currently blocked by children or siblings.
 ;; CLOCKSUM     The sum of CLOCK intervals in the subtree.  org-clock-sum
 ;;              must be run first to compute the values in the current buffer.
 ;; CLOCKSUM_T   The sum of CLOCK intervals in the subtree for today.
 ;;              org-clock-sum-today must be run first to compute the
 ;;              values in the current buffer.
 ;; CLOSED       When was this entry closed?
 ;; DEADLINE     The deadline time string, without the angular brackets.
 ;; FILE         The filename the entry is located in.
 ;; ITEM         The headline of the entry.
 ;; PRIORITY     The priority of the entry, a string with a single letter.
 ;; SCHEDULED    The scheduling timestamp, without the angular brackets.
 ;; TAGS         The tags defined directly in the headline.
 ;; TIMESTAMP    The first keyword-less timestamp in the entry.
 ;; TIMESTAMP_IA The first inactive timestamp in the entry.
 ;; TODO         The TODO keyword of the entry.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 org-columns-default-format "%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %Effort(Time){:} %6CLOCKSUM{Total}")


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
  (setq badi/keep-clock-running t)
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


(defun badi/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (message "Skipping non stuck projects")
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (badi/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))


(defun badi/skip-non-projects ()
  "Skip trees that are not projects"
  (message "Skipping non projects")
  (if (save-excursion (badi/skip-non-stuck-projects))
      (save-excursion
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((badi/is-project-p) nil)
           ((and (badi/is-project-subtree-p) (not (badi/is-task-p))) nil)
           (t subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defvar badi/hide-scheduled-and-waiting-next-tasks t)

(defun badi/skip-projects-and-single-tasks ()
  "Skip trees that are projects and single non-project tasks"
  (message "Skipping projects and single tasks")
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((and badi/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((badi/is-project-p)
        next-headline)
       ((and (badi/is-task-p) (not (badi/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))


(defun badi/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks and project-related tasks."
  (message "Skipping non-tasks")
  (save-excursion
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((badi/is-task-p) nil)
       (t next-headline)))))


(defun badi/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (message "Skipping non-archivable tasks")
  (save-excursion
    (widen)
    ;; consider only tasks with done todo heading as archivable
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time)
                                                                             (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month)
                                                                   subtree-end t)))))
                (if subtree-is-current subtree-end ;; has a date in this or last month .. skip it
                  nil)) ;; archivable
            (or subtree-end (point-max)))
        next-headline))))


(defun badi/is-project-p ()
  "Any task with a todo keyword subtask"
  (message "is-project-p")
  (save-excursion
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward
                     "^\+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))


(defun badi/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (message "is-project-subtree-p")
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (badi/find-project-task)
      (if (equal (point) task) nil
        t))))


(defun badi/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (message "is-task-p")
  (save-excursion
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))


(defun badi/find-project-task ()
  "Move point to the parent (project) task if any"
  (message "find-project-task")
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))
