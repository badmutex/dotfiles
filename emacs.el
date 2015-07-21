;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional useful functions and variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun badi/package-install (package)
  "Install a package if it is not present already"
  (unless (package-installed-p package)
    (package-install package)))

(defun badi/package-install-list (package-list)
  "Install a list of packages if necessary"
  (dolist (package package-list)
    (badi/package-install package)))

(defun badi/joindirs (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join,
  (dotemacs-joindirs \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"
  (if (not dirs)
      root
    (apply 'badi/joindirs
           (expand-file-name (car dirs) root)
           (cdr dirs))))

(defun badi/package/refresh-contents ()
  "Refresh the package contents if necessary"
  (let ((package/archive-file-exists-p
         (lambda (name)
           (let* ((archive (badi/joindirs package-user-dir
                                          "archives"
                                          name
                                          "archive-contents")))
             (message "[badi/package/archive-file-exists-p] checking %s" archive)
             (file-exists-p archive)))))
    (dolist (package package-archives)
      (unless (funcall package/archive-file-exists-p (car package))
        (package-refresh-contents)))))

(defun badi/package/emacs-compat-fix ()
  "Add gnu packages when emacs is v23 or less for libs like cl-lib"
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))))

(defun badi/package/update ()
  "Updated installed packages"
  (interactive)
  (package-list-packages)
  (package-menu-mark-upgrades)
  (package-menu-execute))

(defun badi/switch-system-name (names-and-bodies)
  "Load overrides based on hostname
   NAMES-AND-BODEIS is a list where individual elements have the form
   (\"hostname\" . (sexpr)), where (sexpr) will be evaluated when the
   current hostname matches"
  (dolist (name-body names-and-bodies)
    (let ((name (car name-body))
	  (body (cdr name-body)))
      (when (string-prefix-p name system-name)
	(message "[switch-system-name] found %s running %s" name body)
	(eval body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modify editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; don't use tabs, just spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup package repositories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(dolist (archive
         '(
           ("melpa" . "http://melpa.org/packages/")
           ("elpy" . "http://jorgenschaefer.github.io/packages/")
           ("marmalade" . "http://marmalade-repo.org/packages/")
           ))
  (add-to-list 'package-archives archive))

(badi/package/emacs-compat-fix)
(package-initialize)
(badi/package/refresh-contents)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install some useful packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO prelude packages https://github.com/bbatsov/prelude
;; TODO projectile https://github.com/bbatsov/projectile
;; TODO helm http://tuhdo.github.io/helm-intro.html
;; TODO flyspell http://www.emacswiki.org/emacs/FlySpellx
;; TODO auctex
;;      - installed, configured
;;      - autocompletion (company-mode is recommended)
 

(badi/package-install-list
 '(
   ;; auto-complete mode
   ;; http://emacswiki.org/emacs/AutoComplete
   auto-complete

   ;; Color variables differently
   ;; https://github.com/ankurdave/color-identifiers-mode
   color-identifiers-mode

   ;; csv-mode for editing csv files
   ;; http://emacswiki.org/emacs/CsvMode
   csv-mode

   ;; expande syntactic regions
   ;; https://github.com/magnars/expand-region.el
   expand-region

   ;; flexible string matching
   flx
   flx-ido

   ;; on-the-fly checking
   flycheck
   ;; colors the mode line according to the Flycheck state
   ;; https://github.com/flycheck/flycheck-color-mode-line
   flycheck-color-mode-line

   ;; haskell-mode (since no ghc)
   haskell-mode

   ;; hungry delete
   ;; delete all whitespace in the direction you are deleting
   ;; https://github.com/nflath/hungry-delete
   hungry-delete

   ;; vertial ido matches
   ido-vertical-mode

   ;; auctex
   auctex
   auto-complete-auctex

   ;; matlab
   matlab-mode

   ;; git
   ;; http://www.emacswiki.org/emacs/Magit
   ;; http://www.masteringemacs.org/article/introduction-magit-emacs-mode-git
   magit
   ;; show changes in fringe
   ;; https://github.com/syohex/emacs-git-gutter-fringe
   git-gutter-fringe
   ;; work in terminal
   ;; https://github.com/nonsequitur/git-gutter-fringe-plus
   git-gutter-fringe+

   ;; markdown
   ;; http://jblevins.org/projects/markdown-mode/
   markdown-mode

   ;; multiple cursors
   ;;https://github.com/magnars/multiple-cursors.el
   multiple-cursors

   ;; nix mode for nix expression files
   ;; https://nixos.org
   nix-mode

   ;; enhance M-x with IDO
   ;; https://github.com/nonsequitur/smex
   smex

   ;; visual regexp (actual regex, not emacs-style)
   ;; note: this requires python
   ;; https://github.com/benma/visual-regexp-steroids.el/
   visual-regexp-steroids

   ;; snippets
   ;; https://github.com/capitaomorte/yasnippet
   yasnippet

   ;; yaml editing
   yaml-mode
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto complete
;; globally enable auto-complete
;; (global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; color identifiers
;; globally enable color-identifiers-mode
(add-hook 'after-init-hook 'global-color-identifiers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; csv mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ido
;; Interactively do things
(ido-mode 1)
(ido-everywhere t)
(require 'flx-ido) ; flexible string matching
(flx-ido-mode t)
;; disable ido faces to see flx highlighting
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq ido-everywhere t)
(flx-ido-mode 1)

;; use ido vertically (easier to read)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down) ; for arrow keys


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flycheck
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; git
(when (window-system)
  (when (require 'git-gutter-fringe nil t)
    (global-git-gutter-mode 1)
    (setq-default indicate-buffer-boundaries 'left)
    (setq-default indicate-empty-lines 1)))

(when (not (window-system))
  (require 'git-gutter-fringe+)
  (global-git-gutter+-mode 1)
  (git-gutter+-toggle-fringe))

(global-set-key (kbd "C-c C-g") 'magit-status)

;; magit-status full-screen
;; http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))


;; pass -v to commit so we can see the staged hunk
;; http://emacs.stackexchange.com/questions/3893
(advice-add #'magit-key-mode-popup-committing :after
            (lambda ()
              (magit-key-mode-toggle-option (quote committing) "--verbose")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hungry delete
(global-hungry-delete-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Latex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-complete-mode)

; compile to PDF
(setq TeX-PDF-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; matlab
(autoload 'matlab-mode "matlab" "Matlab Editing  Mode" t)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multiple cursors
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; nix mode
(autoload 'nix-mode "nix-mode" "Major mode for editing Nix expressions." t)
(push '("\\.nix\\'" . nix-mode) auto-mode-alist)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smex
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; the old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; visual regexp
(require 'visual-regexp-steroids)
(global-set-key (kbd "C-s")   'isearch-forward)
(global-set-key (kbd "C-r")   'isearch-backward)
(global-set-key (kbd "C-c s") 'vr/isearch-forward)
(global-set-key (kbd "C-c r") 'vr/isearch-backward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yaml-mode
(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user interface settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; only start the server if it is not yet running
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

(unless window-system (menu-bar-mode -1))
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; upcase/downcase regions is nice
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)
(show-paren-mode t)
(hl-line-mode t)
(subword-mode)
(pending-delete-mode t)
(transient-mark-mode t)

;;; use versioned backups, don't clobber symlinks, don't litter fs tree
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-version 2
 version-control 5)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(badi/package-install 'atom-dark-theme)
(load-theme 'atom-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python ide stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this requires jedi, flake8, and pyflakes to be availables.
;; The easiest path is to install them globally

;; elpy: ide
;; flymake: on-the-fly checks
;; sphinx-doc: autoinsert sphinx-doc docstrings
;;             (C-c M-d at function def)
(setq python-ide-package-list '(elpy flymake sphinx-doc))
(badi/package-install-list '(elpy
                             flymake
                             sphinx-doc))

(elpy-enable)
(add-hook 'python-mode-hook (lambda ()
                              (require 'sphinx-doc)
                              (sphinx-doc-mode t)))

;; configure nose for testing
;; ;TODO: see elpy docs for more details: http://elpy.readthedocs.org/en/latest/ide.html#testing
;; ;TODO: checkout tdd.el: https://github.com/jorgenschaefer/emacs-tdd/
(setq elpy-test-runner 'elpy-test-nose-runner)
(setq elpy-test-nose-runner-command '("nosetests" "--all-modules" "-s"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; automatically change entry to DONE when all children are DONE
(defun badi/org/summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'badi/org/summary-todo)

(setq org-hide-leading-stars t)

;; block changes to DONE that have incomplete dependencies
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)


;; provide statistics
(setq org-provide-todo-statistics t)
(setq org-hierarchical-todo-statistics nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load specific overrides based on system type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(badi/switch-system-name '(("sulimo" . (set-face-attribute 'default nil :height 80))))
(badi/switch-system-name '(("lorien" . (set-face-attribute 'default nil :height 120))))
