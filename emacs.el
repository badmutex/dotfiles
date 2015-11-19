;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional useful functions and variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/package-install (package)
  "Install a package if it is not present already"
  (unless (package-installed-p package)
    (package-install package)))

(defun my/package-install-list (package-list)
  "Install a list of packages if necessary"
  (dolist (package package-list)
    (my/package-install package)))

(defun my/joindirs (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join,
  (dotemacs-joindirs \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"
  (if (not dirs)
      root
    (apply 'my/joindirs
           (expand-file-name (car dirs) root)
           (cdr dirs))))

(defun my/package/refresh-contents ()
  "Refresh the package contents if necessary"
  (let ((package/archive-file-exists-p
         (lambda (name)
           (let* ((archive (my/joindirs package-user-dir
                                        "archives"
                                        name
                                        "archive-contents")))
             (message
              "[my/package/archive-file-exists-p] checking %s" archive)
             (file-exists-p archive)))))
    (dolist (package package-archives)
      (unless (funcall package/archive-file-exists-p (car package))
        (package-refresh-contents)))))

(defun my/package/emacs-compat-fix ()
  "Add gnu packages when emacs is v23 or less for libs like cl-lib"
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))))

(defun my/package/update ()
  "Update installed packages from 'package"
  (interactive)
  (package-list-packages)
  (package-menu-mark-upgrades)
  (package-menu-execute))

;; don't use yet as this causes emacs to hang and not complete
;; instead run 'my/package/udpate and 'el-get-update-all manually
(defun my/update-packages ()
  "Update all installed packaged"
  (interactive)
  (el-get-update-all)
  (my/package/update))

(defun my/switch-system-name (names-and-bodies)
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
           ("melpa-stable" . "http://stable.melpa.org/packages/")
           ("elpy" . "http://jorgenschaefer.github.io/packages/")
           ("marmalade" . "http://marmalade-repo.org/packages/")
           ))
  (add-to-list 'package-archives archive))

(my/package/emacs-compat-fix)
(package-initialize)
(my/package/refresh-contents)



;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; list of packages unknown to el-get
(setq my-el-get-sources
      '(
        (:name ebnf-mode
               :description "Highlight mode for Extended Backus-Naur Form"
               :features ebnf-mode
               :type git
               :url "git@github.com:jeramey/ebnf-mode.git")
        ))

(setq el-get-sources my-el-get-sources)


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
 

(my/package-install-list
 '(
   ;; auto-complete mode
   ;; http://emacswiki.org/emacs/AutoComplete
   auto-complete

   ;; Color variables differently
   ;; https://github.com/ankurdave/color-identifiers-mode
   color-identifiers-mode

   ;; enforce the 80 columns rule
   ;; https://github.com/jordonbiondo/column-enforce-mode/
   column-enforce-mode

   ;; csv-mode for editing csv files
   ;; http://emacswiki.org/emacs/CsvMode
   csv-mode

   ;; dockerfile-mode for editing dockerfiles
   dockerfile-mode

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

   ;; haskell-mode
   haskell-mode
   ac-haskell-process
   company-ghci
   flycheck-haskell

   ;; hungry delete
   ;; delete all whitespace in the direction you are deleting
   ;; https://github.com/nflath/hungry-delete
   hungry-delete

   ;; vertial ido matches
   ido-vertical-mode

   ;; auctex
   auctex
   auto-complete-auctex

   ;; jinja
   jinja2-mode

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

   ;; mark delimiters with different colors
   ;; https://github.com/Fanael/rainbow-delimiters
   rainbow-delimiters

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

(setq my-el-get-packages
      (append
       'nil  ; change this to: '( foo bar baz ) when needed
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-el-get-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; speed bar
(require 'speedbar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 80 column rules
(require 'column-enforce-mode)
(add-hook 'prog-mode-hook 'column-enforce-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto complete
;; globally enable auto-complete
;; (global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; color identifiers
;; globally enable color-identifiers-mode
(add-hook 'after-init-hook 'global-color-identifiers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; csv mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ebnf configuration

;; use iso ebnf
;; https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form
;; http://www.cl.cam.ac.uk/~mgk25/iso-ebnf.html
(setq ebnf-syntax 'iso-ebnf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ido
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flycheck
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; git
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

;; restore windows after exiting magit buffers
;; http://magit.vc/manual/magit.html#Modes-and-Buffers
(setq magit-restore-window-configuration t)

;; automatic save
;; http://magit.vc/manual/magit.html#Automatic-save
(setq magit-save-repository-buffers t)

;; pass -v to commit so we can see the staged hunk
;; http://emacs.stackexchange.com/questions/3893
(advice-add #'magit-key-mode-popup-committing :after
            (lambda ()
              (magit-key-mode-toggle-option (quote committing) "--verbose")))

;; see issue #9
;; also:
;; https://github.com/magit/magit/commit/325a4fff
(setq magit-push-always-verify nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; haskell
;; see the link for documentation
;; https://github.com/haskell/haskell-mode

(eval-after-load "haskell-mode"
  '(progn
     (require 'haskell-mode)
     (require 'haskell-interactive-mode)
     (require 'haskell-process)

     (require 'flycheck)
     (require 'flycheck-haskell)
     (require 'ac-haskell-process)

     ;; keybindings
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-c C-d") 'ac-haskell-process-popup-doc)
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
     (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

     ;; ghci
     (setq haskell-process-type 'stack-ghci)
     (setq haskell-process-path-ghci "stack")
     (setq haskell-process-args-ghci "ghci")

     ;; custom vars
     (custom-set-variables
      '(haskell-process-suggest-remove-import-lines t)
      '(haskell-process-auto-import-loaded-modules t)
      '(haskell-process-log t)
      '(haskell-tags-on-save t)
      '(haskell-stylish-on-save t)

      ;; if t, causes emacs to hang
      '(haskell-process-suggest-no-warn-orphans nil)
      '(haskell-process-suggest-hoogle-imports nil)
      '(haskell-process-suggest-hayoo-imports nil)
      '(haskell-process-suggest-haskell-docs-imports nil)
      '(haskell-process-suggest-add-package nil)
      '(haskell-process-suggest-language-pragmas nil)
      '(haskell-process-suggest-remove-import-lines nil)
      '(haskell-process-suggest-overloaded-strings nil))

     ;; autocomplete
     (add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
     (add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
     (add-to-list 'ac-modes 'haskell-interactive-mode)

     ;; haskell-mode-hooks
     (let ((hooks (list
                   'haskell-doc-mode
                   'interactive-haskell-mode
                   'haskell-decl-scan-mode
                   'flycheck-mode
                   'flycheck-haskell-configure
                   'auto-complete-mode
                   )))
       (dolist (hook hooks)
         (add-hook 'haskell-mode-hook hook)))))

(speedbar-add-supported-extension ".hs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hungry delete
(global-hungry-delete-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Latex
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; jinja2
(autoload 'jinja2-mode "jinja2-mode")
(add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; matlab
(autoload 'matlab-mode "matlab" "Matlab Editing  Mode" t)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multiple cursors
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; nix mode
(autoload 'nix-mode "nix-mode" "Major mode for editing Nix expressions." t)
(push '("\\.nix\\'" . nix-mode) auto-mode-alist)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smex
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; the old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; visual regexp
(require 'visual-regexp-steroids)
(global-set-key (kbd "C-s")   'isearch-forward)
(global-set-key (kbd "C-r")   'isearch-backward)
(global-set-key (kbd "C-c s") 'vr/isearch-forward)
(global-set-key (kbd "C-c r") 'vr/isearch-backward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yaml-mode
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
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)


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
(my/package-install 'atom-dark-theme)
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
(my/package-install-list '(elpy
                           flymake
                           sphinx-doc))

(elpy-enable)
(add-hook 'python-mode-hook (lambda ()
                              (require 'sphinx-doc)
                              (sphinx-doc-mode t)))

;; configure nose for testing
;; TODO: see elpy docs for more details:
;;       http://elpy.readthedocs.org/en/latest/ide.html#testing
;; TODO: checkout tdd.el:
;;       https://github.com/jorgenschaefer/emacs-tdd/
(setq elpy-test-runner 'elpy-test-nose-runner)
(setq elpy-test-nose-runner-command '("nosetests" "--all-modules" "-s"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; automatically change entry to DONE when all children are DONE
(defun my/org/summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'my/org/summary-todo)

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

(my/switch-system-name
 '(("sulimo" . (set-face-attribute 'default nil :height 80))))

(my/switch-system-name
 '(("gambit" . (set-face-attribute 'default nil :height 80))))

(my/switch-system-name
 '(("lorien" . (set-face-attribute 'default nil :height 120))))
