;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional useful functions and variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/joindirs (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join,
  (dotemacs-joindirs \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"
  (if (not dirs)
      root
    (apply 'my/joindirs
           (expand-file-name (car dirs) root)
           (cdr dirs))))


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

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple mode lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(el-get-bundle diminish)

;; major modes

(defun set-mode-name (hook dynamic-name)
  (lexical-let ((name dynamic-name)) ;; without lexical-let, access of
    ;; `name` in the lambda below
    ;; fails since `name` has changed
    ;; scope.
    (add-hook hook
              (lambda ()
                (setq mode-name name)))))

(set-mode-name 'emacs-lisp-mode-hook "EL")
(set-mode-name 'haskell-mode-hook "Λ")
(set-mode-name 'haskell-interactive-mode-hook "λ")

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (setq mode-name "EL")))

;; (add-hook 'haskell-mode-hook

;; minor modes
(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "color-identifiers-mode" '(diminish 'color-identifiers-mode))
(eval-after-load "git-gutter" '(diminish 'git-gutter-mode))
(eval-after-load "git-gutter+" '(diminish 'git-gutter+-mode))
(eval-after-load "auto-complete-mode" '(diminish 'auto-complete-mode))
(eval-after-load "aggressive-indent" '(diminish 'aggressive-indent-mode " Δ"))
(eval-after-load "subword-mode" '(diminish 'subword-mode))
(eval-after-load "hungry-delete" '(diminish 'hungry-delete-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install some useful packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO prelude packages https://github.com/bbatsov/prelude
;; TODO flyspell http://www.emacswiki.org/emacs/FlySpellx
;; TODO auctex
;;      - installed, configured
;;      - autocompletion (company-mode is recommended)
 


(el-get-bundle dockerfile-mode)
(el-get-bundle flx)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 80 column rules
;; (el-get-bundle column-enforce-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; aggressive indent
;; https://github.com/Malabarba/aggressive-indent-mode
(el-get-bundle aggressive-indent-mode)
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'nix-mode)
(add-to-list 'aggressive-indent-excluded-modes 'haskell-interactive-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto complete
(el-get-bundle auto-complete)
;; globally enable auto-complete
;; (global-auto-complete-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; color identifiers
(el-get-bundle color-identifiers-mode)
;; globally enable color-identifiers-mode
(add-hook 'after-init-hook 'global-color-identifiers-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; csv mode
(el-get-bundle csv-mode)
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ebnf configuration
(el-get-bundle jeramey/ebnf-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; encryption
(require 'epa-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; expand region
(el-get-bundle expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; emacs lisp mode
(add-hook 'emacs-lisp-mode-hook 'projectile-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flycheck
(el-get-bundle flycheck)
(el-get-bundle flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; git

(el-get-bundle magit)
(el-get-bundle git-gutter-fringe)
(el-get-bundle git-gutter-fringe+)

(when (window-system)
  (when (require 'git-gutter-fringe nil t)
    (global-git-gutter-mode 1)
    (setq-default indicate-buffer-boundaries 'left
                  (setq-default indicate-empty-lines 1))))

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

;; refresh status buffer when git tree changes
(el-get-bundle magit-filenotify)
(add-hook 'magit-status-mode-hook 'magit-filenotify-mode)    ; refresh on changes


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; haskell
;; see the link for documentation
;; https://github.com/haskell/haskell-mode


;; ;; not used right now
;; (el-get-bundle chrisdone/structured-haskell-mode
;;   :load-path "elisp")

(el-get-bundle haskell-mode)
(el-get-bundle ac-haskell-process)
(el-get-bundle flycheck-haskell)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(el-get-bundle flycheck-hdevtools)
(el-get-bundle hi2)
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)
;; (require 'flycheck-haskell)
(require 'ac-haskell-process)

(setq haskell-process-show-debug-tips nil)

;; keybindings
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-c C-d") 'ac-haskell-process-popup-doc)
(define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-left)
(define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-right)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

;; ghci
(setq haskell-process-type 'stack-ghci
      haskell-process-path-ghci "stack"
      haskell-process-args-ghci "ghci"

      haskell-process-auto-import-loaded-modules t
      haskell-process-log t
      haskell-process-suggest-add-package nil
      haskell-process-suggest-haskell-docs-imports nil
      haskell-process-suggest-hayoo-imports nil
      haskell-process-suggest-hoogle-imports nil
      haskell-process-suggest-language-pragmas nil
      haskell-process-suggest-no-warn-orphans nil
      haskell-process-suggest-overloaded-strings nil
      haskell-process-suggest-remove-import-lines nil
      haskell-stylish-on-save t
      haskell-tags-on-save t)


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
              'projectile-mode
              'turn-on-hi2
              )))
  (dolist (hook hooks)
    (add-hook 'haskell-mode-hook hook)))

(add-hook 'haskell-cabal-mode-hook 'projectile-mode)
(add-hook 'haskell-interactive-mode-hook 'projectile-mode)
(add-hook 'haskell-mode-hook #'aggressive-indent-mode)
(add-hook 'haskell-mode-hook 'subword-mode)

;; (speedbar-add-supported-extension ".hs")

(el-get-bundle smartscan)
(add-hook 'haskell-mode-hook 'smartscan-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hungry delete
(el-get-bundle hungry-delete)
(global-hungry-delete-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ido
;; Interactively do things
(ido-mode 1)
(ido-everywhere t)
(require 'flx-ido) ; flexible string matching
(flx-ido-mode t)
;; disable ido faces to see flx highlighting
(setq ido-enable-flex-matching t
      ido-use-faces nil
      ido-everywhere t)
(flx-ido-mode 1)

;; use ido vertically (easier to read)
(el-get-bundle ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down) ; for arrow keys


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; jinja2
(el-get-bundle jinja2-mode)
(autoload 'jinja2-mode "jinja2-mode")
(add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Latex

;; (el-get-bundle auctex)
;; (el-get-bundle auto-complete-auctex)

;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)
;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (add-hook 'LaTeX-mode-hook 'auto-complete-mode)

;; ; compile to PDF
;; (setq TeX-PDF-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; markdown
(el-get-bundle markdown-mode)
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; matlab
;;(el-get-bundle matlab-mode)
;; (autoload 'matlab-mode "matlab" "Matlab Editing  Mode" t)
;; (add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
;; (setq matlab-indent-function t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multiple cursors
(el-get-bundle multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; nix mode
(el-get-bundle nix-mode)
(autoload 'nix-mode "nix-mode" "Major mode for editing Nix expressions." t)
(push '("\\.nix\\'" . nix-mode) auto-mode-alist)
(add-hook 'nix-mode-hook 'projectile-mode)

;; WIP
(el-get-bundle company-mode)
(el-get-bundle nixos-options)
(el-get-bundle travisbhartwell/nix-emacs)
;;(add-to-list 'company-backends 'company-nixos-options)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; powerline

(el-get-bundle (:name powerline
                      :description "Update of original Emacs Powerline"
                      :type Github
                      :pkgname "milkypostman/powerline"))
(powerline-center-theme)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; project management

;; projectile
(el-get-bundle projectile)
(setq projectile-enable-idle-timer nil
      projectile-indexing-method 'native
      projectile-enable-caching t)

;; helm
(el-get-bundle helm)
(el-get-bundle helm-helm-commands)
(el-get-bundle helm-ag)
(el-get-bundle helm-ls-git)
(el-get-bundle helm-descbinds)
(el-get-bundle helm-projectile)
(helm-descbinds-mode)
;; (helm-projectile-on)
(set-face-attribute 'helm-selection nil
                    :background "black")
(helm-autoresize-mode t)

(setq helm-M-x-fuzzy-match t
      helm-recentf-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-locate-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-lisp-fuzzy-completion t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'ibuffer-list-buffers)
(global-set-key (kbd "C-c h m") 'helm-imenu)
(global-set-key (kbd "C-c h S-M") 'helm-imenu-in-all-buffers)
(global-set-key (kbd "C-h a") 'helm-apropos)
(global-set-key (kbd "C-c h k") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h r") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)

;; ag (silver searcher
(el-get-bundle ag)
(el-get-bundle helm-ag)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rainbow delimiters
(el-get-bundle rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smex

;; (el-get-bundle smex)
;; (smex-initialize)

                                        ;(global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "C-M-x") 'smex-major-mode-commands)

;; the old M-x
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; visual regexp
;; (require 'visual-regexp-steroids)
;; (global-set-key (kbd "C-s")   'isearch-forward)
;; (global-set-key (kbd "C-r")   'isearch-backward)
;; (global-set-key (kbd "C-c s") 'vr/isearch-forward)
;; (global-set-key (kbd "C-c r") 'vr/isearch-backward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yasnippet
(el-get-bundle yasnippet)
(yas-global-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yaml-mode
(el-get-bundle yaml-mode)
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


(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; upcase/downcase regions is nice
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)
(show-paren-mode t)
(global-hl-line-mode t)
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

;; https://stackoverflow.com/questions/4506249/
(setq browse-url-browser-function 'browse-url-xdg-open)




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; themes
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle atom-dark-theme)

(require 'atom-dark-theme)
(load-theme 'atom-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python ide stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle elpy)
(el-get-bundle flymake)
(el-get-bundle jedi)
(el-get-bundle sphinx-doc)

(el-get-bundle virtualenvwrapper)
(venv-initialize-interactive-shells)

;; this requires jedi, flake8, and pyflakes to be availables.
;; The easiest path is to install them globally

;; elpy: ide
;; flymake: on-the-fly checks
;; sphinx-doc: autoinsert sphinx-doc docstrings
;;             (C-c M-d at function def)

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

;; Track the current stable version. Copied. directly from el-get's
;; org-mode.rcp with the addition of the :checkout parameter
(el-get-bundle (:name org-mode
                      :website "http://orgmode.org/"
                      :type git
                      :url "git://orgmode.org/org-mode.git"
                      :checkout "maint"
                      :info "doc"
                      :build/berkeley-unix `,(mapcar
                                              (lambda (target)
                                                (list "gmake"
                                                      target
                                                      (concat  "EMACS=" (shell-quote-argument el-get-emacs))))
                                              '("oldorg"))
                      :build `,((mapcar  )
                                (lambda (target)
                                  (list "make"
                                        target
                                        (concat "EMACS=" (shell-quote-argument el-get-emacs))))
                                '("oldorg"))
                      :load-path ("." "contrib/lisp" "lisp")
                      :load ("lisp/org-loaddefs.el")))


(load "~/.org.el")


;; ;; automatically change entry to DONE when all children are DONE
;; (defun my/org/summary-todo (n-done n-not-done)
;;   "Switch entry to DONE when all subentries are done, to TODO otherwise."
;;   (let (org-log-done org-log-states)   ; turn off logging
;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
;; (add-hook 'org-after-todo-statistics-hook 'my/org/summary-todo)

;; (setq org-hide-leading-stars t)

;; ;; block changes to DONE that have incomplete dependencies
;; (setq org-enforce-todo-dependencies t
;;       org-enforce-todo-checkbox-dependencies t)


;; ;; provide statistics
;; (setq org-provide-todo-statistics t
;;       org-hierarchical-todo-statistics nil)

;; ;; enable inline graphviz images
;; (el-get-bundle graphviz-dot-mode)

;; (setq org-src-preserve-indentation nil)

;; ;; enable language execution

;; (el-get-bundle htmlize)
;; (org-babel-do-load-languages
;;  'org-babel-load-languages '((dot . t)
;;                              (sh . t)
;;                              (python . t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; irc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(el-get-bundle! erc-highlight-nicknames)


(defun start-irc ()
  (interactive)
  (load-library "~/.secrets.el.gpg")
  (require 'erc)

  (require 'erc-truncate)
  (erc-truncate-mode 1)
  ;; override if needed, default is 30,000
  (setq erc-max-buffer-size 5000)

  (require 'erc-services)
  (erc-services-mode 1)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-nickserv-passwords
        `((freenode (("badi" . ,irc-freenode-badi-pass)))))

  (require 'erc-spelling)
  (erc-spelling-mode 1)

  (and (require 'erc-highlight-nicknames)
       (add-to-list 'erc-modules 'highlight-nicknames)
       (erc-update-modules))

  (require 'erc-nicklist)
  (require 'tls)
  (erc-tls :server "chat.freenode.net" :port 6697))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load specific overrides based on system type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(my/switch-system-name
 '(("gambit" . (set-face-attribute 'default nil :height 80))))

(my/switch-system-name
 '(("lorien" . (set-face-attribute 'default nil :height 120))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((project-venv-name . "workflow")
     (project-venv-name . "virtual-cluster-libs")
     (enforce-mode)
     (column-epa-armor . t)
     (project-venv-name . "sempl")
     (project-venv-name . "venv")
     (org-confirm-babel-evaluate)
     (epa-armor . t)))))


(global-set-key (kbd "C-S-<right>") #'next-buffer)
(global-set-key (kbd "C-S-<left>")  #'previous-buffer)
(global-set-key (kbd "C-M-<right>") #'next-multiframe-window)
(global-set-key (kbd "C-M-<left>")  #'previous-multiframe-window)
(put 'narrow-to-region 'disabled nil)
