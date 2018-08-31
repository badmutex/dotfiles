;; customizations kept elsewhere to keep things clean here
(setq custom-file
      (expand-file-name "custom-vars.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions I may use here

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; setup package management

(require 'package)

(setq package-archives
      '(
        ("melpa" . "http://melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
	))

(setq package-enable-at-startup nil)	; stops multiple initializations
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; server
(if (and (fboundp 'server-running-p)
	 (not (server-running-p)))
    (server-start))

(use-package exec-path-from-shell
  ;; https://github.com/purcell/exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(line-number-mode t)
(column-number-mode t)
(show-paren-mode t)
(global-hl-line-mode t)
(pending-delete-mode t)
(transient-mark-mode t)
(global-linum-mode t)


;; don't use tabs, just spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; tramp
(setq tramp-default-mode "scp")
;; https://www.emacswiki.org/emacs/TrampMode#toc12
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

(add-hook 'prog-mode-hook #'subword-mode)
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html

(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; clear whitespace on save


;;; use versioned backups, don't clobber symlinks, don't litter fs tree
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control 5)

;; (use-package linum-ex
;;   ;; linum-mode causes performance problems.
;;   ;; linum-ex modifies it to use display line numbers on demand
;;   ;; https://www.emacswiki.org/emacs/linum-ex.el
;;   :ensure nil
;;   :init
;;   (global-linum-mode 0)
;;   :config
;;   (global-linum-mode t))


(my/switch-system-name
 '(("fangorn" . (set-face-attribute 'default nil :height 100))))

(use-package naquadah-theme
  :ensure t)

(use-package powerline
  ;; https://github.com/milkypostman/powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package rainbow-identifiers
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  ;; https://github.com/abo-abo/avy
  :ensure t
  :bind
  (("C-'" . avy-goto-char)
   ("C-S-\"" . avy-pop-mark))
  :config
  (avy-setup-default))

(use-package ace-window
  :ensure t)

(use-package bm
  ;; https://github.com/joodland/bm
  :ensure t
  :bind
  (("<f1>" . bm-toggle)
   ("<f2>" . bm-next)
   ("<f3>" . bm-previous)))

(use-package csv-mode
  ;; https://elpa.gnu.org/packages/csv-mode.html
  :ensure t)

(use-package d-mode
  ;; https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode
  :ensure t)

(use-package diminish
  :ensure t)

(use-package direnv
  :ensure t
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package dockerfile-mode
  :ensure t)

(use-package erlang
  :ensure t)

(use-package flycheck
  ;; http://www.flycheck.org/en/latest/
  ;; C-c ! C-c	flycheck-compile
  ;; C-c ! C-w	flycheck-copy-errors-as-kill
  ;; C-c ! ?		flycheck-describe-checker
  ;; C-c ! C		flycheck-clear
  ;; C-c ! H		display-local-help
  ;; C-c ! V		flycheck-version
  ;; C-c ! c		flycheck-buffer
  ;; C-c ! e		flycheck-explain-error-at-point
  ;; C-c ! h		flycheck-display-error-at-point
  ;; C-c ! i		flycheck-manual
  ;; C-c ! l		flycheck-list-errors
  ;; C-c ! n		flycheck-next-error
  ;; C-c ! p		flycheck-previous-error
  ;; C-c ! s		flycheck-select-checker
  ;; C-c ! v		flycheck-verify-setup
  ;; C-c ! x		flycheck-disable-checker
  :ensure t
  :init
  (global-flycheck-mode))

(use-package flycheck-color-mode-line
  ;; https://github.com/flycheck/flycheck-color-mode-line
  :ensure t
  :after (flycheck-inline)
  :config
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
  (flycheck-inline-mode))

(use-package flycheck-inline
  ;; https://github.com/flycheck/flycheck-inline
  :ensure t
  :after (quick-peek)
  :config
  (setq flycheck-inline-display-function
      (lambda (msg pos)
        (let* ((ov (quick-peek-overlay-ensure-at pos))
               (contents (quick-peek-overlay-contents ov)))
          (setf (quick-peek-overlay-contents ov)
                (concat contents (when contents "\n") msg))
          (quick-peek-update ov)))
      flycheck-inline-clear-function #'quick-peek-hide))

(use-package flyspell
  ;; https://www.emacswiki.org/emacs/FlySpell
  :ensure t
  :init
  (setq flyspell-issue-message-flag nil)
  :config
  ;; https://www.emacswiki.org/emacs/FlySpell#toc13
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  ;; https://stackoverflow.com/questions/16084022/emacs-flyspell-deactivate-c-key-binding#16085470
  (define-key flyspell-mode-map (kbd "C-.") nil))

(use-package graphviz-dot-mode
  :ensure t)

;; (use-package intero
;;   :ensure t
;;   :config
;;   (add-hook 'haskell-mode-hook #'intero-mode))

(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-process-type 'auto)
  (setq haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans"
                                          "--no-build"
                                          "--no-load"))
  (setq haskell-tags-on-save t)
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)
  :bind
  (:map haskell-mode-map
        (("C-c C-l" . haskell-process-load-file)
         ("C-c C-z" . haskell-interactive-switch)
         ("C-c C-n C-t" . haskell-process-do-type)
         ("C-c C-n C-i" . haskell-process-do-info)
         ("C-c C-z" . haskell-interactive-switch)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c C-c" . haskell-process-cabal-build)
         ("C-c c" . haskell-process-cabal)))
  :bind
  (:map haskell-cabal-mode-map
        (("C-c C-z" . haskell-interactive-switch)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c C-c" . haskell-process-cabal-build)
         ("C-c c" . haskell-process-cabal))))


(use-package hindent
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package hungry-delete
  ;; https://github.com/nflath/hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package lua-mode
  :ensure t)

(use-package magit
  ;; https://github.com/magit/magit
  :ensure t
  :config
  (setq magit-save-repository-buffers t
	magit-restore-window-configuration t)
  :bind
  ("<f4>" . magit-status))

(use-package magit-filenotify
  ;; https://github.com/ruediger/magit-filenotify
  :ensure t
  :config
  (add-hook 'after-save-hook #'magit-after-save-refresh-status))

(use-package markdown-mode
  ;; http://jblevins.org/projects/markdown-mode/
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode
  (("\\.md\\'" . gfm-mode)))

(use-package multiple-cursors
  ;; https://github.com/magnars/multiple-cursors.el
  :ensure t
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(use-package nix-mode
  :ensure t)

(use-package flycheck-plantuml
  :mode
  (("\\.plantuml\\'" . plantuml-mode))
  :config
  (setq platuml-jar-path "/Users/badi/.nix-profile/bin/plantuml")
  :ensure t)

(use-package plantuml-mode
  :ensure t)

(use-package rainbow-delimiters
  ;; https://github.com/Fanael/rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  ;; https://github.com/Fanael/rainbow-identifiers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org$" . org-mode))
  )

(use-package terraform-mode
  ;; https://github.com/syohex/emacs-terraform-mode
  :ensure t)

(use-package yaml-mode
  :ensure t
  :config
  (autoload 'yaml-mode "yaml-mode"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  ;; https://github.com/bbatsov/projectile
  :ensure t
  :init
  (setq projectile-enable-idle-timer nil
	projectile-indexing-method 'native
	projectile-enable-caching t)
  :config
  (add-to-list 'projectile-globally-ignored-directories ".stack-*")
  (add-to-list 'projectile-globally-ignored-directories ".pyc")
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package ag
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package helm :ensure t
  ;; https://github.com/emacs-helm/helm
  :init
  (setq helm-M-x-fuzzy-match t
	helm-recentf-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-locate-fuzzy-match t
	helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match t
	helm-apropos-fuzzy-match t
	helm-lisp-fuzzy-completion t
	helm-autoresize-mode t)
  :config
  ;; https://tuhdo.github.io/helm-intro.html
  (require 'helm-config)
  (require 'helm-projectile)
  (helm-projectile-on)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "M-y") #'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") #'helm-mini)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  ;; TODO: helm-semantic-or-imenue
  )

(use-package helm-ag
  ;; https://github.com/syohex/emacs-helm-ag
  :ensure t)

(use-package helm-descbinds
  ;; https://github.com/emacs-helm/helm-descbinds
  ;; 1. C-h b: list bindings
  ;; 2. search for binding
  ;; 3. C-z to get persistent description
  :ensure t
  :config
  (helm-descbinds-mode))

;; (use-package helm-helm-commands
;;   ;; https://github.com/nonsequitur/helm-helm-commands
;;   :ensure t)


(use-package helm-flx
  :ensure t
  :config (helm-flx-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elpy
  ;; https://elpy.readthedocs.io/en/latest/introduction.html
  :ensure t
  :config
  (setq pyvenv-workon "adroll")
  (elpy-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rust-mode
  ;; https://github.com/rust-lang/rust-mode
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package flycheck-rust
  :ensure t)

(use-package toml-mode :ensure t)
(use-package cargo :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package quick-peek
  ;; https://github.com/cpitclaudel/quick-peek
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Extra


;; find aspell and hunspell automatically
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
(cond
 ;; try hunspell at first
  ;; if hunspell does NOT exist, use aspell
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  ;; (setq ispell-local-dictionary-alist
  ;;       ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
  ;;       ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
  ;;       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
  ;;         ))
  )

 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  ;; (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO
;; (use-package ace-window	      ; TODO
;;   ;; https://github.com/abo-abo/ace-window
;;   )

;; (use-package hi-lock-mode		; TODO
;;   ;; http://doc.endlessparentheses.com/Fun/hi-lock-mode.html
;;   )

;; nix-buffer https://github.com/travisbhartwell/nix-emacs


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
