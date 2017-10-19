
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
      '(("melpa" . "http://melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ))

(setq package-enable-at-startup nil)	; stops multiple initializations
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(require 'diminish)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; basic functionality

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(line-number-mode t)
(global-linum-mode t)
(column-number-mode t)
(show-paren-mode t)
(global-hl-line-mode t)
(delete-selection-mode t)               ; typed text delete selection first
(transient-mark-mode t)

(setq-default indent-tabs-mode nil)     ; use spaces instead of tabs
(setq tab-width 2)

;; detect words in eg camelcase
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
(add-hook 'prog-mode-hook #'subword-mode)

;; use versioned backups
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/saves"))
 delete-old-versions t
 kept-new-versions 6
 keep-old-versions 2
 version-control 5)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; appearance

(set-face-attribute 'default nil :height 100)

(my/switch-system-name
 '(("fangorn" . (set-face-attribute 'default nil :height 100))))


(use-package atom-dark-theme :disabled t)
(use-package naquadah-theme :ensure t)

(use-package powerline
  ;; https://github.com/milkypostman/powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package rainbow-identifiers
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

(use-package rainbow-delimiters
  ;; https://github.com/fanael/rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package hungry-delete
  ;; Delete multiple whitespace characters at once
  ;; https://github.com/nflath/hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; editing and tools

(use-package ag
  ;; silver searcher integration
  :ensure t)

(use-package avy
  ;; Hit C-. and type to quickly jump to any character in the buffer
  ;; https://github.com/abo-abo/avy
  :ensure t
  :bind
  ("C-." . avy-goto-char)
  :config
  (avy-setup-default))

(use-package bm
  ;; Visual bookmarks (highlight and navigation)
  ;; https://github.com/joodland/bm
  :ensure t
  :bind
  (("<f1>" . bm-toggle)
   ("<f2>" . bm-next)
   ("<f3>" . bm-previous)))


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
  :config
  (eval-after-load "flycheck"
    '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(use-package flyspell
  ;; https://www.emacswiki.org/emacs/FlySpell
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package magit
  ;; the magical git interface!
  ;; https://github.com/magit/magit
  :ensure t
  :config
  (setq
   magit-save-repository-buffers t
   magit-restore-window-configuration t)
  :bind
  ("<f4>" . magit-status))

(use-package magit-filenotify
  ;; display autodetected file changes in status buffer
  ;; https://github.com/ruediger/magit-filenotify
  :ensure t
  :config
  (add-hook 'after-save-hook #'magit-after-save-refresh-status))

(use-package multiple-cursors
  ;; https://github.com/magnars/multiple-cursors.el
  :ensure t
  :config
  (global-set-key (kbd "C->") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") #'mc/mark-previous-like-this))

(use-package org
  :ensure t)

(use-package projectile
  ;; https://github.com/bbatsov/projectile
  :ensure t
  :init
  (setq
   projectile-enable-idle-timer nil
   projectile-indexing-method 'native
   projectile-enable-caching t)
  :config
  (add-to-list 'projectile-globally-ignored-directories ".stack-*")
  (add-to-list 'projectile-globally-ignored-directories ".pyc")
  (projectile-mode))


(use-package helm
  ;; https://github.com/emacs-helm/heml
  :ensure t
  :init
  (setq
   helm-M-x-fuzzy-match t
   helm-recentf-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-locate-fuzzy-match t
   helm-semantic-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-apropos-fuzzy-match t
   helm-lisp-fuzzy-completion t
   helm-autoresize-mode t)
  :config
  (require 'helm-config)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "M-y") #'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") #'helm-mini)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm-ag
  ;; https://github.com/syohex/emacs-helm-ag)
  :ensure t)

(use-package helm-descbinds
  ;; 1. C-h b: list bindings
  ;; 2. search for bindings
  ;; 3. C-z gets persistent description
  ;; https://github.com/emacs-helm/helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode))

(use-package helm-helm-commands
  :disabled t)

(use-package helm-flx
  :ensure t
  :config
  (helm-flx-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; mode for working with files

(use-package csv-mode
  ;; csv editing/viewing mode
  ;; https://elpa.gnu.org/packages/csv-mode.html
  :ensure t)

(use-package d-mode
  ;; edit D-Lang files
  ;; https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package erlang
  :ensure t)

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

(use-package markdown-mode
  ;; https://jblevins.org/projects/markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode
  (("\\.md\\'" . gfm-mode)))

(use-package nix-mode
  :ensure t)

(use-package terraform-mode
  ;; https://github.com/syohex/emacs-terraform-mode
  :ensure t)

(use-package yaml-mode
  :ensure t
  :config
  (autoload 'yaml-mode "yaml-mode"))

(use-package rust-mode
  :ensure t)

(use-package toml-mode
  :ensure t)

(use-package cargo
  :ensure t)
