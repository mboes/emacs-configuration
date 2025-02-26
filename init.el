;; -*- lexical-binding: t; -*-

(when (file-exists-p "~/.emacs.d/secrets")
  (load "~/.emacs.d/secrets"))

(set-face-attribute 'default nil :height 120 :family "Iosevka")

;; For performance.
(setq bidi-display-reordering nil)

(setq inhibit-startup-screen t)

;; Turns off that bloody bell
(setq ring-bell-function #'ignore)

;; Replace yes/no by y/n
(fset 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format (concat invocation-name ": %b %f"))

(global-set-key "\C-x\C-y" 'x-clipboard-yank)
(global-set-key (kbd "C-x SPC") 'just-one-space)
(global-set-key (kbd "M-SPC") 'dabbrev-expand)

;; Disable narrowing
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil)

(global-auto-revert-mode 1)
(global-hl-line-mode 1)

(setq calendar-date-style 'european)
(setq calendar-week-start-day 1)

(setq column-number-mode t)

(setq display-time-24hr-format t)
(setq display-time-mode t)

(setq fill-nobreak-predicate '(fill-french-nobreak-p))

(setq ispell-local-dictionary "en_GB-ize-w_accents")

(setq kill-whole-line t)

(setq lisp-indent-offset 2)

(setq mouse-yank-at-point t)

(setq require-final-newline t)

(setq save-interprogram-paste-before-kill t)

(setq sentence-end-double-space nil)

(setq show-paren-mode t)

(setq transient-mark-mode t)

(setq visible-bell nil)

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
(elpaca-wait)

(use-package diminish)

(use-package base16-theme
  :config
  (load-theme 'base16-tomorrow-night t))

(use-package recentf
  :elpaca nil
  :config
  (setq recentf-max-saved-items 200)
  (setq recentf-max-menu-items 15))

(use-package selectrum
  :config
  (selectrum-mode +1)
  :bind
  ("C-x C-z" . #'selectrum-repeat))

(use-package selectrum-prescient
  :after selectrum
  :config
  (selectrum-prescient-mode +1))

(use-package ctrlf
  :config
  (ctrlf-mode +1))

(use-package flyspell
  :elpaca nil
  :config
  (setq flyspell-abbrev-p nil)         ; Don't save abbreviations
  (setq flyspell-sort-corrections nil)
  :hook
  ((LaTeX-mode . flyspell-mode)
   (LaTeX-mode . reftex-mode)
   (message-mode . turn-on-flyspell)
   (markdown-mode . turn-on-flyspell)))

(use-package windmove
  :elpaca nil
  :config
  (windmove-default-keybindings)
  :bind
  (("S-<left>" . windmove-left)
   ("S-<down>" . windmove-down)
   ("S-<up>" . windmove-up)
   ("S-<right>" . windmove-right)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands
  lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package haskell-mode
  :config
  (setq haskell-stylish-on-save nil)
  (setq haskell-tags-on-save nil)
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
	 ("\\.chs\\'" . haskell-mode)
         ("\\.lhs\\'" . haskell-literate-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :hook
  ((haskell-mode . haskell-indentation-mode)
   (haskell-mode . (lambda () (set-fill-column 80)))
   (haskell-mode . flyspell-prog-mode)
   (haskell-mode . #'lsp)
   (haskell-literate-mode . #'lsp)))

(use-package lsp-haskell)

(use-package bazel)

(use-package typescript-mode)

(use-package hcl-mode)

(use-package yaml-mode)

(use-package csv-mode)

(use-package rust-mode)

(use-package protobuf-mode)

(use-package nix-mode)

(use-package markdown-mode)

(use-package dockerfile-mode)

(use-package undo-tree
  :diminish
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

(use-package projectile
  :diminish
  :config
  (projectile-mode))

;; Built-in package, but transient wants a newer version. See
;; https://github.com/progfolio/elpaca/issues/216

(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))
(use-package seq :elpaca `(seq :build ,(+elpaca-seq-build-steps)))

;; Dependency of Magit
(use-package transient
  :config
  (setq transient-save-history nil))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package forge
  :after magit)

(use-package smartparens
  :diminish
  :config
  (smartparens-global-mode t))

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

(use-package ledger-mode
  :config
  (setq ledger-accounts-file "~/finances/accounts.ledger")
  (setq ledger-default-date-format ledger-iso-date-format))
