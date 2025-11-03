;; -*- lexical-binding: t; -*-

(when (file-exists-p "~/.emacs.d/secrets")
  (load "~/.emacs.d/secrets"))

(set-face-attribute 'default nil :height 120 :family "Iosevka")

(load-theme 'modus-vivendi t)

;; For performance.
(setq bidi-display-reordering nil)

(setq inhibit-startup-screen t)

;; Turns off that bloody bell
(setq ring-bell-function #'ignore)

;; Replace yes/no by y/n
(fset 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format (concat invocation-name ": %b %f"))

(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-o") 'delete-blank-lines)

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

(setq kill-whole-line t)

(setq lisp-indent-offset 2)

(setq mouse-yank-at-point t)

(setq require-final-newline t)

(setq save-interprogram-paste-before-kill t)

(setq sentence-end-double-space nil)

(setq show-paren-mode t)

(setq tab-always-indent 'complete)

(setq tab-first-indent 'word-or-paren-or-punct)

(setq transient-mark-mode t)

(setq visible-bell nil)

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
(elpaca-wait)

(use-package diminish)

(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 200)
  (setq recentf-max-menu-items 15))

(use-package vertico
  :config
  (vertico-mode))

(use-package vertico-multiform
  :after vertico
  :ensure nil
  :config
  (add-to-list 'vertico-multiform-categories
    '(jinx grid (vertico-grid-annotate . 20) (vertico-count . 4)))
  (vertico-multiform-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-preview-key 'any))

(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1))

(use-package dired
  :ensure nil
  :hook
  ((dired-mode . dired-hide-details-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-free-space nil)
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"))

(use-package dired-subtree
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package jinx
  :ensure nil
  :diminish
  :hook
  ((LaTeX-mode . jinx-mode)
   (message-mode . jinx-mode)
   (markdown-mode . jinx-mode))
  :bind
  (("M-$" . jinx-correct)
   ("C-M-$" . jinx-languages))
  :init
  (setq jinx-languages "en_GB fr-classique"))

(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings)
  :bind
  (("S-<left>" . windmove-left)
   ("S-<down>" . windmove-down)
   ("S-<up>" . windmove-up)
   ("S-<right>" . windmove-right)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

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
   (haskell-mode . flyspell-prog-mode)))

(use-package lsp-haskell)

(use-package bazel)

(use-package typescript-mode)

(use-package hcl-mode)

(use-package yaml-mode)

(use-package csv-mode)

(use-package rust-mode)

(use-package protobuf-mode)

(use-package nix-mode)

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package dockerfile-mode)

(use-package undo-tree
  :diminish
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

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

;; Dependency of copilot
(use-package jsonrpc)

(use-package copilot
  :ensure (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el")))

(use-package nickel-mode)
