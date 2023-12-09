(load "~/.emacs.d/custom")
(load "~/.emacs.d/lib")
(load "~/.emacs.d/org")

(setq ring-bell-function #'(lambda ())) ; Turns off that bloody bell
(fset 'yes-or-no-p 'y-or-n-p)		; Replace yes/no by y/n
(setq frame-title-format (concat invocation-name ": %b %f"))

(global-set-key "\C-x\C-y" 'x-clipboard-yank)
(global-set-key (kbd "C-x SPC") 'just-one-space)
(global-set-key (kbd "M-SPC") 'dabbrev-expand)

;; Disable narrowing
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; For performance.
(setq bidi-display-reordering nil)

;; Transpose windows
(define-key ctl-x-4-map (kbd "t") 'transpose-windows)

;; Elisp editing
(setq lisp-indent-offset 2)

(setq
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil)

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

(setq compilation-finish-functions
      (lambda (buf str)
        (unless (string-match "exited abnormally" str)
          ;;no errors, make the compilation window go away in a few seconds
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!"))))

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

(use-package org-roam
  :diminish
  :config
  (setq org-roam-directory "~/org/notes")
  :hook
  (after-init . org-roam-mode))
