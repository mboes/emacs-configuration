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
(add-hook 'after-save-hook
          (lambda ()
            (if (eq major-mode 'emacs-lisp-mode)
                (save-excursion (byte-compile-file buffer-file-name)))))

(setq
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil)

;; Bootstrap straight.el.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package bind-key)

(use-package base16-theme
  :config
  (load-theme 'base16-tomorrow-night t))

(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (setq ido-max-dir-file-cache 0)
  :hook (file-file . ido-save-history)
  :bind ("M-i" . ido-goto-symbol))

(use-package flx-ido
  :config
  (flx-ido-mode 1)
  (setq ido-use-faces nil))    ; disable ido faces to see flx highlights.

(use-package flyspell
  :config
  (setq flyspell-abbrev-p nil)         ; Don't save abbreviations
  (setq flyspell-sort-corrections nil)
  :hook
  ((LaTeX-mode . flyspell-mode)
   (LaTeX-mode . reftex-mode)
   (message-mode . turn-on-flyspell)
   (markdown-mode . turn-on-flyspell)))

(use-package windmove
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

;; Load Dired-x when Dired is loaded.
(require 'dired-x)

(use-package diminish)

(use-package bazel-mode)

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
  (global-undo-tree-mode))

(use-package projectile
  :diminish
  :config
  (projectile-mode))

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
