;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(setq frame-inhibit-implied-resize t)

(setq load-prefer-newer t)

;; Hide UI chrome
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Turn off GC during startup, for speed.
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; Re-enable GC after startup.
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))
