(setq org-agenda-files '("~/org/"))

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(add-hook 'message-mode-hook 'turn-on-orgstruct++)

;; Misc settings
(setq org-completion-use-ido t)
(setq org-log-done t)
(setq org-log-into-drawer t)
(setq org-replace-disputed-keys t)
(setq org-use-speed-commands t)
(setq org-hide-leading-stars t)
(setq org-pretty-entities t)
(setq org-export-with-toc nil)
(setq org-enable-priority-commands nil)
(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-startup-indented t)
(setq org-cycle-separator-lines 0)


;; Todo keywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)" "ARCHIVE(a)")
              (sequence "WAIT(w@/!)" "|" "CNCL(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "#cb4b16" :weight bold)
	      ("NEXT" :foreground "#d33682" :weight bold)
	      ("DONE" :foreground "#546e00" :weight bold)
              ("WAIT" :foreground "#cb4b16" :weight bold)
              ("CNCL" :foreground "#00629d" :weight bold))))

;; Agenda commands
(setq org-agenda-custom-commands
      '(("k" "Kanban" ((todo "DOING") (todo "TODAY") (todo "THISWEEK") (todo "WAIT") (todo "TODO") (todo "DONE") (todo "CANCEL")))))

;; Clock settings
(setq org-clock-into-drawer t)
(setq org-clock-out-when-done t)
(setq org-clock-out-remove-zero-time-clocks t)

;; Capturing and refiling
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org")
               "* TODO %?\n  %U\n  %i"
	       :clock-in t
	       :clock-resume t)
              ("n" "note" entry (file "~/org/refile.org")
               "* %? :NOTE:\n  %U\n\n  %i"
	       :clock-in t
	       :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/org/diary.org")
               "* %?\n     %U\n  %i"
	       :clock-in t
	       :clock-resume t)
              ("w" "org-protocol" entry (file "~/org/refile.org")
               "* TODO Review %c\n  %U\n  %i"
	       :immediate-finish t)
              ("p" "Phone call" entry (file "~/org/refile.org")
               "* PHONE %? :PHONE:\n  %U"
	       :clock-in t
	       :clock-resume t)
              ("h" "Habit" entry (file "~/org/refile.org")
               "* NEXT %?\n%U\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n  %i")
	      ("l" "Link" entry (file "~/org/refile.org")
	       "* %c\n  %i"
	       :immediate-finish t)
	      ("e" "Event" entry (file+headline "~/org/org.org.gpg" "Calendar")
	       "** %?\n   SCHEDULED: %^t"
	       :clock-in t
	       :clock-resume t))))

(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3))))
(setq org-refile-use-outline-path nil)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes (quote confirm))
(setq org-refile-target-verify-function 'mb/verify-refile-target)

(defun mb/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; Add CREATED tag.
(defun mb/inactive-time-stamp (fmt)
  (concat "[" (substring fmt 1 -1) "]"))

(defadvice org-insert-todo-heading (after mb/insert-created-property activate)
  (let* ((fmt (cdr org-time-stamp-formats))
	 (stamp (format-time-string fmt (org-current-time))))
    (org-entry-put (point) "CREATED" stamp)))

(ad-activate 'org-insert-todo-heading)

;; Archiving
(setq org-archive-location "%s_archive::* Archived Tasks")

(defun mb/org-todo ()
  (interactive)
  (widen)
  (org-narrow-to-subtree)
  (org-show-todo-tree nil))

(defun mb/widen ()
  (interactive)
  (widen)
  (org-reveal))

(defun gtd ()
  (interactive)
  (find-file "~/org/org.org.gpg"))

(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?E)
                            ("@work" . ?W)
                            ("@home" . ?H)
                            (:endgroup)
                            ("mail" . ?m)
                            ("write" . ?w)
                            ("phone" . ?c)
                            ("code" . ?c))))

;; Make electric indent behave properly.
(add-hook 'org-mode-hook
          (lambda ()
            (set (make-local-variable 'electric-indent-functions)
                 (list (lambda (arg) 'no-indent)))))

(add-hook 'org-mode-hook
          (lambda ()
            (set (make-local-variable 'electric-indent-functions)
                 (list (lambda (arg) 'no-indent)))))
