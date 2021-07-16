;; Recommended by consult documentation:
;; -*- lexical-binding: t -*-

;; From straight.el README:
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

;; Makes each use-package declaration also invoke straight to install package:
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(add-to-list 'custom-theme-load-path "~/.noivy-emacs.d/themes")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ben Smith"
      user-mail-address "bensmithmath@gmail.com")

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(defun highlight-selected-window ()
  "Highlight selected window with a different background color."
  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
                      (buffer-face-set '(:background "#373737"))))))
  (buffer-face-set 'default))
(add-hook 'buffer-list-update-hook 'highlight-selected-window)

(use-package diminish) ;gets rid of lighters with use-package :diminish

(use-package delight
  :config
  (delight `((buffer-face-mode nil "face-remap"))))

(use-package yascroll
  :config
  (global-yascroll-bar-mode 1)
  (setq yascroll:delay-to-hide nil))

(use-package magit)

(use-package evil
  :diminish
  :init
  (setq evil-search-module "evil-search")
  (setq evil-cross-lines t)
  (setq evil-want-integration t) ;; recommended for evil-collection
  (setq evil-want-keybinding nil) ;; required for evil-collection
  (setq evil-want-fine-undo t)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (global-set-key (kbd "C-<tab>") 'evil-window-next)
  (global-set-key (kbd "<C-iso-lefttab>") 'evil-window-prev))

;; subsumed by evil-collection
;; (use-package evil-magit
;;   :after magit evil)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (setq evil-surround-pairs-alist (delete  '(40 . ("( " . " )")) evil-surround-pairs-alist))
  (setq evil-surround-pairs-alist (delete  '(91 . ("[ " . " ]")) evil-surround-pairs-alist))
  (setq evil-surround-pairs-alist (delete  '(123 . ("{ " . " }")) evil-surround-pairs-alist))
  (push  '(40 . ("(" . ")")) evil-surround-pairs-alist)
  (push  '(91 . ("[" . "]")) evil-surround-pairs-alist)
  (push  '(123 . ("{" . "}")) evil-surround-pairs-alist))

(use-package undo-fu
  :config
  (define-key evil-normal-state-map (kbd "C-/") 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-?") 'undo-fu-only-redo))

(use-package emacs
  :config 
  (load-theme 'zenburnt t)
  (set-frame-font "DejaVu Sans Mono 11" nil t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (recentf-mode) ;;needed for virtual buffers in consult
  (setq mouse-wheel-scroll-amount '(1))
  ;;(fringe-mode nil)
  ;; (global-visual-line-mode)
  (setq visible-bell t)
  (setq inhibit-splash-screen t)
;  (setq meta-prefix-char nil) ;stops ESC from being weird ; breaks ALT completely!
  (show-paren-mode)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "C-s-f") 'find-file-other-window)
  (global-set-key (kbd "s-e") 'eval-last-sexp)
  (global-set-key (kbd "s-d") 'kill-buffer)
  (global-set-key (kbd "s-0") 'delete-window)
  (global-set-key (kbd "s-1") 'delete-other-windows)
  (global-set-key (kbd "s-2") 'split-window-below)
  (global-set-key (kbd "s-3") 'split-window-right)
  (global-set-key (kbd "s-4") 'ctl-x-4-prefix)
  (global-set-key (kbd "S-SPC") 'completion-at-point)
  (setq dired-listing-switches "-alh")
  (setq-default custom-file null-device)
  ;; (setq completion-cycle-threshold 3) ;; maybe?
  (setq tab-always-indent 'complete)

  ;; (global-set-key (kbd "C-S-g") 'ben/quit-minibuffer)
  ;; (defun ben/quit-minibuffer ()
  ;;   "Quit the minibuffer from anywhere."
  ;;   (interactive)
  ;;   (with-selected-window (active-minibuffer-window)
  ;;     (execute-kbd-macro (kbd "C-g"))))
  ;; I like this one more:
  (global-set-key (kbd "C-g") 'ben/keyboard-escape-quit)
  (defun ben/keyboard-escape-quit ()
    "Just like `keyboard-escape-quit` but doesn't close other windows."
    (interactive)
    (cond ((eq last-command 'mode-exited) nil)
	  ((region-active-p)
	   (deactivate-mark))
	  ((> (minibuffer-depth) 0)
	   (abort-recursive-edit))
	  (current-prefix-arg
	   nil)
	  ((> (recursion-depth) 0)
	   (exit-recursive-edit))
	  (buffer-quit-function
	   (funcall buffer-quit-function))
	  ;; ((not (one-window-p t))
	  ;;  (delete-other-windows))
	  ((string-match "^ \\*" (buffer-name (current-buffer)))
	   (bury-buffer))))

  (global-set-key (kbd "s-i") 'ben/to-and-fro-minibuffer)
  (defun ben/to-and-fro-minibuffer ()
    "Go back and forth between minibuffer and other window."
    (interactive)
    (if (window-minibuffer-p (selected-window))
	(select-window (minibuffer-selected-window))
      (select-window (active-minibuffer-window))))

  (evil-define-key 'normal 'global (kbd "n") 'minad/down-from-outside)
  (defun minad/down-from-outside ()
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (execute-kbd-macro [down])))

  (evil-define-key 'normal 'global (kbd "N") 'minad/up-from-outside)
  (defun minad/up-from-outside ()
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (execute-kbd-macro [up])))
  )

(use-package dired-du)

(use-package vterm)

(use-package avy
  :config
  (setq avy-all-windows t)
  (evil-global-set-key 'normal (kbd "u") 'avy-goto-char-timer) ;reserved for avy
  (evil-global-set-key 'motion (kbd "u") 'avy-goto-char-timer)
  (evil-global-set-key 'visual (kbd "u") 'avy-goto-char-timer)
  (evil-global-set-key 'operator (kbd "u") 'avy-goto-char-timer))

(use-package expand-region
  :config
  (global-set-key (kbd "M-e") 'er/expand-region) 
  (global-set-key (kbd "C-M-e") 'er/contract-region)) 

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)            ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)             ;; Enable auto completion
  ;; (corfu-quit-at-boundary t) ;; Automatically quit at word boundary
  ;; (corfu-quit-no-match t)    ;; Automatically quit if there is no match

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  ;; :bind (:map corfu-map
  ;;        ("TAB" . corfu-next)
  ;;        ([tab] . corfu-next)
  ;;        ("S-TAB" . corfu-previous)
  ;;        ([backtab] . corfu-previous))

  ;; You may want to enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (org-mode . corfu-mode)
	 (LaTeX-mode . corfu-mode))
  :config
  (define-key corfu-map (kbd "TAB") 'corfu-insert)
  
)

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
;  :init
;  (corfu-global-mode)) ; This seems to've broken selectrum; it wouln't resize minibuffer

(use-package openwith
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "zathura" (file))
                                ("\\.xlsx\\'" "soffice" (file))
                                ("\\.docx\\'" "soffice" (file))
                                ("\\.pptx\\'" "soffice" (file))
                                ("\\.csv\\'" "soffice" (file))
                                ("\\.ods\\'" "soffice" (file))
				("\\.xopp\\'" "xournalpp" (file)))))
(use-package vertico
  :init
  (vertico-mode)
  (setq enable-recursive-minibuffers t)
  (define-key minibuffer-local-map (kbd "C-<tab>") 'other-window)
  ;; Add vertico extensions to load path and load vertico-repeat:
  (let ((default-directory "/home/ben/.noivy-emacs.d/straight/build/vertico"))
    (normal-top-level-add-subdirs-to-load-path))
  (load "vertico-repeat")
  (global-set-key (kbd "s-r") 'vertico-repeat))

;; selectrum minibuffer was unpredictably disappearing (zero height)
;; (use-package selectrum
;;   :custom
;;   (selectrum-fix-vertical-window-height t)
;;   :init
;;   (selectrum-mode +1)
;;   :config
;;   (global-set-key (kbd "s-r") 'selectrum-repeat)
;;   (setq enable-recursive-minibuffers t)
;;   (define-key minibuffer-local-map (kbd "C-<tab>") 'other-window))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil)
  ;; Enable `partial-completion' for files to allow path expansion.
  ;; You may prefer to use `initials' instead of `partial-completion'.
  (setq completion-category-overrides '((file (styles . (partial-completion)))))
  :config
  ;; performance enhancement; highlighting only the visible candidates.
  ;; (setq orderless-skip-highlighting (lambda () selectrum-is-active))
  ;; (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  ;; (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq orderless-smart-case t)
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t))

;; integrates with vertico/selectrum to make more recent selections first
(use-package savehist
  :init
  (savehist-mode))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)
  (setq marginalia-align-offset 70) ;; moves marginalia much further from the right
)

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ;;("C-c h" . consult-history)
         ;;("C-c m" . consult-mode-command)
         ;;("C-c b" . consult-bookmark)
         ;;("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ;;("C-x M-:" . consult-complex-command) make this s-r?    ;; orig. repeat-complex-command
         ("s-b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-s-b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; Other custom bindings
         ;; ("C-p" . consult-yank-pop)                ;; orig. yank-pop
  ;; Replace bindings. Lazily loaded due by `use-package'.
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))                 ;; required by consult-line to detect isearch

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (evil-define-key 'normal 'global (kbd "/") 'consult-line)

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") 

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)

(use-package affe
  :after orderless
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight
	;; -a is important!
	affe-find-command  "fd -H -a --color=never -p")
  (consult-customize affe-grep affe-find :preview-key (kbd "s-o"))

  (global-set-key (kbd "s-,")  #'(lambda () (interactive)
				   (affe-find "/home/ben/" nil)))
  (global-set-key (kbd "s-.")  #'(lambda () (interactive)
				   (affe-find default-directory nil)))
  (global-set-key (kbd "s-/")  #'(lambda () (interactive)
				   (affe-find "/" nil))))

(use-package embark
  :after evil
  :init
  (evil-define-key '(motion normal insert emacs) 'global
    (kbd "C-d") 'embark-act
    (kbd "C-S-d") 'embark-act-noquit)
  (global-set-key (kbd "C-d") 'embark-act)
  (global-set-key (kbd "C-S-d") 'embark-act-noquit)
  (global-set-key (kbd "C-h B") 'embark-bindings)
  :config
  (define-key minibuffer-local-map (kbd "C-S-s") 'embark-collect-snapshot)
  (define-key minibuffer-local-map (kbd "C-S-b") 'embark-become)
  (define-key minibuffer-local-map (kbd "C-S-e") 'embark-export)
  (define-key embark-file-map (kbd "R") nil)
  (define-key embark-file-map (kbd "b") nil)
  (define-key embark-file-map (kbd "l") nil)
  (define-key embark-file-map (kbd "e") nil)
  (define-key embark-file-map (kbd "R") nil)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; make which-key show action list
  (setq embark-action-indicator
      (lambda (map _target)
        (which-key--show-keymap _target map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)

  (defun embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act)))

  (define-key embark-file-map (kbd "D") 'ben/dired-here)
  (defun ben/dired-here (file)
    "Open dired in this directory"
    (if (f-directory-p file)
	(dired file)
      (dired (file-name-directory file))))

  (define-key embark-file-map (kbd "x") 'ben/xdg-open)
  (defun ben/xdg-open (file)
    "Open FILE with xdg-open. FILE must be absolute path."
    (call-process "xdg-open" nil 0 nil file))

  (define-key embark-file-map (kbd "a") 'ben/xournal)
  (defun ben/xournal (file)
    "Open FILE with xournal. FILE must be absolute path."
    (call-process "xournalpp" nil 0 nil file))

  (define-key embark-file-map (kbd "b") 'ben/db-link)
  (defun ben/db-link (file)
      "Get public dropbox link to FILE, and copy it to clipboard."
      (shell-command (concat "dropbox-cli sharelink \""
			     file "\" | xclip -selection clipboard &> /dev/null")))

  (define-key embark-file-map (kbd "g") 'ben/counsel-rg-here)
  (defun ben/counsel-rg-here (file)
    "consult-ripgrep in this directory."
    (let ((default-directory (file-name-directory file)))
      (consult-ripgrep))))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :diminish
  :config
  (which-key-mode)
  (global-set-key (kbd "C-h K") 'which-key-show-full-keymap))

(use-package eldoc
  :diminish)

(use-package doct
  :commands (doct))

(use-package org
  :config
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  (define-key org-mode-map (kbd "s-j") 'org-next-visible-heading)
  (define-key org-mode-map (kbd "s-k") 'org-previous-visible-heading)
  (define-key org-mode-map (kbd "s-h") 'outline-up-heading)
  (define-key org-mode-map (kbd "s-m") 'consult-org-heading)
  (define-key org-mode-map (kbd "M-j") 'org-metadown)
  (define-key org-mode-map (kbd "M-k") 'org-metaup)
  (define-key org-mode-map (kbd "M-l") 'org-metaright)
  (define-key org-mode-map (kbd "M-h") 'org-metaleft)
  (define-key org-mode-map (kbd "<s-tab>") 'org-cycle)
  (define-key org-mode-map (kbd "<C-tab>") nil)
  (evil-define-key '(normal insert) org-mode-map (kbd "<M-return>") 'sbr-org-insert-dwim)
  (define-key org-mode-map (kbd "S-SPC") 'completion-at-point)
  (setq org-startup-indented t)
  (setq org-startup-truncated nil)
  (setq org-cycle-emulate-tab nil)
  (setq org-M-RET-may-split-line nil)
  (setq org-src-tab-acts-natively t)
  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "/gtd.org"))
  (add-to-list 'org-show-context-detail '(occur-tree . local))
  (define-key org-mode-map (kbd "s-t") 'bensult-roster)
  
  (defun bensult-roster ()
  "Use consult to choose name from roster, create an org sparse
tree for it. Then show each subtree of each headline match.

The value in the alist org-show-context-detail for occur-tree
should be 'ancestors, the default."
    (interactive)
    (org-occur (consult--read roster
			      :prompt "Roster sparse tree: "
			      :category 'consult-location
			      :require-match t))
    (save-excursion
      (goto-char (point-min))
      (while t
	(org-occur-next-match 1)
	(outline-show-subtree))))
  
  (defun sbr-org-insert-dwim (&optional arg)
  "Insert another entry of the same type as the current
entry. For example, if the point is on a list item, then add
another list item of the same type, and if the point is on a
checkbox list item, then add an empty checkbox item. If instead
the point is in a heading, then add another heading. If the point 
is in a TODO heading, then add another TODO heading (set to the 
TODO state). 

By default, the new entry is inserted below the current
subtree/item. With a 'C-u' prefix, insert the entry above the
current heading/item instead. Taken from https://www.reddit.com/r/orgmode/comments/boyu8r/function_for_dwim_insertion_of_new_entries/"
  (interactive "P")
  (when (eq major-mode 'org-mode)
    (let ((org-special-ctrl-a/e t)
	  (below? (unless  (equal arg '(4)) '(4))))
      ;; hack to ensure that the point is not after ellipses because
      ;; that would mess up org-at-item-p etc.
      (org-beginning-of-line)
      (cond ((org-at-item-p) ;; at list item or checkbox
	     (let ((org-M-RET-may-split-line nil)
		   (org-enable-sort-checkbox nil))
	       ;; hack to make item be inserted after the current one
	       ;; doesn't work if we are on an empty item line
	       (when below?
		 (org-end-of-line))                     
	       (org-insert-item (org-at-item-checkbox-p))))
	    ((org-before-first-heading-p) ;; above first heading
	     (org-insert-heading))
	    (t ;; in some kind of heading
	     (org-back-to-heading)
	     (if (org-get-todo-state)
		 ;; at TODO heading
		 (org-insert-todo-heading t below?)
	       ;; at non-TODO heading 
	       (org-insert-heading below?)))))))

  (defun sbr-org-shift-return (&optional arg)
    "If point is at a table, copy the table cell downward (i.e.,
the usual effect of typing S-RET). Otherwise,  insert the same
kind of heading or item as the current entry containing the
point. "
    (interactive "P")
    (if (org-at-table-p)
	(org-table-copy-down (prefix-numeric-value arg))
      (sbr-org-insert-dwim arg)))
  
  (setq org-agenda-skip-scheduled-if-done t
        org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "SOMEDAY(s)" "CANCELLED(c)"))
        org-agenda-files '("~/org/inbox.org" "~/org/gtd.org"))
  ;(setq org-capture-templates
  ;      (doct `(("Binding" :keys "b"
  ;               :type entry
  ;               :file "~/org/bindings.org"
  ;               :function ,(defun +org-capture-heading-from-major-mode ()
  ;                            (let* ((buffer (org-capture-get :original-buffer))
  ;                                   (mm (with-current-buffer buffer (symbol-name major-mode))))
  ;                              (if-let ((marker (org-find-exact-headline-in-buffer mm)))
  ;                                  (goto-char marker)
  ;                                (goto-char (point-max))
  ;                                (insert "* " mm))))
  ;               :template "* %?")
  ;              ("Todo" :keys "i"
  ;               :type entry
  ;               :file "~/org/inbox.org"
  ;               :headline "Tasks"
  ;               :template "* %i%?")
                ;("Tickler" :keys "t"
                ; :type entry
                ; :file "~/org/tickler.org"
                ; :headline "Tickler"
                ; :template "* %i%? \n %U"))))
  (setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 4)))
;                           ("~/org/someday.org" :level . 1)
 ;                          ("~/org/tickler.org" :maxlevel . 2)))
  (setq process-connection-type t)

  ;; Reads the roster; headings are students name under heading Roster
  (add-hook 'org-mode-hook
	    (lambda ()
	      ;; TODO: change stats.org to class.org or something
	      (when (string-suffix-p "stats.org" (buffer-file-name))
		(set (make-local-variable 'roster)  
		     (org-element-map (org-element-parse-buffer) 'headline
		       (lambda (hl)
			 (when (string= "Roster"
				      (org-element-property
				       :raw-value (org-element-property :parent hl)))
			   (org-element-property :raw-value hl)))))
		(add-to-list 'completion-at-point-functions #'ben-roster-completion-at-point))))
  
  
  ;; (defun ben-roster-completion-at-point ()
  ;;   (interactive)
  ;;   (let ((part (thing-at-point 'word))
  ;; 	  (start (save-excursion
  ;; 		   (skip-syntax-backward "^ ")
  ;; 		   (point))))
  ;;     (list start (point) 
  ;; 	       (remove nil
  ;; 			 (mapcar (lambda (name)
  ;; 				   (when (string-prefix-p part name t)
  ;; 				     name))
  ;; 				 roster))
  ;; 	       :exclusive 'no))))
  (defun ben-roster-completion-at-point ()
    (interactive)
    (let ((part (thing-at-point 'word))
	  (start (save-excursion
		   (skip-syntax-backward "^ ")
		   (point))))
      (list start (point) roster :exclusive 'no)))
  )

(use-package ess-r-mode
  :mode ("\\.r\\'" . ess-r-mode))

(use-package latex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (yas-minor-mode 1)
			       (flyspell-mode)
			       (TeX-source-correlate-mode)
			       (setq-default TeX-engine 'xetex
					     TeX-PDF-mode t)
			       (setq TeX-source-correlate-start-server t)
			       (add-to-list 'TeX-view-program-selection
                                            '(output-pdf "Zathura"))
			       (setq TeX-electric-sub-and-superscript nil)
			       (setq +latex-indent-level-item-continuation 2))))

(use-package evil-tex
  :config
  (add-hook `LaTeX-mode-hook #'evil-tex-mode))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.noivy-emacs.d/snippets"))
  (yas-reload-all)
  (setq yas-triggers-in-field t))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-command "pandoc -t html5"))

;; TODO learn how to configure lsp-mode properly!
(use-package lsp-mode
  :commands (lsp lsp-deffered)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :config
  (add-hook `python-mode-hook #'lsp))

(use-package lsp-pyright
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))


