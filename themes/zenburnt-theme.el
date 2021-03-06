;;; zenburnt-theme.el --- A low contrast color theme for Emacs.

;; Zenburn Copyright (C) 2011-2018 Bozhidar Batsov
;; Modified to zenburnt by Ben Smith 2021

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/zenburn-emacs
;; Version: 2.7-snapshot

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of the popular Vim theme Zenburn for Emacs 24+, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on which this port
;; is based.

;;; Code:

(deftheme zenburnt "The Zenburnt color theme")

(defgroup zenburnt-theme nil
  "Zenburnt theme."
  :group 'faces
  :prefix "zenburnt-theme-"
  :link '(url-link :tag "GitHub" "http://github.com/bbatsov/zenburn-emacs")
  :tag "Zenburnt theme")

;;;###autoload
(defcustom zenburnt-override-colors-alist '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'zenburnt-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defcustom zenburnt-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'zenburnt-theme
  :package-version '(zenburnt . "2.6"))

(defcustom zenburnt-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'zenburnt-theme
  :package-version '(zenburnt . "2.6"))

(defcustom zenburnt-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'zenburnt-theme
  :package-version '(zenburnt . "2.6"))

(defcustom zenburnt-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'zenburnt-theme
  :package-version '(zenburnt . "2.6"))

(defcustom zenburnt-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'zenburnt-theme
  :package-version '(zenburnt . "2.6"))

(defcustom zenburnt-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'zenburnt-theme
  :package-version '(zenburnt . "2.6"))

(defcustom zenburnt-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled."
  :type 'boolean
  :group 'zenburnt-theme
  :package-version '(zenburnt . "2.6"))

(defcustom zenburnt-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled."
  :type 'boolean
  :group 'zenburnt-theme
  :package-version '(zenburnt . "2.6"))

;;; Color Palette

(defvar zenburnt-default-colors-alist
  '(("zenburnt-fg-1"     . "#656555")
    ("zenburnt-fg-05"    . "#989890")
    ("zenburnt-fg"       . "#DCDCCC")
    ("zenburnt-fg+1"     . "#FFFFEF")
    ("zenburnt-fg+2"     . "#FFFFFD")
    ("zenburnt-bg-2"     . "#000000")
    ("zenburnt-bg-1"     . "#292929")
    ("zenburnt-bg-08"    . "#303030")
    ("zenburnt-bg-05"    . "#383838")
    ("zenburnt-bg"       . "#3F3F3F")
    ("zenburnt-bg+05"    . "#494949")
    ("zenburnt-bg+1"     . "#4F4F4F")
    ("zenburnt-bg+2"     . "#5F5F5F")
    ("zenburnt-bg+3"     . "#6F6F6F")
    ("zenburnt-red-6"    . "#6C3333")
    ("zenburnt-red-5"    . "#7C4343")
    ("zenburnt-red-4"    . "#8C5353")
    ("zenburnt-red-3"    . "#9C6363")
    ("zenburnt-red-2"    . "#AC7373")
    ("zenburnt-red-1"    . "#BC8383")
    ("zenburnt-red"      . "#CC9393")
    ("zenburnt-red+1"    . "#DCA3A3")
    ("zenburnt-red+2"    . "#ECB3B3")
    ("zenburnt-orange"   . "#DFAF8F")
    ("zenburnt-yellow-2" . "#D0BF8F")
    ("zenburnt-yellow-1" . "#E0CF9F")
    ("zenburnt-yellow"   . "#F0DFAF")
    ("zenburnt-green-5"  . "#2F4F2F")
    ("zenburnt-green-4"  . "#3F5F3F")
    ("zenburnt-green-3"  . "#4F6F4F")
    ("zenburnt-green-2"  . "#5F7F5F")
    ("zenburnt-green-1"  . "#6F8F6F")
    ("zenburnt-green"    . "#7F9F7F")
    ("zenburnt-green+1"  . "#8FB28F")
    ("zenburnt-green+2"  . "#9FC59F")
    ("zenburnt-green+3"  . "#AFD8AF")
    ("zenburnt-green+4"  . "#BFEBBF")
    ("zenburnt-cyan"     . "#93E0E3")
    ("zenburnt-blue+3"   . "#BDE0F3")
    ("zenburnt-blue+2"   . "#ACE0E3")
    ("zenburnt-blue+1"   . "#94BFF3")
    ("zenburnt-blue"     . "#8CD0D3")
    ("zenburnt-blue-1"   . "#7CB8BB")
    ("zenburnt-blue-2"   . "#6CA0A3")
    ("zenburnt-blue-3"   . "#5C888B")
    ("zenburnt-blue-4"   . "#4C7073")
    ("zenburnt-blue-5"   . "#366060")
    ("zenburnt-magenta"  . "#DC8CC3"))
  "List of Zenburnt colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro zenburnt-with-color-variables (&rest body)
  "`let' bind all colors defined in `zenburnt-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append zenburnt-default-colors-alist
                           zenburnt-override-colors-alist))
         (z-variable-pitch (if zenburnt-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))

;;; Theme Faces
(zenburnt-with-color-variables
  (custom-theme-set-faces
   'zenburnt
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,zenburnt-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,zenburnt-yellow-2 :underline t :weight normal))))
   ;original `(default ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg))))
   `(default ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg-1))))
   `(cursor ((t (:foreground ,zenburnt-fg :background ,zenburnt-fg+1))))
   `(widget-field ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg+3))))
   `(escape-glyph ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(fringe ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg+1))))
   `(header-line ((t (:foreground ,zenburnt-yellow
                                  :background ,zenburnt-bg-1
                                  :box (:line-width -1 :style released-button)
                                  :extend t))))
                                        ;`(highlight ((t (:background ,zenburnt-bg-05))))
   `(highlight ((t (:background ,zenburnt-bg-2))))
   `(success ((t (:foreground ,zenburnt-green :weight bold))))
   `(warning ((t (:foreground ,zenburnt-orange :weight bold))))
   `(tooltip ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,zenburnt-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,zenburnt-green))))
   `(compilation-error-face ((t (:foreground ,zenburnt-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,zenburnt-fg))))
   `(compilation-info-face ((t (:foreground ,zenburnt-blue))))
   `(compilation-info ((t (:foreground ,zenburnt-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,zenburnt-green))))
   `(compilation-line-face ((t (:foreground ,zenburnt-yellow))))
   `(compilation-line-number ((t (:foreground ,zenburnt-yellow))))
   `(compilation-message-face ((t (:foreground ,zenburnt-blue))))
   `(compilation-warning-face ((t (:foreground ,zenburnt-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,zenburnt-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,zenburnt-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,zenburnt-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,zenburnt-fg-1))))
;;;;; customize
   `(custom-variable-tag ((t (:foreground ,zenburnt-blue :weight bold))))
   `(custom-group-tag ((t (:foreground ,zenburnt-blue :weight bold :height 1.2))))
   `(custom-state ((t (:foreground ,zenburnt-green+4))))
;;;;; display-fill-column-indicator
     `(fill-column-indicator ((,class :foreground ,zenburnt-bg-05 :weight semilight)))
;;;;; eww
   '(eww-invalid-certificate ((t (:inherit error))))
   '(eww-valid-certificate   ((t (:inherit success))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,zenburnt-fg))))
   `(grep-error-face ((t (:foreground ,zenburnt-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,zenburnt-blue))))
   `(grep-match-face ((t (:foreground ,zenburnt-orange :weight bold))))
   `(match ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-orange :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,zenburnt-cyan    :foreground ,zenburnt-bg-1))))
   `(hi-green   ((t (:background ,zenburnt-green+4 :foreground ,zenburnt-bg-1))))
   `(hi-pink    ((t (:background ,zenburnt-magenta :foreground ,zenburnt-bg-1))))
   `(hi-yellow  ((t (:background ,zenburnt-yellow  :foreground ,zenburnt-bg-1))))
   `(hi-blue-b  ((t (:foreground ,zenburnt-blue    :weight     bold))))
   `(hi-green-b ((t (:foreground ,zenburnt-green+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,zenburnt-red     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,zenburnt-yellow-2 :weight bold :background ,zenburnt-bg+2))))
   `(isearch-fail ((t (:foreground ,zenburnt-fg :background ,zenburnt-red-4))))
   `(lazy-highlight ((t (:foreground ,zenburnt-yellow-2 :weight bold :background ,zenburnt-bg-05))))

   `(menu ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg))))
   `(minibuffer-prompt ((t (:foreground ,zenburnt-yellow))))
   `(mode-line
     ((,class (:foreground ,zenburnt-green+1
               ;discarded change :foreground ,zenburnt-blue+1
                           ;:background ,zenburnt-green-5
			   :background ,zenburnt-bg
                           ;discarded change :background ,zenburnt-green-5
					;origional :box (:line-width -1 :style released-button)))
			   :box (:line-width 1 :color ,zenburnt-fg)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,zenburnt-green-2
          ;discarded change :foreground ,zenburnt-blue
                      ;original :background ,zenburnt-bg-05
		      :background ,zenburnt-bg+05))))
                      ;discarded change :background ,zenburnt-bg+1
                      ;original :box (:line-width -1 :style released-button)))))
   ;original `(region ((,class (:background ,zenburnt-bg-1 :extend t))
       ;      (t :inverse-video t)))
    `(region ((,class (:background ,zenburnt-bg+1 :extend t))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,zenburnt-bg+2))))
   `(trailing-whitespace ((t (:background ,zenburnt-red))))
   ;original`(vertical-border ((t (:foreground ,zenburnt-fg))))
   `(vertical-border ((t (:foreground ,zenburnt-bg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,zenburnt-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,zenburnt-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,zenburnt-green-2))))
   `(font-lock-constant-face ((t (:foreground ,zenburnt-green+4))))
   `(font-lock-doc-face ((t (:foreground ,zenburnt-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,zenburnt-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,zenburnt-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,zenburnt-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,zenburnt-red))))
   `(font-lock-type-face ((t (:foreground ,zenburnt-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,zenburnt-orange))))
   `(font-lock-warning-face ((t (:foreground ,zenburnt-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((t (:foreground ,zenburnt-bg+3 :background ,zenburnt-bg-05))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,zenburnt-yellow-2))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,zenburnt-fg))))
   `(newsticker-default-face ((t (:foreground ,zenburnt-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,zenburnt-green+3))))
   `(newsticker-extra-face ((t (:foreground ,zenburnt-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,zenburnt-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,zenburnt-green))))
   `(newsticker-new-item-face ((t (:foreground ,zenburnt-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,zenburnt-red))))
   `(newsticker-old-item-face ((t (:foreground ,zenburnt-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,zenburnt-fg))))
   `(newsticker-treeview-face ((t (:foreground ,zenburnt-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,zenburnt-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,zenburnt-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,zenburnt-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,zenburnt-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,zenburnt-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-yellow))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,zenburnt-fg-1 :background ,zenburnt-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,zenburnt-green+2 :background ,zenburnt-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,zenburnt-fg-1 :background ,zenburnt-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,zenburnt-green+1))))
   `(android-mode-error-face ((t (:foreground ,zenburnt-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,zenburnt-fg))))
   `(android-mode-verbose-face ((t (:foreground ,zenburnt-green))))
   `(android-mode-warning-face ((t (:foreground ,zenburnt-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,zenburnt-cyan :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,zenburnt-red :weight bold))))
   `(anzu-match-1 ((t (:foreground ,zenburnt-bg :background ,zenburnt-green))))
   `(anzu-match-2 ((t (:foreground ,zenburnt-bg :background ,zenburnt-orange))))
   `(anzu-match-3 ((t (:foreground ,zenburnt-bg :background ,zenburnt-blue))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,zenburnt-yellow))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,zenburnt-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,zenburnt-yellow))))
   `(font-latex-italic-face ((t (:foreground ,zenburnt-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,zenburnt-orange))))
   `(font-latex-script-char-face ((t (:foreground ,zenburnt-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,zenburnt-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,zenburnt-orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,zenburnt-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,zenburnt-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,zenburnt-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,zenburnt-blue))))
   `(agda2-highlight-function-face ((t (:foreground ,zenburnt-blue))))
   `(agda2-highlight-module-face ((t (:foreground ,zenburnt-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,zenburnt-bg :background ,zenburnt-magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,zenburnt-bg :background ,zenburnt-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,zenburnt-bg :background ,zenburnt-magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,zenburnt-bg :background ,zenburnt-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,zenburnt-bg :background ,zenburnt-magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,zenburnt-red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,zenburnt-bg+3 :foreground ,zenburnt-bg-2))))
   `(ac-selection-face ((t (:background ,zenburnt-blue-4 :foreground ,zenburnt-fg))))
   `(popup-tip-face ((t (:background ,zenburnt-yellow-2 :foreground ,zenburnt-bg-2))))
   `(popup-menu-mouse-face ((t (:background ,zenburnt-yellow-2 :foreground ,zenburnt-bg-2))))
   `(popup-summary-face ((t (:background ,zenburnt-bg+3 :foreground ,zenburnt-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,zenburnt-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,zenburnt-bg-1))))
   `(popup-isearch-match ((t (:background ,zenburnt-bg :foreground ,zenburnt-fg))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,zenburnt-fg-1 :background ,zenburnt-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,zenburnt-green+3 :background ,zenburnt-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,zenburnt-yellow :background ,zenburnt-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,zenburnt-red+1 :background ,zenburnt-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,zenburnt-cyan :background ,zenburnt-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,zenburnt-orange :background ,zenburnt-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,zenburnt-orange :background ,zenburnt-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg-1))))
   `(company-tooltip-mouse ((t (:background ,zenburnt-bg-1))))
   `(company-tooltip-common ((t (:foreground ,zenburnt-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,zenburnt-green+2))))
   `(company-scrollbar-fg ((t (:background ,zenburnt-bg-1))))
   `(company-scrollbar-bg ((t (:background ,zenburnt-bg+2))))
   `(company-preview ((t (:background ,zenburnt-green+2))))
   `(company-preview-common ((t (:foreground ,zenburnt-green+2 :background ,zenburnt-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,zenburnt-yellow-1 :foreground ,zenburnt-bg))))
   `(bm-fringe-face ((t (:background ,zenburnt-yellow-1 :foreground ,zenburnt-bg))))
   `(bm-fringe-persistent-face ((t (:background ,zenburnt-green-2 :foreground ,zenburnt-bg))))
   `(bm-persistent-face ((t (:background ,zenburnt-green-2 :foreground ,zenburnt-bg))))
;;;;; calfw
   `(cfw:face-annotation ((t (:foreground ,zenburnt-red :inherit cfw:face-day-title))))
   `(cfw:face-day-title ((t nil)))
   `(cfw:face-default-content ((t (:foreground ,zenburnt-green))))
   `(cfw:face-default-day ((t (:weight bold))))
   `(cfw:face-disable ((t (:foreground ,zenburnt-fg-1))))
   `(cfw:face-grid ((t (:inherit shadow))))
   `(cfw:face-header ((t (:inherit font-lock-keyword-face))))
   `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))
   `(cfw:face-periods ((t (:foreground ,zenburnt-cyan))))
   `(cfw:face-saturday ((t (:foreground ,zenburnt-blue :weight bold))))
   `(cfw:face-select ((t (:background ,zenburnt-blue-5))))
   `(cfw:face-sunday ((t (:foreground ,zenburnt-red :weight bold))))
   `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
   `(cfw:face-today ((t (:foreground ,zenburnt-cyan :weight bold))))
   `(cfw:face-today-title ((t (:inherit highlight bold))))
   `(cfw:face-toolbar ((t (:background ,zenburnt-blue-5))))
   `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))
   `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))))
;;;;; centaur-tabs
   `(centaur-tabs-default ((t (:background ,zenburnt-bg :foreground ,zenburnt-fg :box nil))))
   `(centaur-tabs-selected ((t (:background ,zenburnt-bg :foreground ,zenburnt-fg+2 :box nil))))
   `(centaur-tabs-unselected ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-fg-05 :box nil))))
   `(centaur-tabs-selected-modified ((t (:background ,zenburnt-bg :foreground ,zenburnt-orange :box nil))))
   `(centaur-tabs-unselected-modified ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-orange :box nil))))
   `(centaur-tabs-active-bar-face ((t (:background ,zenburnt-yellow :box nil))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground ,zenburnt-yellow :box nil))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground ,zenburnt-yellow :box nil))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,zenburnt-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,zenburnt-green+1))))
   `(cider-deprecated-face ((t (:background ,zenburnt-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,zenburnt-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,zenburnt-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,zenburnt-red-4))))
   `(cider-test-error-face ((t (:background ,zenburnt-magenta))))
   `(cider-test-success-face ((t (:background ,zenburnt-green-2))))
   `(cider-fringe-good-face ((t (:foreground ,zenburnt-green+4))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,zenburnt-cyan))))
   `(circe-my-message-face ((t (:foreground ,zenburnt-fg))))
   `(circe-fool-face ((t (:foreground ,zenburnt-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,zenburnt-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,zenburnt-fg))))
   `(circe-server-face ((t (:foreground ,zenburnt-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,zenburnt-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,zenburnt-orange :background ,zenburnt-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,zenburnt-fg)))
   `(context-coloring-level-1-face ((t :foreground ,zenburnt-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,zenburnt-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,zenburnt-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,zenburnt-orange)))
   `(context-coloring-level-5-face ((t :foreground ,zenburnt-magenta)))
   `(context-coloring-level-6-face ((t :foreground ,zenburnt-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,zenburnt-green+2)))
   `(context-coloring-level-8-face ((t :foreground ,zenburnt-yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,zenburnt-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,zenburnt-blue :foreground ,zenburnt-bg))))
   `(ctbl:face-continue-bar ((t (:background ,zenburnt-bg-05 :foreground ,zenburnt-bg))))
   `(ctbl:face-row-select ((t (:background ,zenburnt-cyan :foreground ,zenburnt-bg))))
;;;;; debbugs
   `(debbugs-gnu-done ((t (:foreground ,zenburnt-fg-1))))
   `(debbugs-gnu-handled ((t (:foreground ,zenburnt-green))))
   `(debbugs-gnu-new ((t (:foreground ,zenburnt-red))))
   `(debbugs-gnu-pending ((t (:foreground ,zenburnt-blue))))
   `(debbugs-gnu-stale ((t (:foreground ,zenburnt-orange))))
   `(debbugs-gnu-tagged ((t (:foreground ,zenburnt-red))))
;;;;; diff
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(diff-added          ((t (:background "#335533" :foreground ,zenburnt-green))))
   `(diff-changed        ((t (:background "#555511" :foreground ,zenburnt-yellow-1))))
   `(diff-removed        ((t (:background "#553333" :foreground ,zenburnt-red-2))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,zenburnt-green+4))))
   `(diff-refine-changed ((t (:background "#888811" :foreground ,zenburnt-yellow))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,zenburnt-red))))
   `(diff-header ((,class (:background ,zenburnt-bg+2))
                  (t (:background ,zenburnt-fg :foreground ,zenburnt-bg))))
   `(diff-file-header
     ((,class (:background ,zenburnt-bg+2 :foreground ,zenburnt-fg :weight bold))
      (t (:background ,zenburnt-fg :foreground ,zenburnt-bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,zenburnt-blue :background ,zenburnt-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,zenburnt-red+1 :background ,zenburnt-red-1))))
   `(diff-hl-insert ((,class (:foreground ,zenburnt-green+1 :background ,zenburnt-green-2))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,zenburnt-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,zenburnt-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,zenburnt-orange))))
   `(diredp-date-time ((t (:foreground ,zenburnt-magenta))))
   `(diredp-deletion ((t (:foreground ,zenburnt-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,zenburnt-red))))
   `(diredp-dir-heading ((t (:foreground ,zenburnt-blue :background ,zenburnt-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,zenburnt-cyan))))
   `(diredp-exec-priv ((t (:foreground ,zenburnt-red))))
   `(diredp-executable-tag ((t (:foreground ,zenburnt-green+1))))
   `(diredp-file-name ((t (:foreground ,zenburnt-blue))))
   `(diredp-file-suffix ((t (:foreground ,zenburnt-green))))
   `(diredp-flag-mark ((t (:foreground ,zenburnt-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,zenburnt-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,zenburnt-red))))
   `(diredp-link-priv ((t (:foreground ,zenburnt-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,zenburnt-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,zenburnt-orange))))
   `(diredp-no-priv ((t (:foreground ,zenburnt-fg))))
   `(diredp-number ((t (:foreground ,zenburnt-green+1))))
   `(diredp-other-priv ((t (:foreground ,zenburnt-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,zenburnt-red-1))))
   `(diredp-read-priv ((t (:foreground ,zenburnt-green-2))))
   `(diredp-symlink ((t (:foreground ,zenburnt-yellow))))
   `(diredp-write-priv ((t (:foreground ,zenburnt-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,zenburnt-red :weight bold))))
   `(dired-async-message ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,zenburnt-yellow))))
;;;;; diredfl
   `(diredfl-compressed-file-suffix ((t (:foreground ,zenburnt-orange))))
   `(diredfl-date-time ((t (:foreground ,zenburnt-magenta))))
   `(diredfl-deletion ((t (:foreground ,zenburnt-yellow))))
   `(diredfl-deletion-file-name ((t (:foreground ,zenburnt-red))))
   `(diredfl-dir-heading ((t (:foreground ,zenburnt-blue :background ,zenburnt-bg-1))))
   `(diredfl-dir-priv ((t (:foreground ,zenburnt-cyan))))
   `(diredfl-exec-priv ((t (:foreground ,zenburnt-red))))
   `(diredfl-executable-tag ((t (:foreground ,zenburnt-green+1))))
   `(diredfl-file-name ((t (:foreground ,zenburnt-blue))))
   `(diredfl-file-suffix ((t (:foreground ,zenburnt-green))))
   `(diredfl-flag-mark ((t (:foreground ,zenburnt-yellow))))
   `(diredfl-flag-mark-line ((t (:foreground ,zenburnt-orange))))
   `(diredfl-ignored-file-name ((t (:foreground ,zenburnt-red))))
   `(diredfl-link-priv ((t (:foreground ,zenburnt-yellow))))
   `(diredfl-no-priv ((t (:foreground ,zenburnt-fg))))
   `(diredfl-number ((t (:foreground ,zenburnt-green+1))))
   `(diredfl-other-priv ((t (:foreground ,zenburnt-yellow-1))))
   `(diredfl-rare-priv ((t (:foreground ,zenburnt-red-1))))
   `(diredfl-read-priv ((t (:foreground ,zenburnt-green-1))))
   `(diredfl-symlink ((t (:foreground ,zenburnt-yellow))))
   `(diredfl-write-priv ((t (:foreground ,zenburnt-magenta))))
;;;;; doom-modeline
   `(doom-modeline-bar  ((t (:background ,zenburnt-yellow))))
   `(doom-modeline-inactive-bar  ((t (:background nil))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,zenburnt-fg :background ,zenburnt-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,zenburnt-fg :background ,zenburnt-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,zenburnt-fg :background ,zenburnt-green-2))))
   `(ediff-current-diff-C ((t (:foreground ,zenburnt-fg :background ,zenburnt-blue-5))))
   `(ediff-even-diff-A ((t (:background ,zenburnt-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,zenburnt-bg+1))))
   `(ediff-even-diff-B ((t (:background ,zenburnt-bg+1))))
   `(ediff-even-diff-C ((t (:background ,zenburnt-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,zenburnt-fg :background ,zenburnt-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,zenburnt-fg :background ,zenburnt-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,zenburnt-fg :background ,zenburnt-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,zenburnt-fg :background ,zenburnt-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,zenburnt-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,zenburnt-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,zenburnt-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,zenburnt-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,zenburnt-fg))))
   `(egg-help-header-1 ((t (:foreground ,zenburnt-yellow))))
   `(egg-help-header-2 ((t (:foreground ,zenburnt-green+3))))
   `(egg-branch ((t (:foreground ,zenburnt-yellow))))
   `(egg-branch-mono ((t (:foreground ,zenburnt-yellow))))
   `(egg-term ((t (:foreground ,zenburnt-yellow))))
   `(egg-diff-add ((t (:foreground ,zenburnt-green+4))))
   `(egg-diff-del ((t (:foreground ,zenburnt-red+1))))
   `(egg-diff-file-header ((t (:foreground ,zenburnt-yellow-2))))
   `(egg-section-title ((t (:foreground ,zenburnt-yellow))))
   `(egg-stash-mono ((t (:foreground ,zenburnt-green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,zenburnt-red))))
   `(elfeed-log-info-level-face ((t (:foreground ,zenburnt-blue))))
   `(elfeed-log-warn-level-face ((t (:foreground ,zenburnt-yellow))))
   `(elfeed-search-date-face ((t (:foreground ,zenburnt-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,zenburnt-green))))
   `(elfeed-search-feed-face ((t (:foreground ,zenburnt-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,zenburnt-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,zenburnt-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,zenburnt-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,zenburnt-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,zenburnt-green+2 :background ,zenburnt-bg))))
   `(w3m-lnum-match ((t (:background ,zenburnt-bg-1
                                     :foreground ,zenburnt-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,zenburnt-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,zenburnt-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,zenburnt-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,zenburnt-yellow))))
   `(erc-keyword-face ((t (:foreground ,zenburnt-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,zenburnt-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,zenburnt-green))))
   `(erc-pal-face ((t (:foreground ,zenburnt-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,zenburnt-orange :background ,zenburnt-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,zenburnt-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; eros
   `(eros-result-overlay-face ((t (:background unspecified))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,zenburnt-green+4 :background ,zenburnt-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,zenburnt-red :background ,zenburnt-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,zenburnt-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,zenburnt-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,zenburnt-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,zenburnt-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,zenburnt-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,zenburnt-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburnt-red-1) :inherit unspecified))
      (t (:foreground ,zenburnt-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburnt-yellow) :inherit unspecified))
      (t (:foreground ,zenburnt-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburnt-cyan) :inherit unspecified))
      (t (:foreground ,zenburnt-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,zenburnt-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,zenburnt-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburnt-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburnt-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburnt-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburnt-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburnt-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburnt-green-2 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburnt-orange) :inherit unspecified))
      (t (:foreground ,zenburnt-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburnt-red) :inherit unspecified))
      (t (:foreground ,zenburnt-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,zenburnt-fg))))
   `(ack-file ((t (:foreground ,zenburnt-blue))))
   `(ack-line ((t (:foreground ,zenburnt-yellow))))
   `(ack-match ((t (:foreground ,zenburnt-orange :background ,zenburnt-bg-1 :weight bold))))
;;;;; git-annex
   '(git-annex-dired-annexed-available ((t (:inherit success :weight normal))))
   '(git-annex-dired-annexed-unavailable ((t (:inherit error :weight normal))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,zenburnt-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,zenburnt-blue+1  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,zenburnt-blue+1  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,zenburnt-green  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,zenburnt-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,zenburnt-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,zenburnt-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,zenburnt-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,zenburnt-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,zenburnt-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,zenburnt-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,zenburnt-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, zenburnt-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,zenburnt-green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,zenburnt-red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,zenburnt-blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,zenburnt-blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,zenburnt-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,zenburnt-blue))))
   `(gnus-summary-high-read ((t (:foreground ,zenburnt-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,zenburnt-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,zenburnt-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,zenburnt-blue))))
   `(gnus-summary-low-read ((t (:foreground ,zenburnt-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,zenburnt-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,zenburnt-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,zenburnt-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,zenburnt-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,zenburnt-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,zenburnt-fg))))
   `(gnus-summary-selected ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,zenburnt-blue))))
   `(gnus-cite-10 ((t (:foreground ,zenburnt-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,zenburnt-yellow))))
   `(gnus-cite-2 ((t (:foreground ,zenburnt-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,zenburnt-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,zenburnt-green+2))))
   `(gnus-cite-5 ((t (:foreground ,zenburnt-green+1))))
   `(gnus-cite-6 ((t (:foreground ,zenburnt-green))))
   `(gnus-cite-7 ((t (:foreground ,zenburnt-red))))
   `(gnus-cite-8 ((t (:foreground ,zenburnt-red-1))))
   `(gnus-cite-9 ((t (:foreground ,zenburnt-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,zenburnt-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,zenburnt-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,zenburnt-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,zenburnt-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,zenburnt-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,zenburnt-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,zenburnt-bg+2))))
   `(gnus-signature ((t (:foreground ,zenburnt-yellow))))
   `(gnus-x ((t (:background ,zenburnt-fg :foreground ,zenburnt-bg))))
   `(mm-uu-extract ((t (:background ,zenburnt-bg-05 :foreground ,zenburnt-green+1))))
;;;;; go-guru
   `(go-guru-hl-identifier-face ((t (:foreground ,zenburnt-bg-1 :background ,zenburnt-green+1))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,zenburnt-blue))))
   `(guide-key/key-face ((t (:foreground ,zenburnt-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,zenburnt-green+1))))
;;;;; hackernews
   '(hackernews-comment-count ((t (:inherit link-visited :underline nil))))
   '(hackernews-link          ((t (:inherit link         :underline nil))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,zenburnt-green
                      :background ,zenburnt-bg
                      :underline nil
                      :box nil
                      :extend t))))
   `(helm-source-header
     ((t (:foreground ,zenburnt-yellow
                      :background ,zenburnt-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)
                      :extend t))))
   `(helm-selection ((t (:background ,zenburnt-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,zenburnt-bg+1))))
   `(helm-visible-mark ((t (:foreground ,zenburnt-bg :background ,zenburnt-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,zenburnt-green+4 :background ,zenburnt-bg-1))))
   `(helm-separator ((t (:foreground ,zenburnt-red :background ,zenburnt-bg))))
   `(helm-time-zone-current ((t (:foreground ,zenburnt-green+2 :background ,zenburnt-bg))))
   `(helm-time-zone-home ((t (:foreground ,zenburnt-red :background ,zenburnt-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,zenburnt-orange :background ,zenburnt-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,zenburnt-magenta :background ,zenburnt-bg))))
   `(helm-bookmark-info ((t (:foreground ,zenburnt-green+2 :background ,zenburnt-bg))))
   `(helm-bookmark-man ((t (:foreground ,zenburnt-yellow :background ,zenburnt-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,zenburnt-magenta :background ,zenburnt-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,zenburnt-red :background ,zenburnt-bg))))
   `(helm-buffer-process ((t (:foreground ,zenburnt-cyan :background ,zenburnt-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg))))
   `(helm-buffer-size ((t (:foreground ,zenburnt-fg-1 :background ,zenburnt-bg))))
   `(helm-ff-directory ((t (:foreground ,zenburnt-cyan :background ,zenburnt-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,zenburnt-green+2 :background ,zenburnt-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,zenburnt-red :background ,zenburnt-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,zenburnt-yellow :background ,zenburnt-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,zenburnt-bg :background ,zenburnt-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,zenburnt-cyan :background ,zenburnt-bg))))
   `(helm-grep-file ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg))))
   `(helm-grep-finish ((t (:foreground ,zenburnt-green+2 :background ,zenburnt-bg))))
   `(helm-grep-lineno ((t (:foreground ,zenburnt-fg-1 :background ,zenburnt-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,zenburnt-red :background ,zenburnt-bg))))
   `(helm-match ((t (:foreground ,zenburnt-orange :background ,zenburnt-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,zenburnt-cyan :background ,zenburnt-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,zenburnt-fg-1 :background ,zenburnt-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg))))
;;;;; helm-lxc
   `(helm-lxc-face-frozen ((t (:foreground ,zenburnt-blue :background ,zenburnt-bg))))
   `(helm-lxc-face-running ((t (:foreground ,zenburnt-green :background ,zenburnt-bg))))
   `(helm-lxc-face-stopped ((t (:foreground ,zenburnt-red :background ,zenburnt-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,zenburnt-yellow :background ,zenburnt-bg+2 :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,zenburnt-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,zenburnt-bg-05 :extend t)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,zenburnt-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,zenburnt-red-1 :background ,zenburnt-bg))))
   `(hydra-face-amaranth ((t (:foreground ,zenburnt-red-3 :background ,zenburnt-bg))))
   `(hydra-face-blue ((t (:foreground ,zenburnt-blue :background ,zenburnt-bg))))
   `(hydra-face-pink ((t (:foreground ,zenburnt-magenta :background ,zenburnt-bg))))
   `(hydra-face-teal ((t (:foreground ,zenburnt-cyan :background ,zenburnt-bg))))
;;;;; info+
   `(info-command-ref-item ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-orange))))
   `(info-constant-ref-item ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-magenta))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-yellow))))
   `(info-function-ref-item ((t (:background ,zenburnt-bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-yellow))))
   `(info-menu ((t (:foreground ,zenburnt-yellow))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,zenburnt-bg-1))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-yellow))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-blue+1))))
   `(info-user-option-ref-item ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-red))))
   `(info-variable-ref-item ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-orange))))
;;;;; irfc
   `(irfc-head-name-face ((t (:foreground ,zenburnt-red :weight bold))))
   `(irfc-head-number-face ((t (:foreground ,zenburnt-red :weight bold))))
   `(irfc-reference-face ((t (:foreground ,zenburnt-blue-1 :weight bold))))
   `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(irfc-rfc-link-face ((t (:inherit link))))
   `(irfc-rfc-number-face ((t (:foreground ,zenburnt-cyan :weight bold))))
   `(irfc-std-number-face ((t (:foreground ,zenburnt-green+4 :weight bold))))
   `(irfc-table-item-face ((t (:foreground ,zenburnt-green+3))))
   `(irfc-title-face ((t (:foreground ,zenburnt-yellow
                                      :underline t :weight bold))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,zenburnt-green :background ,zenburnt-bg))))
   `(ivy-current-match ((t (:foreground ,zenburnt-yellow :weight bold :underline t))))
   `(ivy-cursor ((t (:foreground ,zenburnt-bg :background ,zenburnt-fg))))
   `(ivy-match-required-face ((t (:foreground ,zenburnt-red :background ,zenburnt-bg))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,zenburnt-bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,zenburnt-green-2))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,zenburnt-green))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,zenburnt-green+1))))
   `(ivy-remote ((t (:foreground ,zenburnt-blue :background ,zenburnt-bg))))
   `(ivy-subdir ((t (:foreground ,zenburnt-yellow :background ,zenburnt-bg))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,zenburnt-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,zenburnt-yellow))))
   `(ido-indicator ((t (:foreground ,zenburnt-yellow :background ,zenburnt-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,zenburnt-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,zenburnt-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,zenburnt-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,zenburnt-red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,zenburnt-magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,zenburnt-orange))))
   `(jabber-roster-user-error ((t (:foreground ,zenburnt-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,zenburnt-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,zenburnt-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,zenburnt-red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,zenburnt-green+3))))
   `(jabber-activity-face((t (:foreground ,zenburnt-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,zenburnt-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,zenburnt-orange))))
   `(js2-error ((t (:foreground ,zenburnt-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,zenburnt-green-2))))
   `(js2-jsdoc-type ((t (:foreground ,zenburnt-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,zenburnt-green+3))))
   `(js2-function-param ((t (:foreground, zenburnt-orange))))
   `(js2-external-variable ((t (:foreground ,zenburnt-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,zenburnt-green-2))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,zenburnt-orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,zenburnt-red-1))))
   `(js2-object-property ((t (:foreground ,zenburnt-blue+1))))
   `(js2-magic-paren ((t (:foreground ,zenburnt-blue-5))))
   `(js2-private-function-call ((t (:foreground ,zenburnt-cyan))))
   `(js2-function-call ((t (:foreground ,zenburnt-cyan))))
   `(js2-private-member ((t (:foreground ,zenburnt-blue-1))))
   `(js2-keywords ((t (:foreground ,zenburnt-magenta))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,zenburnt-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,zenburnt-fg :weight normal))))
   `(ledger-font-payee-pending-face ((t (:foreground ,zenburnt-red :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,zenburnt-bg+1))))
   `(ledger-font-auto-xact-face ((t (:foreground ,zenburnt-yellow-1 :weight normal))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,zenburnt-green :weight normal))))
   `(ledger-font-pending-face ((t (:foreground ,zenburnt-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,zenburnt-fg))))
   `(ledger-font-posting-date-face ((t (:foreground ,zenburnt-orange :weight normal))))
   `(ledger-font-posting-account-face ((t (:foreground ,zenburnt-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,zenburnt-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,zenburnt-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,zenburnt-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,zenburnt-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,zenburnt-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,zenburnt-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,zenburnt-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,zenburnt-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,zenburnt-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,zenburnt-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,zenburnt-green+2 :background ,zenburnt-bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,zenburnt-bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,zenburnt-bg :background ,zenburnt-fg))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,zenburnt-yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,zenburnt-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,zenburnt-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,zenburnt-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,zenburnt-green+2 :background ,zenburnt-bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,zenburnt-blue-1))))
   `(lui-hilight-face ((t (:foreground ,zenburnt-green+2 :background ,zenburnt-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,zenburnt-green+2 :background ,zenburnt-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,zenburnt-red+1 :background ,zenburnt-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,zenburnt-blue+1 :background ,zenburnt-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,zenburnt-magenta :background ,zenburnt-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,zenburnt-yellow :background ,zenburnt-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(magit-section-highlight           ((t (:background ,zenburnt-bg+05))))
   `(magit-section-heading             ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,zenburnt-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,zenburnt-bg+05 :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,zenburnt-bg+05 :weight bold
                                                        :foreground ,zenburnt-orange))))
   `(magit-diff-added                  ((t (:background ,zenburnt-green-2))))
   `(magit-diff-added-highlight        ((t (:background ,zenburnt-green))))
   `(magit-diff-removed                ((t (:background ,zenburnt-red-4))))
   `(magit-diff-removed-highlight      ((t (:background ,zenburnt-red-3))))
   `(magit-diff-hunk-heading           ((t (:background ,zenburnt-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,zenburnt-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,zenburnt-bg+2
                                                        :foreground ,zenburnt-orange))))
   `(magit-diff-lines-heading          ((t (:background ,zenburnt-orange
                                                        :foreground ,zenburnt-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,zenburnt-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added              ((t (:foreground ,zenburnt-green+4))))
   `(magit-diffstat-removed            ((t (:foreground ,zenburnt-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,zenburnt-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,zenburnt-green-2 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,zenburnt-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,zenburnt-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,zenburnt-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,zenburnt-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,zenburnt-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,zenburnt-orange))))
   `(magit-log-date      ((t (:foreground ,zenburnt-fg-1))))
   `(magit-log-graph     ((t (:foreground ,zenburnt-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,zenburnt-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,zenburnt-green))))
   `(magit-sequence-part ((t (:foreground ,zenburnt-yellow))))
   `(magit-sequence-head ((t (:foreground ,zenburnt-blue))))
   `(magit-sequence-drop ((t (:foreground ,zenburnt-red))))
   `(magit-sequence-done ((t (:foreground ,zenburnt-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,zenburnt-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,zenburnt-green))))
   `(magit-bisect-skip ((t (:foreground ,zenburnt-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,zenburnt-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-blue-2))))
   `(magit-blame-hash    ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-blue-2))))
   `(magit-blame-name    ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-orange))))
   `(magit-blame-date    ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-orange))))
   `(magit-blame-summary ((t (:background ,zenburnt-bg-1 :foreground ,zenburnt-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,zenburnt-bg+3))))
   `(magit-hash           ((t (:foreground ,zenburnt-bg+3))))
   `(magit-tag            ((t (:foreground ,zenburnt-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,zenburnt-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,zenburnt-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,zenburnt-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,zenburnt-blue   :weight bold))))
   `(magit-refname        ((t (:background ,zenburnt-bg+2 :foreground ,zenburnt-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,zenburnt-bg+2 :foreground ,zenburnt-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,zenburnt-bg+2 :foreground ,zenburnt-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,zenburnt-green))))
   `(magit-signature-bad       ((t (:foreground ,zenburnt-red))))
   `(magit-signature-untrusted ((t (:foreground ,zenburnt-yellow))))
   `(magit-signature-expired   ((t (:foreground ,zenburnt-orange))))
   `(magit-signature-revoked   ((t (:foreground ,zenburnt-magenta))))
   '(magit-signature-error     ((t (:inherit    magit-signature-bad))))
   `(magit-cherry-unmatched    ((t (:foreground ,zenburnt-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,zenburnt-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,zenburnt-green))))
   `(magit-reflog-amend        ((t (:foreground ,zenburnt-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,zenburnt-green))))
   `(magit-reflog-checkout     ((t (:foreground ,zenburnt-blue))))
   `(magit-reflog-reset        ((t (:foreground ,zenburnt-red))))
   `(magit-reflog-rebase       ((t (:foreground ,zenburnt-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,zenburnt-green))))
   `(magit-reflog-remote       ((t (:foreground ,zenburnt-cyan))))
   `(magit-reflog-other        ((t (:foreground ,zenburnt-cyan))))
;;;;; markup-faces
   `(markup-anchor-face ((t (:foreground ,zenburnt-blue+1))))
   `(markup-code-face ((t (:inherit font-lock-constant-face))))
   `(markup-command-face ((t (:foreground ,zenburnt-yellow))))
   `(markup-emphasis-face ((t (:inherit bold))))
   `(markup-internal-reference-face ((t (:foreground ,zenburnt-yellow-2 :underline t))))
   `(markup-list-face ((t (:foreground ,zenburnt-fg+1))))
   `(markup-meta-face ((t (:foreground ,zenburnt-yellow))))
   `(markup-meta-hide-face ((t (:foreground ,zenburnt-yellow))))
   `(markup-secondary-text-face ((t (:foreground ,zenburnt-yellow-1))))
   `(markup-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))
   `(markup-value-face ((t (:foreground ,zenburnt-yellow))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,zenburnt-green+1))))
   `(message-header-other ((t (:foreground ,zenburnt-green))))
   `(message-header-to ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,zenburnt-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,zenburnt-green))))
   `(message-mml ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,zenburnt-orange))))
   `(mew-face-header-from ((t (:foreground ,zenburnt-yellow))))
   `(mew-face-header-date ((t (:foreground ,zenburnt-green))))
   `(mew-face-header-to ((t (:foreground ,zenburnt-red))))
   `(mew-face-header-key ((t (:foreground ,zenburnt-green))))
   `(mew-face-header-private ((t (:foreground ,zenburnt-green))))
   `(mew-face-header-important ((t (:foreground ,zenburnt-blue))))
   `(mew-face-header-marginal ((t (:foreground ,zenburnt-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,zenburnt-red))))
   `(mew-face-header-xmew ((t (:foreground ,zenburnt-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,zenburnt-red))))
   `(mew-face-body-url ((t (:foreground ,zenburnt-orange))))
   `(mew-face-body-comment ((t (:foreground ,zenburnt-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,zenburnt-green))))
   `(mew-face-body-cite2 ((t (:foreground ,zenburnt-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,zenburnt-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,zenburnt-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,zenburnt-red))))
   `(mew-face-mark-review ((t (:foreground ,zenburnt-blue))))
   `(mew-face-mark-escape ((t (:foreground ,zenburnt-green))))
   `(mew-face-mark-delete ((t (:foreground ,zenburnt-red))))
   `(mew-face-mark-unlink ((t (:foreground ,zenburnt-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,zenburnt-green))))
   `(mew-face-mark-unread ((t (:foreground ,zenburnt-red-2))))
   `(mew-face-eof-message ((t (:foreground ,zenburnt-green))))
   `(mew-face-eof-part ((t (:foreground ,zenburnt-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,zenburnt-cyan :background ,zenburnt-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,zenburnt-bg :background ,zenburnt-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,zenburnt-bg :background ,zenburnt-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,zenburnt-blue))))
   `(mingus-pausing-face ((t (:foreground ,zenburnt-magenta))))
   `(mingus-playing-face ((t (:foreground ,zenburnt-cyan))))
   `(mingus-playlist-face ((t (:foreground ,zenburnt-cyan ))))
   `(mingus-mark-face ((t (:bold t :foreground ,zenburnt-magenta))))
   `(mingus-song-file-face ((t (:foreground ,zenburnt-yellow))))
   `(mingus-artist-face ((t (:foreground ,zenburnt-cyan))))
   `(mingus-album-face ((t (:underline t :foreground ,zenburnt-red+1))))
   `(mingus-album-stale-face ((t (:foreground ,zenburnt-red+1))))
   `(mingus-stopped-face ((t (:foreground ,zenburnt-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,zenburnt-yellow))))
   `(nav-face-button-num ((t (:foreground ,zenburnt-cyan))))
   `(nav-face-dir ((t (:foreground ,zenburnt-green))))
   `(nav-face-hdir ((t (:foreground ,zenburnt-red))))
   `(nav-face-file ((t (:foreground ,zenburnt-fg))))
   `(nav-face-hfile ((t (:foreground ,zenburnt-red-4))))
;;;;; merlin
   `(merlin-type-face ((t (:inherit highlight))))
   `(merlin-compilation-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburnt-orange)))
      (t
       (:underline ,zenburnt-orange))))
   `(merlin-compilation-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburnt-red)))
      (t
       (:underline ,zenburnt-red))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,zenburnt-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,zenburnt-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,zenburnt-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,zenburnt-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,zenburnt-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,zenburnt-green-2 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,zenburnt-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,zenburnt-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,zenburnt-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,zenburnt-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,zenburnt-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,zenburnt-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,zenburnt-bg+1))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,zenburnt-blue+1 :weight bold))))
   `(neo-header-face ((t (:foreground ,zenburnt-fg))))
   `(neo-root-dir-face ((t (:foreground ,zenburnt-blue+1 :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,zenburnt-blue))))
   `(neo-file-link-face ((t (:foreground ,zenburnt-fg))))
   `(neo-expand-btn-face ((t (:foreground ,zenburnt-blue))))
   `(neo-vc-default-face ((t (:foreground ,zenburnt-fg+1))))
   `(neo-vc-user-face ((t (:foreground ,zenburnt-red :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,zenburnt-fg))))
   `(neo-vc-edited-face ((t (:foreground ,zenburnt-magenta))))
   `(neo-vc-needs-merge-face ((t (:foreground ,zenburnt-red+1))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,zenburnt-red :background ,zenburnt-blue-5))))
   `(neo-vc-added-face ((t (:foreground ,zenburnt-green+1))))
   `(neo-vc-conflict-face ((t (:foreground ,zenburnt-red+1))))
   `(neo-vc-missing-face ((t (:foreground ,zenburnt-red+1))))
   `(neo-vc-ignored-face ((t (:foreground ,zenburnt-fg-1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,zenburnt-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,zenburnt-fg :weight bold))))
   `(org-block ((t (:background ,zenburnt-bg+05 :extend t))))
   `(org-checkbox ((t (:background ,zenburnt-bg+2 :foreground ,zenburnt-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,zenburnt-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,zenburnt-red-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,zenburnt-green+3))))
   `(org-formula ((t (:foreground ,zenburnt-yellow-2))))
   `(org-headline-done ((t (:foreground ,zenburnt-green+3))))
   `(org-hide ((t (:foreground ,zenburnt-bg))))
   `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-orange
                               ,@(when zenburnt-scale-org-headlines
                                   (list :height zenburnt-height-plus-4))))))
   `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-green+4
                               ,@(when zenburnt-scale-org-headlines
                                   (list :height zenburnt-height-plus-3))))))
   `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-blue-1
                               ,@(when zenburnt-scale-org-headlines
                                   (list :height zenburnt-height-plus-2))))))
   `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-yellow-2
                               ,@(when zenburnt-scale-org-headlines
                                   (list :height zenburnt-height-plus-1))))))
   `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-cyan))))
   `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-green+2))))
   `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-red-4))))
   `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-blue-4))))
   `(org-link ((t (:foreground ,zenburnt-yellow-2 :underline t))))
   `(org-quote ((t (:background ,zenburnt-bg+05 :extend t))))
   `(org-scheduled ((t (:foreground ,zenburnt-green+4))))
   `(org-scheduled-previously ((t (:foreground ,zenburnt-red))))
   `(org-scheduled-today ((t (:foreground ,zenburnt-blue+1))))
   `(org-sexp-date ((t (:foreground ,zenburnt-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,zenburnt-green+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,zenburnt-orange))))
   `(org-todo ((t (:weight bold :foreground ,zenburnt-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,zenburnt-red :weight bold :underline nil))))
   `(org-column ((t (:background ,zenburnt-bg-1))))
   `(org-column-title ((t (:background ,zenburnt-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,zenburnt-bg :background ,zenburnt-red-1))))
   `(org-ellipsis ((t (:foreground ,zenburnt-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,zenburnt-cyan :underline t))))
   `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-blue
                                      :weight bold
                                      ,@(when zenburnt-scale-org-headlines
                                          (list :height zenburnt-height-plus-4))))))
   `(org-document-info ((t (:foreground ,zenburnt-blue))))
   `(org-habit-ready-face ((t :background ,zenburnt-green)))
   `(org-habit-alert-face ((t :background ,zenburnt-yellow-1 :foreground ,zenburnt-bg)))
   `(org-habit-clear-face ((t :background ,zenburnt-blue-3)))
   `(org-habit-overdue-face ((t :background ,zenburnt-red-3)))
   `(org-habit-clear-future-face ((t :background ,zenburnt-blue-4)))
   `(org-habit-ready-future-face ((t :background ,zenburnt-green-2)))
   `(org-habit-alert-future-face ((t :background ,zenburnt-yellow-2 :foreground ,zenburnt-bg)))
   `(org-habit-overdue-future-face ((t :background ,zenburnt-red-4)))
;;;;; org-ref
   `(org-ref-ref-face ((t :underline t)))
   `(org-ref-label-face ((t :underline t)))
   `(org-ref-cite-face ((t :underline t)))
   `(org-ref-glossary-face ((t :underline t)))
   `(org-ref-acronym-face ((t :underline t)))
;;;;; outline
   `(outline-1 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-orange
                             ,@(when zenburnt-scale-outline-headlines
                                 (list :height zenburnt-height-plus-4))))))
   `(outline-2 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-green+4
                             ,@(when zenburnt-scale-outline-headlines
                                 (list :height zenburnt-height-plus-3))))))
   `(outline-3 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-blue-1
                             ,@(when zenburnt-scale-outline-headlines
                                 (list :height zenburnt-height-plus-2))))))
   `(outline-4 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-yellow-2
                             ,@(when zenburnt-scale-outline-headlines
                                 (list :height zenburnt-height-plus-1))))))
   `(outline-5 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-cyan))))
   `(outline-6 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-green+2))))
   `(outline-7 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-red-4))))
   `(outline-8 ((t (:inherit ,z-variable-pitch :foreground ,zenburnt-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; c/perl
   `(cperl-nonoverridable-face ((t (:foreground ,zenburnt-magenta))))
   `(cperl-array-face ((t (:foreground ,zenburnt-yellow, :backgorund ,zenburnt-bg))))
   `(cperl-hash-face ((t (:foreground ,zenburnt-yellow-1, :background ,zenburnt-bg))))
;;;;; paren-face
   `(parenthesis ((t (:foreground ,zenburnt-fg-1))))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,zenburnt-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,zenburnt-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,zenburnt-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,zenburnt-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,zenburnt-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,zenburnt-bg :background ,zenburnt-orange))))
   `(proof-error-face ((t (:foreground ,zenburnt-fg :background ,zenburnt-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,zenburnt-bg :background ,zenburnt-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,zenburnt-bg :background ,zenburnt-orange))))
   `(proof-locked-face ((t (:background ,zenburnt-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,zenburnt-bg :background ,zenburnt-orange))))
   `(proof-queue-face ((t (:background ,zenburnt-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,zenburnt-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,zenburnt-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,zenburnt-bg))))
   `(proof-warning-face ((t (:foreground ,zenburnt-bg :background ,zenburnt-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,zenburnt-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,zenburnt-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,zenburnt-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,zenburnt-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,zenburnt-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,zenburnt-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,zenburnt-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,zenburnt-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,zenburnt-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,zenburnt-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,zenburnt-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,zenburnt-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,zenburnt-blue))))
   `(rcirc-other-nick ((t (:foreground ,zenburnt-orange))))
   `(rcirc-bright-nick ((t (:foreground ,zenburnt-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,zenburnt-blue-2))))
   `(rcirc-server ((t (:foreground ,zenburnt-green))))
   `(rcirc-server-prefix ((t (:foreground ,zenburnt-green+1))))
   `(rcirc-timestamp ((t (:foreground ,zenburnt-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,zenburnt-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:weight bold))))
   `(rcirc-prompt ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:weight bold))))
   `(rcirc-url ((t (:weight bold))))
   `(rcirc-keyword ((t (:foreground ,zenburnt-yellow :weight bold))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,zenburnt-bg :background ,zenburnt-magenta))))
   `(reb-match-1 ((t (:foreground ,zenburnt-bg :background ,zenburnt-blue))))
   `(reb-match-2 ((t (:foreground ,zenburnt-bg :background ,zenburnt-orange))))
   `(reb-match-3 ((t (:foreground ,zenburnt-bg :background ,zenburnt-red))))
;;;;; realgud
   `(realgud-overlay-arrow1 ((t (:foreground ,zenburnt-green))))
   `(realgud-overlay-arrow2 ((t (:foreground ,zenburnt-yellow))))
   `(realgud-overlay-arrow3 ((t (:foreground ,zenburnt-orange))))
   `(realgud-bp-enabled-face ((t (:inherit error))))
   `(realgud-bp-disabled-face ((t (:inherit secondary-selection))))
   `(realgud-bp-line-enabled-face ((t (:box (:color ,zenburnt-red :style nil)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color "grey70" :style nil)))))
   `(realgud-line-number ((t (:foreground ,zenburnt-yellow))))
   `(realgud-backtrace-number ((t (:foreground ,zenburnt-yellow, :weight bold))))
;;;;; regex-tool
   `(regex-tool-matched-face ((t (:background ,zenburnt-blue-4 :weight bold))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,zenburnt-green))))
   `(rpm-spec-doc-face ((t (:foreground ,zenburnt-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,zenburnt-red))))
   `(rpm-spec-macro-face ((t (:foreground ,zenburnt-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,zenburnt-red))))
   `(rpm-spec-package-face ((t (:foreground ,zenburnt-red))))
   `(rpm-spec-section-face ((t (:foreground ,zenburnt-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,zenburnt-blue))))
   `(rpm-spec-var-face ((t (:foreground ,zenburnt-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,zenburnt-orange))))
   `(rst-level-2-face ((t (:foreground ,zenburnt-green+1))))
   `(rst-level-3-face ((t (:foreground ,zenburnt-blue-1))))
   `(rst-level-4-face ((t (:foreground ,zenburnt-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,zenburnt-cyan))))
   `(rst-level-6-face ((t (:foreground ,zenburnt-green-2))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,zenburnt-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,zenburnt-red+1 :background ,zenburnt-bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,zenburnt-fg :background ,zenburnt-bg+3 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Zenburnt for sml
   `(sml/global ((,class (:foreground ,zenburnt-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,zenburnt-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,zenburnt-fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,zenburnt-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,zenburnt-blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,zenburnt-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,zenburnt-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,zenburnt-orange))))
   `(sml/git ((,class (:foreground ,zenburnt-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,zenburnt-orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,zenburnt-red-2))))
   `(sml/outside-modified ((,class (:foreground ,zenburnt-orange))))
   `(sml/modified ((,class (:foreground ,zenburnt-red))))
   `(sml/vc-edited ((,class (:foreground ,zenburnt-green+2))))
   `(sml/charging ((,class (:foreground ,zenburnt-green+4))))
   `(sml/discharging ((,class (:foreground ,zenburnt-red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,zenburnt-red+1 :background ,zenburnt-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,zenburnt-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,zenburnt-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,zenburnt-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburnt-red)))
      (t
       (:underline ,zenburnt-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburnt-orange)))
      (t
       (:underline ,zenburnt-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburnt-yellow)))
      (t
       (:underline ,zenburnt-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburnt-green)))
      (t
       (:underline ,zenburnt-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; solaire
   `(solaire-default-face ((t (:inherit default :background ,zenburnt-bg-08))))
   `(solaire-minibuffer-face ((t (:inherit default :background ,zenburnt-bg-08))))
   `(solaire-hl-line-face ((t (:inherit hl-line :background ,zenburnt-bg))))
   `(solaire-org-hide-face ((t (:inherit org-hide :background ,zenburnt-bg-08))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,zenburnt-green+2))))
   `(speedbar-directory-face ((t (:foreground ,zenburnt-cyan))))
   `(speedbar-file-face ((t (:foreground ,zenburnt-fg))))
   `(speedbar-highlight-face ((t (:foreground ,zenburnt-bg :background ,zenburnt-green+2))))
   `(speedbar-selected-face ((t (:foreground ,zenburnt-red))))
   `(speedbar-separator-face ((t (:foreground ,zenburnt-bg :background ,zenburnt-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,zenburnt-yellow))))
;;;;; swiper
   `(swiper-line-face ((t (:underline t))))
;;;;; sx
   `(sx-custom-button
     ((t (:background ,zenburnt-fg :foreground ,zenburnt-bg-1
          :box (:line-width 3 :style released-button) :height 0.9))))
   `(sx-question-list-answers
     ((t (:foreground ,zenburnt-green+3
          :height 1.0 :inherit sx-question-list-parent))))
   `(sx-question-mode-accepted
     ((t (:foreground ,zenburnt-green+3
          :height 1.3 :inherit sx-question-mode-title))))
   '(sx-question-mode-content-face ((t (:inherit highlight))))
   `(sx-question-mode-kbd-tag
     ((t (:box (:color ,zenburnt-bg-1 :line-width 3 :style released-button)
          :height 0.9 :weight semi-bold))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,zenburnt-fg
                                    :background ,zenburnt-bg))))
   `(tabbar-selected ((t (:foreground ,zenburnt-fg
                                      :background ,zenburnt-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,zenburnt-fg
                                        :background ,zenburnt-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,zenburnt-bg
                                       :background ,zenburnt-bg-1))))
   `(term-color-red ((t (:foreground ,zenburnt-red-2
                                     :background ,zenburnt-red-4))))
   `(term-color-green ((t (:foreground ,zenburnt-green
                                       :background ,zenburnt-green+2))))
   `(term-color-yellow ((t (:foreground ,zenburnt-orange
                                        :background ,zenburnt-yellow))))
   `(term-color-blue ((t (:foreground ,zenburnt-blue-1
                                      :background ,zenburnt-blue-4))))
   `(term-color-magenta ((t (:foreground ,zenburnt-magenta
                                         :background ,zenburnt-red))))
   `(term-color-cyan ((t (:foreground ,zenburnt-cyan
                                      :background ,zenburnt-blue))))
   `(term-color-white ((t (:foreground ,zenburnt-fg
                                       :background ,zenburnt-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,zenburnt-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,zenburnt-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,zenburnt-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,zenburnt-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,zenburnt-cyan))))
;;;;; visual-regexp
   `(vr/group-0 ((t (:foreground ,zenburnt-bg :background ,zenburnt-green :weight bold))))
   `(vr/group-1 ((t (:foreground ,zenburnt-bg :background ,zenburnt-orange :weight bold))))
   `(vr/group-2 ((t (:foreground ,zenburnt-bg :background ,zenburnt-blue :weight bold))))
   `(vr/match-0 ((t (:inherit isearch))))
   `(vr/match-1 ((t (:foreground ,zenburnt-yellow-2 :background ,zenburnt-bg-1 :weight bold))))
   `(vr/match-separator-face ((t (:foreground ,zenburnt-red :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,zenburnt-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,zenburnt-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,zenburnt-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,zenburnt-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,zenburnt-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,zenburnt-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,zenburnt-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,zenburnt-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,zenburnt-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,zenburnt-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,zenburnt-bg+1 :foreground ,zenburnt-bg+1))))
   `(whitespace-hspace ((t (:background ,zenburnt-bg+1 :foreground ,zenburnt-bg+1))))
   `(whitespace-tab ((t (:background ,zenburnt-red-1))))
   `(whitespace-newline ((t (:foreground ,zenburnt-bg+1))))
   `(whitespace-trailing ((t (:background ,zenburnt-red))))
   `(whitespace-line ((t (:background ,zenburnt-bg :foreground ,zenburnt-magenta))))
   `(whitespace-space-before-tab ((t (:background ,zenburnt-orange :foreground ,zenburnt-orange))))
   `(whitespace-indentation ((t (:background ,zenburnt-yellow :foreground ,zenburnt-red))))
   `(whitespace-empty ((t (:background ,zenburnt-yellow))))
   `(whitespace-space-after-tab ((t (:background ,zenburnt-yellow :foreground ,zenburnt-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,zenburnt-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,zenburnt-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,zenburnt-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,zenburnt-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,zenburnt-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,zenburnt-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,zenburnt-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,zenburnt-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,zenburnt-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,zenburnt-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,zenburnt-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,zenburnt-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,zenburnt-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,zenburnt-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,zenburnt-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,zenburnt-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,zenburnt-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,zenburnt-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,zenburnt-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,zenburnt-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,zenburnt-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,zenburnt-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,zenburnt-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,zenburnt-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,zenburnt-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,zenburnt-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,zenburnt-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,zenburnt-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,zenburnt-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,zenburnt-bg :background ,zenburnt-blue+1))))
   `(cscope-separator-face ((t (:foreground ,zenburnt-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,zenburnt-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,zenburnt-yellow :foreground ,zenburnt-yellow))))
   ))

;;; Theme Variables
(zenburnt-with-color-variables
  (custom-theme-set-variables
   'zenburnt
;;;;; ansi-color
   `(ansi-color-names-vector [,zenburnt-bg ,zenburnt-red ,zenburnt-green ,zenburnt-yellow
                                          ,zenburnt-blue ,zenburnt-magenta ,zenburnt-cyan ,zenburnt-fg])
;;;;; company-quickhelp
   `(company-quickhelp-color-background ,zenburnt-bg+1)
   `(company-quickhelp-color-foreground ,zenburnt-fg)
;;;;; fill-column-indicator
   `(fci-rule-color ,zenburnt-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,zenburnt-red ,zenburnt-orange ,zenburnt-yellow ,zenburnt-green ,zenburnt-green+4
       ,zenburnt-cyan ,zenburnt-blue+1 ,zenburnt-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,zenburnt-fg . ,zenburnt-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,zenburnt-red-1)
       ( 40. . ,zenburnt-red)
       ( 60. . ,zenburnt-orange)
       ( 80. . ,zenburnt-yellow-2)
       (100. . ,zenburnt-yellow-1)
       (120. . ,zenburnt-yellow)
       (140. . ,zenburnt-green-2)
       (160. . ,zenburnt-green)
       (180. . ,zenburnt-green+1)
       (200. . ,zenburnt-green+2)
       (220. . ,zenburnt-green+3)
       (240. . ,zenburnt-green+4)
       (260. . ,zenburnt-cyan)
       (280. . ,zenburnt-blue-2)
       (300. . ,zenburnt-blue-1)
       (320. . ,zenburnt-blue)
       (340. . ,zenburnt-blue+1)
       (360. . ,zenburnt-magenta)))
   `(vc-annotate-very-old-color ,zenburnt-magenta)
   `(vc-annotate-background ,zenburnt-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar zenburnt-add-font-lock-keywords nil
  "Whether to add font-lock keywords for zenburnt color names.

In buffers visiting library `zenburnt-theme.el' the zenburnt
specific keywords are always added, provided that library has
been loaded (because that is where the code that does it is
definded).  If you visit this file and only enable the theme,
then you have to turn `rainbow-mode' off and on again for the
zenburnt-specific font-lock keywords to be used.

In all other Emacs-Lisp buffers this variable controls whether
this should be done.  This requires library `rainbow-mode'.")

(defvar zenburnt-colors-font-lock-keywords nil)

(defun zenburnt--rainbow-turn-on ()
  "Maybe also add font-lock keywords for zenburnt colors."
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (or zenburnt-add-font-lock-keywords
                 (and (buffer-file-name)
                      (equal (file-name-nondirectory (buffer-file-name))
                             "zenburnt-theme.el"))))
    (unless zenburnt-colors-font-lock-keywords
      (setq zenburnt-colors-font-lock-keywords
            `((,(regexp-opt (mapcar 'car zenburnt-default-colors-alist) 'words)
               (0 (rainbow-colorize-by-assoc zenburnt-default-colors-alist))))))
    (font-lock-add-keywords nil zenburnt-colors-font-lock-keywords 'end)))

(defun zenburnt--rainbow-turn-off ()
  "Also remove font-lock keywords for zenburnt colors."
  (font-lock-remove-keywords nil zenburnt-colors-font-lock-keywords))

(when (fboundp 'advice-add)
  (advice-add 'rainbow-turn-on :after  #'zenburnt--rainbow-turn-on)
  (advice-add 'rainbow-turn-off :after #'zenburnt--rainbow-turn-off))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'zenburnt)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; zenburnt-theme.el ends here
