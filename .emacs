;; <INDENTING TEXT>
;;   Code copied from:  http://www.emacswiki.org/emacs/IndentingText

;; Shift the selected region right if distance is postive, left if
;; negative

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
;; the following so that Ctrl-Shift-Right Arrow moves selected text one 
;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
;; column to the left:

;; When using macOS's Terminal program, the C-S-right and C-S-left
;; key-combinations are not being recognized - though they are
;; recognized when using the Carbon-native version of emacs.
;; In order to use C-S-right/left with the macOS Terminal program,
;; I went into Terminal Preferences (under Profiles / Keyboard)
;; and configured Terminal to send the keycodes below.
(define-key input-decode-map "\e[1;6C" [C-S-right])
(define-key input-decode-map "\e[1;6D" [C-S-left])

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)

;; </INDENTING TEXT>


;; Whenever we're in text mode, automatically break lines at right margin.
(setq text-mode-hook 'turn-on-auto-fill)


(cond
 ((= 22 emacs-major-version)
  ;;  Use personal copy of ruby-mode when using Emacs 22 (e.g. on OS X)
  ;;  ruby-mode is included in Emacs 23
  ;;  For more info, see:
  ;;    http://www.emacswiki.org/emacs/RubyMode
  (load-file "~/.emacs.d/ruby-mode/ruby-mode.el")
  (autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))))


;;  Create minor-mode that disables backups and auto-saving
;;  
;;  Source:
;;    http://anirudhsasikumar.net/blog/2005.01.21.html

(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
   It disables backup creation and auto saving.

   With no argument, this command toggles the mode.
   Non-null prefix argument turns on the mode.
   Null prefix argument turns off the mode."

  ;; The initial value.
  nil

  ;; The indicator for the mode line.
  " Sensitive"

  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
	;; disable backups
	(set (make-local-variable 'backup-inhibited) t)
	;; disable auto-save
	(if auto-save-default
	        (auto-save-mode -1)))
    ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;resort to default auto save setting
    (if auto-save-default
	(auto-save-mode 1))))

;; Disable auto-save and backups for SVN commit files
(setq auto-mode-alist 
      (append '(("svn-commit\\.tmp$" . sensitive-mode)) auto-mode-alist))


;; web-mode provides support for editing HTML/JavaScript/CSS/Django
;;   http://web-mode.org
(load-file "~/.emacs.d/web-mode.el")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."

  ;; Use spaces instead of tabs
  (setq-default indent-tabs-mode nil)

  ;; Delete trailing whitespace when saving files
  (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
              nil))
)
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'js-mode-hook 'my-web-mode-hook)

(defun set-web-indent (tabwidth)
  "Set web-mode/js-mode tab width"
  (interactive "nEnter tab width: ")
  (message "tab width is %d" tabwidth)
  (setq web-mode-code-indent-offset tabwidth)
  (setq web-mode-css-indent-offset tabwidth)
  (setq web-mode-markup-indent-offset tabwidth)
  (setq js-indent-level tabwidth))

(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-script-padding 0)
(setq web-mode-style-padding 0)

(setq js-indent-level 2)
