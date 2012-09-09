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

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)

;; </INDENTING TEXT>


; Whenever we're in text mode, automatically break lines at right margin.
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
