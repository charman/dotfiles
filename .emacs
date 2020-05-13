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


;; Markdown Mode - use GitHub flavored Markdown (gfm) for all .MD files
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))


;; Highlight trailing whitespace
(load-file "~/.emacs.d/highlight-chars.el")
(require 'highlight-chars)

(add-hook 'python-mode-hook 'hc-highlight-trailing-whitespace)
(add-hook 'markdown-mode 'hc-highlight-trailing-whitespace)

