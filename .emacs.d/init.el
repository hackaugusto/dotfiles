(require 'cask "~/.cask/cask.el")
(cask-initialize)

; disable the interface
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(auto-complete-mode)
(ac-config-default)
(evil-surround-mode)
(evil-mode 1)
(key-chord-mode 1)
(sackspace-global-mode 1)
(show-paren-mode 1)
(global-linum-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(defvar current-line 0)

(defadvice linum-update (before set-current-line activate)
  (setq current-line (line-number-at-pos)))

(defun add-to-hooks (fun hooks)
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun linum-format-func (line)
  (let ((width_ceiling (ceiling (log (count-lines (point-min) (point-max)) 10)))
        (width (length (number-to-string (count-lines (point-min) (point-max)))))
        (line-number (if (= line current-line) current-line (abs (- line current-line)))))
    (propertize (format (format "%%%dd" width) line-number)
                'face (if (= current-line line) 'linum-current-line 'linum))))

(defun minibuffer-keyboard-quit ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun esc-evil (prompt)
  (cond
    ((or
      (evil-insert-state-p)
      (evil-normal-state-p)
      (evil-replace-state-p)
      (evil-visual-state-p))
     [escape])
    (t (kbd "C-g"))))

(setq-default inhibit-startup-screen t
              scroll-bar-mode -1
              menu-bar-mode -1
              tool-bar-mode -1
              savehist-mode t
              initial-scratch-message nil
              truncate-lines t
              default-truncate-lines t
              column-number-mode t
              line-number-mode t
              tab-width 4
              indent-tabs-mode nil
              fill-column 80
              make-backup-files nil
              read-file-name-completion-ignore-case t
              savehist-file "~/.emacs.d/savehist"
              use-dialog-box nil
              show-paren-delay 0)
(setq evil-leader/in-all-states 1
      jedi:complete-on-dot t
      undo-tree-save-history t
      linum-format 'linum-format-func
      ac-auto-start 2
      ac-auto-show-menu t
      ac-use-fuzzy t
      ac-quick-help-delay 0.3
      ac-quick-help-height 50)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(add-hook 'linum-before-numbering-hook
  (lambda ()
    (setq-local linum-format-fmt
      (let ((width (length (number-to-string (count-lines (point-min) (point-max))))))
        (concat "%" (number-to-string width) "d")))))
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push '(?~ . ("\\texttt{" . "}")) evil-surround-pairs-alist)
            (push '(?= . ("\\verb=" . "=")) evil-surround-pairs-alist)
            (push '(?/ . ("\\emph{" . "}")) evil-surround-pairs-alist)
            (push '(?* . ("\\textbf{" . "}")) evil-surround-pairs-alist)
            (push '(?P . ("\\(" . "\\)")) evil-surround-pairs-alist)))
(add-to-hooks
 (lambda ()
   (push '(?c . (":class:`" . "`")) evil-surround-pairs-alist)
   (push '(?f . (":func:`" . "`")) evil-surround-pairs-alist)
   (push '(?m . (":meth:`" . "`")) evil-surround-pairs-alist)
   (push '(?a . (":attr:`" . "`")) evil-surround-pairs-alist)
   (push '(?e . (":exc:`" . "`")) evil-surround-pairs-alist))
   '(rst-mode-hook python-mode-hook))

(add-to-hooks
 (lambda ()
   (push '(?` . ("`" . "'")) evil-surround-pairs-alist))
 '(emacs-lisp-mode-hook lisp-mode-hook))
(add-to-hooks
 (lambda ()
   (push '(?~ . ("``" . "``")) evil-surround-pairs-alist))
 '(markdown-mode-hook rst-mode-hook python-mode-hook))

(global-evil-search-highlight-persist t)
(global-evil-tabs-mode t)
;(global-relative-line-numbers-mode t)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(evil-leader/set-key "n" 'evil-search-highlight-persist-remove-all)
(evil-leader/set-key "w" 'save-buffer)
(evil-leader/set-key "q" 'elscreen-kill)
(evil-leader/set-key "s" 'split-window-horizontally)

; C-u is bound to universal-argument
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-<up>") 'keyboard-up)
(define-key evil-normal-state-map (kbd "C-<dow>") 'keyboard-down)
(define-key evil-normal-state-map (kbd "C-<right>") 'keyboard-right)
(define-key evil-normal-state-map (kbd "C-<left>") 'keyboard-left)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-visual-state-map (kbd "j") 'evil-next-line)
(define-key evil-visual-state-map (kbd "k") 'evil-previous-line)
; In emacs (and emacs-nox) M-k is bound to kill-setence
(key-chord-define-global "jk" 'evil-normal-state)
; esc quits
(global-set-key [escape] 'evil-exit-emacs-state)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key key-translation-map (kbd "C-c") 'esc-evil)
