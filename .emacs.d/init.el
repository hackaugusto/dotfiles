(require 'cask "~/.cask/cask.el")
(cask-initialize)

(load-theme 'wombat)
(custom-set-faces
 '(elscreen-tab-background-face ((t nil)))
 '(elscreen-tab-control-face ((t nil)))
 '(elscreen-tab-current-screen-face ((t (:background "dim gray"))))
 '(elscreen-tab-other-screen-face ((t nil)))
 '(evil-search-highlight-persist-highlight-face ((t (:inherit isearch))))
 '(helm-selection ((t (:background "dim gray"))))
 '(helm-source-header ((t (:weight bold :height 1.3)))))

(custom-set-variables
 '(elscreen-tab-display-control nil)
 '(elscreen-tab-display-kill-screen nil))

(defalias 'yes-or-no-p 'y-or-n-p)

(defvar current-line 0)

(defadvice linum-update (before set-current-line activate)
  (setq current-line (line-number-at-pos)))

(defun font-lock-comment-annotations ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(defun add-to-hooks (fun hooks)
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun linum-format-func (line)
  (let ((width_ceiling (ceiling (log (count-lines (point-min) (point-max)) 10)))
        (width (length (number-to-string (count-lines (point-min) (point-max)))))
        (line-number (if (= line current-line) current-line (abs (- line current-line)))))
    (propertize (format (format "%%%dd " width) line-number)
                'face 'linum)))

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

(defun helm-projectile-ag ()
  (interactive)
  (if (projectile-project-p)
      (helm-ag (projectile-project-root))
    (helm-ag)))

(defun helm-open-vcs-files ()
  (interactive)
  ; these are helm variables
  (if (projectile-project-p)
      (helm-projectile)
      (helm-other-buffer '(helm-source-files-in-current-dir
                           helm-source-recentf
                           helm-source-buffers-list
                           helm-source-elscreen
                           )
                         "*helm-my-buffers*")))
; disable the interface
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(auto-complete-mode)
(ac-config-default)
(ido-mode 1)
(flx-ido-mode 1)
(ido-ubiquitous-mode 1)
(smex-initialize)
(electric-indent-mode)
(sackspace-global-mode 1)
(show-paren-mode 1)
(superword-mode 1)
(global-linum-mode)
(global-git-gutter-mode 1)
(indent-guide-global-mode)
(semantic-mode)
(smartparens-global-mode 1)
(projectile-global-mode)
(require 'expand-region)
(require 'saveplace)

(require 'helm-config) ; this cant go in the Cask file
(helm-mode 1)
;(helm-ag)

; _must_ be before (evil-mode 1)
(global-evil-leader-mode)
(global-evil-tabs-mode 1)
(evil-leader/set-leader "SPC")

(global-evil-surround-mode)
(evil-mode 1)
(key-chord-mode 1)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq-default inhibit-startup-screen t
              initial-scratch-message nil
              savehist-mode t
              truncate-lines t
              default-truncate-lines t
              column-number-mode t
              line-number-mode t
              tab-width 4
              indent-tabs-mode nil ; expand tabs
              fill-column 80
              make-backup-files nil
              read-file-name-completion-ignore-case t
              savehist-file "~/.emacs.d/savehist"
              save-place t
              use-dialog-box nil
              show-paren-delay 0)

(setq color-theme-is-global t
      jedi:complete-on-dot t
      undo-tree-save-history t
      linum-format 'linum-format-func
      ido-enable-flex-matching t
      ido-everywhere t
      helm-quick-update t
      helm-buffers-fuzzy-matching t
      key-chord-two-keys-delay 0.3
      large-file-warning-threshold 100000000 ; 100M
      visible-bell nil
      initial-major-mode 'text-mode
      redisplay-dont-pause t
      save-place-file "~/.emacs.d/saveplace"
      tooltip-use-echo-area t
      ac-auto-start 2
      ac-auto-show-menu nil
      ;ac-auto-show-menu 0.8
      ac-use-quick-help nil
      ac-use-fuzzy t
      ac-quick-help-delay 0.3
      ac-quick-help-height 50)

(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

(prefer-coding-system 'utf-8)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(add-hook
 'python-mode-hook
 (lambda ()
   (jedi:setup)
   (yas-minor-mode)
   (which-function-mode)
   (key-chord-define python-mode-map "gd" 'jedi:goto-definition)
   (key-chord-define python-mode-map "]d" 'er/mark-defun)))

(add-hook
 'linum-before-numbering-hook
 (lambda ()
   (setq-local linum-format-fmt
               (let ((width (length (number-to-string (count-lines (point-min) (point-max))))))
                 (concat "%" (number-to-string width) "d")))))
(add-hook
 'LaTeX-mode-hook
 (lambda ()
   (push '(?~ . ("\\texttt{" . "}")) evil-surround-pairs-alist)
   (push '(?= . ("\\verb=" . "=")) evil-surround-pairs-alist)
   (push '(?/ . ("\\emph{" . "}")) evil-surround-pairs-alist)
   (push '(?* . ("\\textbf{" . "}")) evil-surround-pairs-alist)
   (push '(?P . ("\\(" . "\\)")) evil-surround-pairs-alist)))

(add-hook
 'ido-setup-hook
 (lambda ()
   (define-key ido-completion-map (kbd "TAB") 'ido-next-match)
   (define-key ido-completion-map (kbd "<backtab>") 'ido-prev-match)))

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

; prefer helm-for-files
;(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(key-chord-define-global "]b" 'er/expand-region)


(global-evil-search-highlight-persist t)
;(global-relative-line-numbers-mode t)
(evil-leader/set-key "n" 'evil-search-highlight-persist-remove-all)
(evil-leader/set-key "w" 'save-buffer)
(evil-leader/set-key "q" 'elscreen-kill)
(evil-leader/set-key "s" 'split-window-horizontally)
(evil-leader/set-key "x" 'helm-M-x)
(evil-leader/set-key "k" 'helm-kill-ring)
(evil-leader/set-key "f" 'helm-open-vcs-files)
(evil-leader/set-key "F" 'helm-find-files)
(evil-leader/set-key "i" 'helm-semantic-or-imenu)
(evil-leader/set-key "m" 'helm-man-woman)
(evil-leader/set-key "/" 'helm-projectile-ag)

; C-u bound universal-argument will be shadowed
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-<up>") 'keyboard-up)
(define-key evil-normal-state-map (kbd "C-<dow>") 'keyboard-down)
(define-key evil-normal-state-map (kbd "C-<right>") 'keyboard-right)
(define-key evil-normal-state-map (kbd "C-<left>") 'keyboard-left)

(define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
(define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
;would shadow help
;(define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
(define-key evil-normal-state-map (kbd "C-l") 'windmove-right)

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-visual-state-map (kbd "j") 'evil-next-line)
(define-key evil-visual-state-map (kbd "k") 'evil-previous-line)

(key-chord-define-global "jk" 'evil-normal-state)

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
