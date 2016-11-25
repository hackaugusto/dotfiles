(defalias 'yes-or-no-p 'y-or-n-p)

(defvar current-line 0)

(defun add-to-hooks (fun hooks)
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun def-keys (map key def &rest bindings)
  (while key
    (define-key map (read-kbd-macro key) def)
    (setq key (pop bindings)
	  def (pop bindings))))

(defadvice linum-update (before set-current-line activate)
  (setq current-line (line-number-at-pos)))

(defun font-lock-comment-annotations ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
	  1 font-lock-warning-face t))))

(defun linum-format-func (line)
  (let ((width_ceiling (ceiling (log (count-lines (point-min) (point-max)) 10)))
	(width (length (number-to-string (count-lines (point-min) (point-max)))))
	(line-number (if (= line current-line) current-line (abs (- line current-line)))))
    (propertize (format (format "%%%dd " width) line-number)
		'face 'linum)))

(defun python-highlight-pdb ()
  (interactive)
  ; TODO: evil-search-highlight-persist-remove-all should not remove this
  (highlight-regexp "pdb\\.\\(set_trace\\|post_mortem\\|run\\(call\\|ctx\\|eval\\)?\\)([^)\n\r]*)")
  (highlight-regexp "import\\( \\|.*[, ]\\)pdb"))

(defun minibuffer-keyboard-quit ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun multicursor-next-like-this-force-normal (rawarg)
  (interactive "P")
  (if (and (evil-visual-state-p)
	   (> (count-lines (marker-position evil-visual-beginning) (marker-position evil-visual-end)) 1))
    (mc/edit-lines rawarg)
    (mc/mark-next-like-this (prefix-numeric-value rawarg))))

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

(defun strip-^m ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil nil)
    (replace-match "\\")))

(require 'expand-region)
(require 'saveplace)

;(line-number-mode t)
;gutter is clashing with relative number
;(global-git-gutter-mode t)
;(global-linum-mode)
;(sackspace-global-mode t)
;(smartparens-global-mode t)
(superword-mode t)

; _must_ be before (ido-mode t)
(flx-ido-mode t)
(ido-ubiquitous-mode t)

;helm conflicts with smex
;(require 'helm-config)
;(helm-mode t)

; _must_ be before (evil-mode t)
(smex-initialize)
;(global-relative-line-numbers-mode t)

(ido-mode t)

(prefer-coding-system 'utf-8)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(setq auto-mode-alist
      (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
	try-expand-all-abbrevs
	try-expand-dabbrev
	try-expand-dabbrev-from-kill
	try-expand-dabbrev-all-buffers
	try-complete-file-name-partially
	try-complete-file-name))

(setq ;ac-auto-show-menu 0.8
      ac-auto-show-menu nil
      ac-auto-start 2
      ac-quick-help-delay 0.3
      ac-quick-help-height 50
      ac-use-fuzzy t
      ac-use-quick-help nil
      jedi:complete-on-dot t
      read-file-name-completion-ignore-case t

      backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      make-backup-files t

      bookmark-default-file "~/.emacs.d/bookmarks"

      color-theme-is-global t
      default-truncate-lines t
      fill-column 80

      helm-buffers-fuzzy-matching t
      helm-quick-update t
      ido-enable-flex-matching t
      ido-everywhere t

      indent-tabs-mode nil ; expand tabs
      inhibit-startup-screen t
      initial-major-mode 'text-mode
      initial-scratch-message nil
      tooltip-use-echo-area t
      use-dialog-box nil
      visible-bell nil

      redisplay-dont-pause t
      key-chord-two-keys-delay 0.3
      large-file-warning-threshold 100000000 ; 100M
      show-paren-delay 0

      undo-tree-save-history t
      savehist-additional-variables '(search-ring regexp-search-ring)
      savehist-file "~/.emacs.d/savehist"
      save-place-file "~/.emacs.d/saveplace"
      save-place t

      ;linum-format 'linum-format-func
      tab-width 4
      truncate-lines t)

(if (not (file-exists-p "~/.emacs.d/backups"))
    (mkdir "~/.emacs.d/backups" t))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'before-save-hook 'whitespace-cleanup) ; respect tab configuration (better than delete-trailing-space)

(add-hook
 'prog-mode-hook
 (lambda ()
   (semantic-mode)
   (yas-minor-mode)
   (indent-guide-global-mode)
   (projectile-global-mode)
   (font-lock-comment-annotations)
   (electric-indent-mode)))

(add-hook
 'python-mode-hook
 (lambda ()
   (jedi:setup)
   (which-function-mode)
   (python-highlight-pdb)
   (define-key python-mode-map (kbd "RET") 'newline-and-indent)
   (key-chord-define python-mode-map [?\ ?\ ] 'inferior-python-mode) ; this can take some time
   (key-chord-define python-mode-map "gd" 'jedi:goto-definition)
   (key-chord-define python-mode-map "]d" 'er/mark-defun)))

(add-hook
 'inferior-python-mode-hook
 (lambda ()
   (indent-guide-mode -1)))

(add-to-hooks
 (lambda ()
   ; be sure to not use tabs
   (setq indent-tabs-mode -1))
 '(python-mode-hook inferior-python-mode-hook))

; (add-hook
;  'linum-before-numbering-hook
;  (lambda ()
;    (setq-local linum-format-fmt
;	       (let ((width (length (number-to-string (count-lines (point-min) (point-max))))))
;		 (concat "%" (number-to-string width) "d")))))

(add-hook
 'LaTeX-mode-hook
 (lambda ()
   (latex-preview-pane-mode)
   (ispell-change-dictionary "pt_BR")
   (flyspell-mode)
   (auto-fill-mode)
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

(evil-define-command evil-insert-paste-after ()
  (evil-normal-state)
  (evil-paste-after 1))

(evil-define-command evil-insert-paste-before ()
  (evil-normal-state)
  (evil-paste-before 1))

; prefer helm-for-files
;(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key [(control tab)] 'hippie-expand)
(global-set-key [escape] 'evil-exit-emacs-state)

(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key key-translation-map (kbd "C-c") 'esc-evil)
(define-key evil-normal-state-map [escape] 'keyboard-quit)

(key-chord-define-global "]b" 'er/expand-region)
(key-chord-define-global "jk" 'evil-normal-state)

(def-keys 'help-command
  "C-l" 'find-library
  "C-f" 'find-function
  "C-k" 'find-function-on-key
  "C-v" 'find-variable)

(def-keys evil-visual-state-map
  "j" 'evil-next-line
  "k" 'evil-previous-line)

(def-keys evil-insert-state-map
  "M-P" 'evil-insert-paste-before
  "M-p" 'evil-insert-paste-after)

(evil-leader/set-key
  "F" 'helm-find-files
  "f" 'helm-open-vcs-files
  "/" 'helm-projectile-ag
  "i" 'helm-semantic-or-imenu
  "k" 'helm-kill-ring
  "m" 'mc/mark-next-like-thi ; multicursor-next-like-this-force-normal
  "M" 'mc/edit-lines
  "n" 'evil-search-highlight-persist-remove-all
  "q" 'elscreen-kill
  "s" 'split-window-horizontally
  "w" 'save-buffer
  "x" 'helm-M-x)

(def-keys evil-normal-state-map
  "C-u" 'evil-scroll-up ; C-u bound universal-argument will be shadowed
  "C-<up>" 'keyboard-up
  "C-<dow>" 'keyboard-down
  "C-<right>" 'keyboard-right
  "C-<left>" 'keyboard-left

  "C-j" 'windmove-down
  "C-k" 'windmove-up
  ;would shadow help
  ;"C-h" 'windmove-left
  "C-l" 'windmove-right

  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)
