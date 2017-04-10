;;; init.el --- my custom configuration
;;; Commentary:
;;
;; using:
;; package.el instead of el-get and cask
;; auto-complete instead of company
;; flycheck instead of flymake
;; jedi.el instead of anaconda

;;;; Code:

;; NOTE: eval-after-load executes only once, use it to setup keymaps

;;; initialize package.el
(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;;; utils/deps for init.el
(defun def-keys (map key def &rest bindings)
  "Binds in MAP the KEY to the function DEF, additional pairs can be provided in BINDINGS."
  (while key
    (define-key map (read-kbd-macro key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun add-to-hooks (fun hooks)
  "Add FUN to all HOOKS."
  (dolist (hook hooks)
    (add-hook hook fun)))

;;; http://stackoverflow.com/questions/6762686/prevent-emacs-from-asking-modified-buffers-exist-exit-anyway
(defun noconfirm-save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer(once only), then kill this Emacs process.
With prefix ARG, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (save-some-buffers arg t)
  (and (or (not (fboundp 'process-list))
       ;; process-list is not defined on MSDOS.
       (let ((processes (process-list))
         active)
         (while processes
           (and (memq (process-status (car processes)) '(run stop open listen))
            (process-query-on-exit-flag (car processes))
            (setq active t))
           (setq processes (cdr processes)))
         (or (not active)
         (progn (list-processes t)
            (yes-or-no-p "Active processes exist; kill them and exit anyway? ")))))
       ;; Query the user for other things, perhaps.
       (run-hook-with-args-until-failure 'kill-emacs-query-functions)
       (or (null confirm-kill-emacs)
       (funcall confirm-kill-emacs "Really exit Emacs? "))
       (kill-emacs)))

(defun ensure (package)
  "Make sure PACKAGE is installed with package.el."
  (if (not (package-installed-p package))
      (package-install package))
  (require package))

(ensure 'dash)

;;; install runtime packages
(defun package-install-all (packages)
  "Install the missing PACKAGES."
  (let ((uninstalled-packages
         (-remove 'package-installed-p packages)))
    (if (> (length uninstalled-packages) 0)
        (progn
          (package-refresh-contents)
          (dolist (package packages)
            (package-install package)))))) ;; install dependencies and update loaddefs.el

(package-install-all
 '(ido-ubiquitous smex flx flx-ido linum-relative
   evil key-chord evil-surround evil-leader evil-tabs evil-search-highlight-persist
   direx projectile git-gutter git-timemachine magit
   auto-complete flycheck idle-highlight-mode multiple-cursors yasnippet
   ;; indent-guide -> breaks the auto-complete menu
   jedi pyenv-mode python python-mode
   rust-mode solidity-mode
   ;; package-utils -> using paradox instead
   paradox deferred))

;;; emacs configuration
(setq-default inhibit-startup-screen t
              initial-scratch-message nil
              use-dialog-box nil

              redisplay-dont-pause t
              gc-cons-threshold 20000000
              ;; debug-on-error t
              ;; stack-trace-on-error t
              large-file-warning-threshold 100000000 ; 100M

              undo-tree-save-history t
              savehist-additional-variables '(search-ring regexp-search-ring)
              save-place t
              savehist-file "~/.emacs.d/savehist"
              save-place-file "~/.emacs.d/saveplace"

              tab-width 4
              truncate-lines t
              default-truncate-lines t
              column-number-mode t
              line-number-mode t

              indent-tabs-mode nil ; expand tabs
              inhibit-startup-screen t
              initial-major-mode 'text-mode
              initial-scratch-message nil
              tooltip-use-echo-area t
              use-dialog-box nil
              visible-bell nil

              backup-by-copying t
              backup-directory-alist '(("." . "~/.emacs.d/backups"))
              delete-old-versions t
              version-control t
              kept-new-versions 2
              kept-old-versions 5
              make-backup-files t

              bookmark-default-file "~/.emacs.d/bookmarks"
              custom-file "~/.emacs.d/custom.el"

              color-theme-is-global t
              default-truncate-lines t
              fill-column 80)

(if (not (file-exists-p "~/.emacs.d/backups")) (mkdir "~/.emacs.d/backups" t))
(load custom-file :noerror :nomessage)

(require 'recentf)
(recentf-mode t)
(setq recentf-max-menu-items 25)

;(server-start)
(load-theme 'wombat)
(prefer-coding-system 'utf-8)

(file-name-shadow-mode t)
(savehist-mode t)
(global-linum-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode 1)
(blink-cursor-mode -1)

(setq-default show-paren-delay 0)
(show-paren-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(fset 'save-buffers-kill-emacs 'noconfirm-save-buffers-kill-emacs)

(custom-set-variables
 '(linum-relative-current-symbol ""))

(custom-set-variables
 '(initial-frame-alist (quote ((maximized)))))

(def-keys 'help-command
  "C-l" 'find-library
  "C-f" 'find-function
  "C-k" 'find-function-on-key
  "C-v" 'find-variable)

;; modeline
(custom-set-variables
 '(evil-mode-line-format 'nil))

;; (setq mode-line-format '("%e"   ; out-of-memory error
;;                 evil-mode-line-tag
;;                 ;; (vc-mode vc-mode)
;;                 "%f"   ; current file
;;                 "%+"   ; readonly=& modified=* otherwise=-
;;                 " "
;;                 mode-line-modes
;;                 mode-line-misc-info
;;                 mode-line-end-spaces))

;; helm
;;(custom-set-faces
;; '(helm-selection ((t (:background "dim gray"))))
;; '(helm-source-header ((t (:weight bold :height 1.3)))))
;;(setq helm-buffers-fuzzy-matching t
;;      helm-quick-update t)
;;(defun helm-projectile-ag ()
;;  (interactive)
;;  (if (projectile-project-p)
;;      (helm-ag (projectile-project-root))
;;    (helm-ag)))
;;(defun helm-open-vcs-files ()
;;  (interactive)
;;  (if (projectile-project-p)
;;      (helm-projectile)
;;    (helm-other-buffer
;;     '(helm-source-files-in-current-dir
;;       helm-source-recentf
;;       helm-source-buffers-list
;;       helm-source-elscreen)
;;     "*helm-my-buffers*")))
;;(helm-mode t)

;;; ido/smex
(setq-default ido-enable-flex-matching t
              ido-use-faces nil)

(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
(ido-ubiquitous-mode t)
(smex-initialize)

(with-eval-after-load 'ido
  (define-key ido-common-completion-map (kbd "TAB") 'ido-next-match)
  (define-key ido-common-completion-map (kbd "<backtab>") 'ido-prev-match))

;;; autocomplete
(setq-default ac-auto-show-menu 0.1
              ac-auto-start 2
              ac-quick-help-delay 0.3
              ac-quick-help-height 50
              ac-use-fuzzy t
              ac-use-quick-help nil

              ;; indent-guide-delay 0.3  ;; https://github.com/zk-phi/indent-guide/issues/32#issuecomment-185321879

              read-file-name-completion-ignore-case t)

;(ac-config-default)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

(global-set-key [(control tab)] 'hippie-expand)

;;; hippie
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name))

;;; programming

(add-hook
 'prog-mode-hook
 (lambda ()
   (auto-complete-mode)
   (semantic-mode)
   (yas-minor-mode)
   (git-gutter-mode)
   (idle-highlight-mode)
   (projectile-global-mode)))

(add-hook 'after-init-hook #'global-flycheck-mode)

(custom-set-variables
 ;;'(flycheck-checker-error-threshold 1000)
 '(flycheck-display-errors-delay 0))

;;; lisp
(add-to-hooks
 (lambda ()
   (setq indent-tabs-mode nil)
   (define-key emacs-lisp-mode-map "\C-x\C-e" 'pp-eval-last-sexp)
   (push '(?` . ("`" . "'")) evil-surround-pairs-alist))
 '(emacs-lisp-mode-hook lisp-mode-hook))


;;; python

(require 'deferred)

; TODO: evil-search-highlight-persist-remove-all should not remove this
(defun python-highlight-pdb ()
  "Highlight pdb usage."
  (interactive)
  (highlight-regexp "pdb\\.\\(set_trace\\|post_mortem\\|run\\(call\\|ctx\\|eval\\)?\\)([^)\n\r]*)")
  (highlight-regexp "import\\( \\|.*[, ]\\)pdb"))


;(defmacro deferred:compose (first &rest deferreds)
;  "DEFERRED FUNCTIONS."
;  `(let (curr)
;     ,@(cl-loop for d in deferreds
;                with curr = first
;                collect
;                `(setq curr (deferred:set-next curr ,d)))
;     curr))

(defun pyenv-call (&rest args)
  "Call pyenv passing ARGS as positional arguments."
  (eval `(deferred:process "pyenv" ,@args)))


(defun pip-call (&rest args)
  "Call pip passing ARGS as positional arguments."
  (eval `(deferred:process "pip" ,@args)))


(defun pyenv-versions ()
  "Return a list of all existing pyenv virtualenvs."
  (deferred:nextc
    (pyenv-call "versions" "--bare")
    (lambda (x) (split-string x))))

(defun pyenv-version-p (env)
  "Predicate for checking if the pyenv ENV exists."
  (lexical-let ((e env))
    (deferred:nextc
      (pyenv-versions)
      (lambda (x) (seq-contains x e)))))


(defun pyenv-python-27 ()
  "Make sure the version 2.7 is installed in pyenv."
  (deferred:nextc
    (pyenv-version-p "2.7")
    (lambda (x) (if (not x) (pyenv-call "install" "2.7")))))


(defun pyenv-venv-emacs ()
  "Prepare the Emacs venv for use, maybe creating and installing dependencies in it."
  (deferred:nextc
    (pyenv-version-p "emacs")
    (lambda (x)
      (if (not x)
          (deferred:$
            (pyenv-call "virtualenv" "2.7" "emacs")
            (deferred:nextc it (lambda () (pyenv-mode-set "emacs")))
            ;; TODO: run this in parallel
            (deferred:nextc it (lambda () (pip-call "install" "-U" "flake8" "virtualenv")))
            (deferred:nextc it (lambda () (jedi:install-server))))
        (pyenv-mode-set "emacs")))))


(defun pyenv-emacs-with-jedi ()
  "Install dependencies for jedi-mode."
  (deferred:$
    (pyenv-python-27)
    (deferred:nextc it (lambda () (pyenv-venv-emacs)))
    (deferred:nextc it (lambda () (jedi:setup)))))


(setq-default jedi:setup-keys t
              jedi:complete-on-dot t
              key-chord-two-keys-delay 0.3)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; TODO:
;; - test auto-virtualenv
;; - pyenv-mode-auto


;; elscreen
(custom-set-faces
 '(elscreen-tab-background-face ((t nil)))
 '(elscreen-tab-control-face ((t nil)))
 '(elscreen-tab-current-screen-face ((t (:background "dim gray"))))
 '(elscreen-tab-other-screen-face ((t nil))))

(custom-set-variables
 '(elscreen-tab-display-control nil)
 '(elscreen-tab-display-kill-screen nil))

;; evil
(custom-set-faces
 '(evil-search-highlight-persist-highlight-face ((t (:inherit isearch)))))

(custom-set-variables
 '(evil-complete-next-line-func 'hippie-expand)
 '(evil-complete-previous-line-func 'hippie-expand)
 '(evil-cross-lines t)
 '(evil-echo-state nil)
 '(evil-want-C-u-scroll t)
 '(evil-flash-delay 0)
 '(evil-move-beyond-eol nil))


(evil-mode t)
(key-chord-mode 1)
(global-evil-surround-mode t)
(global-evil-leader-mode t)
(global-evil-tabs-mode t) ; elscreen integration
(global-evil-search-highlight-persist t)
(linum-relative-global-mode t)

;; I prefer to use emacs mode since j is bound to open file.
;; (evil-set-initial-state 'dired-mode 'normal)
(evil-set-initial-state 'direx-mode 'normal)
;; Did not work
;; (evil-set-initial-state 'recentf-dialog 'normal)

(evil-define-command evil-insert-paste-after ()
  (evil-normal-state)
  (evil-paste-after 1))

(evil-define-command evil-insert-paste-before ()
  (evil-normal-state)
  (evil-paste-before 1))

(def-keys evil-normal-state-map
    ;;"C-u" 'evil-scroll-up
    "C-<up>" 'keyboard-up
    "C-<dow>" 'keyboard-down
    "C-<right>" 'keyboard-right
    "C-<left>" 'keyboard-left
    "C-j" 'windmove-down
    "C-k" 'windmove-up
    ;;would shadow help "C-h" 'windmove-left
    "C-l" 'windmove-right
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    ;;[escape] 'keyboard-quit
    )

(def-keys evil-visual-state-map
    "j" 'evil-next-line
    "k" 'evil-previous-line
    ;;[escape] 'keyboard-quit
    )

(def-keys evil-insert-state-map
    "M-P" 'evil-insert-paste-before
    "M-p" 'evil-insert-paste-after)

;(define-key evil-motion-state-map ";" 'smex)
(global-set-key [escape] 'evil-exit-emacs-state)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
    "F" 'helm-find-files
    "f" 'helm-open-vcs-files
    "/" 'helm-projectile-ag
    "i" 'helm-semantic-or-imenu
    "k" 'helm-kill-ring
    "m" 'mc/mark-next-like-thi       ; multicursor-next-like-this-force-normal
    "M" 'mc/edit-lines
    "n" 'evil-search-highlight-persist-remove-all
    "q" 'elscreen-kill
    "s" 'split-window-horizontally
    "w" 'save-buffer
    "x" 'helm-M-x)

;;; flymake
;;(require 'use-package)
;;(use-package flymake-coffee :mode "\\.coffee")
;;(use-package flymake-css :mode "\\.css")
;;(use-package flymake-cursor)
;;(use-package flymake-haml :mode "\\.haml")
;;flymake-php
;;(use-package flymake-sass :mode "\\.scss")
;;(use-package flymake-shell)
;;flymake-rust
;;flyphpcs
