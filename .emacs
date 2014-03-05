;; Standard emacs configurations (uptop because I want these configs applied before el-get blocks)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(savehist-mode t)

(setq-default inhibit-startup-screen t
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
              use-dialog-box nil)

(setq debug-on-error t)
(setq stack-trace-on-error t)

(defun add-elpa-repositories () "Configure ELPA repos" (interactive)
  (progn
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
    (add-to-list 'package-archives '("SC" . "http://joseito.republika.pl/sunrise-commander/") t)))

;; must be set before the library is loaded
(setq linum-format "%d ")

;; replaced by el-get

;; el-get sets package-archive on the :post-init for package.el and initialize it
;;(require 'package)
;;(add-elpa-repositories)
;;(package-initialize)

;;(require 'cask "~/.emacs.d/cask/cask.el")
;;(cask-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(setq el-get-verbose t)
;; jedi recipe is available only on master [04/12/2113]
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; see: el-get/el-get-custom.el
(setq el-get-sources
      '((:name package :after
               (progn
                 (add-elpa-repositories)
                 (if (not (or
                            (file-exists-p "~/.emacs.d/el-get/package/elpa/")
                            (file-exists-p "~/.emacs.d/elpa/")))
                     (package-refresh-contents))))
        (:name evil-relative-linum :localname "evil-relative-linum" :type http
               :depends linum+ :url
               "https://raw.github.com/tarao/evil-plugins/master/evil-relative-linum.el")
        (:name evil :depends (undo-tree auto-complete))
        (:name auto-complete :load ("auto-complete.el" "auto-complete-config.el")
               :after (progn (global-auto-complete-mode t)))
        ;; jedi does not work with IPv6 (when /etc/hosts alias localhost host to ::1)
        (:name jedi
               :before (progn (setq jedi:server-args '("--address" "127.0.0.1"))))
        (:name elscreen :type elpa)))


;; Order matters!
(setq my-el-get-packages
    (append
      '(ag
        auto-complete
        ;;auto-complete-clang-async
        coffee-mode
        css-mode
        deferred
        django-mode
        evil
        evil-surround
        flymake
        flymake-coffee
        flymake-css
        flymake-cursor
        flymake-haml
        ;;flymake-php
        flymake-sass
        flymake-shell
        ;;flymake-rust
        ;;flyphpcs
        full-ack
        key-chord
        markdown-mode
        magit
        multiple-cursors
        ;;rust-mode
        smex
        yasnippet)
       (mapcar 'el-get-source-name el-get-sources)))

;; if gnutls complains, wait for it (the emacs wiki recepies are being downloaded)
;; needs to be sync'ed!
;; (setq el-get-is-lazy t)
(el-get 'sync my-el-get-packages)

;; mini modes
(evil-mode 1)
(key-chord-mode 1)
(elscreen-start)
(global-linum-mode)
(show-paren-mode t)

;; evil mode alias
(evil-ex-define-cmd "tabnew" 'elscreen-create)
(evil-ex-define-cmd "tabe[dit]" 'elscreen-find-file)
(evil-ex-define-cmd "q[uit]" 'elscreen-kill)
(define-key evil-normal-state-map "gt" 'elscreen-next)
(define-key evil-normal-state-map "gT" 'elscreen-previous)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-<up>") 'keyboard-up)
(define-key evil-normal-state-map (kbd "C-<dow>") 'keyboard-down)
(define-key evil-normal-state-map (kbd "C-<right>") 'keyboard-right)
(define-key evil-normal-state-map (kbd "C-<left>") 'keyboard-left)
(define-key evil-visual-state-map "jk" 'evil-normal-state)
(define-key evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define-global "jk" 'evil-normal-state)

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:setup)
;; called by jedi:setup: (add-hook 'python-mode-hook 'jedi:ac-setup)
;; (add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (defalias 'yes-or-no-p 'y-or-no-p)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
       (local-file (file-relative-name
            temp-file
            (file-name-directory buffer-file-name))))
      (list "pycheckers.sh"  (list local-file))))
   (add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pyflakes-init)))
