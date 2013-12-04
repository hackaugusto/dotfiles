;; standard emacs configurations (uptop because I want these configs applied before el-get blocks)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
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
;; jedi recipe is available only on master [04/12/2113]
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-sources
      '((:name package :post-init
               (progn
                 (add-elpa-repositories)
                 (if (not (file-exists-p "~/.emacs.d/elpa/"))
                     (package-refresh-contents))))
        (:name evil-relative-linum :localname "evil-relative-linum" :type http
               :depends linum+ :url
               "https://raw.github.com/tarao/evil-plugins/master/evil-relative-linum.el")
        (:name elscreen :type elpa)))


;; Order matters!
(setq my-el-get-packages
    (append
      '(ag
        auto-complete
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
        flymake-sass
        flymake-shell
        ;;flyphpcs
        jedi
        key-chord
        markdown-mode
        magit
        multiple-cursors
        smex
        yasnippet)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-el-get-packages)

;; mini modes
(evil-mode 1)
(key-chord-mode 1)
(elscreen-start)
(global-linum-mode)
(show-paren-mode t)
(savehist-mode t)

;; evil mode alias
(evil-ex-define-cmd "tabnew" 'elscreen-create)
(evil-ex-define-cmd "tabe[dit]" 'elscreen-find-file)
(evil-ex-define-cmd "q[uit]" 'elscreen-kill)
(define-key evil-normal-state-map "gt" 'elscreen-next)
(define-key evil-normal-state-map "gT" 'elscreen-previous)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
;;(define-key evil-visual-state-map "jk" 'evil-normal-state)
;;(define-key evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define-global "jk" 'evil-normal-state)

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (defalias 'yes-or-no-p 'y-or-no-p)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
