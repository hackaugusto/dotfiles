(require 'cask "~/.cask/cask.el")
(cask-initialize)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(savehist-mode t)

(defun linum-format-func (line)
  (concat
    (propertize (format linum-format-fmt line) 'face 'linum)
    (propertize " " 'face 'mode-line)))
(setq linum-format 'linum-format-func)

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
              use-dialog-box nil
              show-paren-delay 0)

(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'linum-before-numbering-hook
  (lambda ()
    (setq-local linum-format-fmt
      (let ((width (length (number-to-string (count-lines (point-min) (point-max))))))
        (concat "%" (number-to-string width) "d")))))

(require 'evil)
(setq evil-leader/in-all-states 1)
(show-paren-mode t)
(evil-mode 1)
(global-evil-search-highlight-persist t)
(global-evil-tabs-mode t)
(global-relative-line-numbers-mode t)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(evil-leader/set-key "n" 'evil-search-highlight-persist-remove-all)

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-<up>") 'keyboard-up)
(define-key evil-normal-state-map (kbd "C-<dow>") 'keyboard-down)
(define-key evil-normal-state-map (kbd "C-<right>") 'keyboard-right)
(define-key evil-normal-state-map (kbd "C-<left>") 'keyboard-left)
(define-key evil-visual-state-map "jk" 'evil-normal-state)
(define-key evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define-global "jk" 'evil-normal-state)
