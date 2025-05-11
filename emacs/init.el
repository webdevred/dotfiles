;;; package --- Summary: init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; set packages
(setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))

(require 'package)

(setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))


(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package no-littering
  :ensure t
  :demand t)

(use-package diminish
  :init
  (diminish 'auto-revert-mode)
  (diminish 'rainbow-mode)
  (diminish 'eldoc-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package magit
  :ensure t
  :hook (magit-status . (lambda () (which-function-mode 0))))

(use-package git-modes
  :ensure t)

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history 5)
  :diminish 'undo-tree-mode
  :init
  (global-undo-tree-mode))

(defun my-lsp-prog-hook ()
  "Run `lsp-deferred` in programming modes except `jbeam-mode`."
  (unless (eq major-mode 'jbeam-mode)
    (lsp-deferred)))

(use-package lsp-mode
  :hook ((prog-mode . my-lsp-prog-hook)
         (save-buffer . (lambda () (when (lsp-workspaces) (lsp-restart-workspace)))))
  :custom
  (lsp-auto-guess-root t)
  (lsp-warn-no-matched-clients nil)
  :commands lsp lsp-deferred)

(use-package php-mode)

(use-package yaml-mode
  :mode (("\\.ya?ml$" . yaml-mode)))

(use-package company
  :after lsp-mode
  :diminish 'company-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package treemacs
  :init
  (setq treemacs-no-png-images t)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package projectile
  :diminish 'projectile-mode
  :bind ("C-c C-p" . projectile-command-map)
  :init
  (projectile-mode +1)
  (setq projectile-use-git-grep t))

(use-package which-func
  :init
  (which-function-mode 1)
  :hook (magit-status . (lambda () (which-function-mode 0))))

(use-package rainbow-mode
  :hook prog-mode)

(use-package ido
  :config
  (setq
   ido-enable-flex-matching t
   ido-everywhere t
   ido-ignore-files '("\\`\\.nfs" "\\`#.*" "\\`.*~"))
  :init
  (ido-mode 1))

(defun load-config-file (filename)
  "load file name in this config"
  (let ((filepath (concat (expand-file-name (file-name-directory user-init-file)) filename)))
    (load filepath)))

(load-config-file "extra-enabled/haskell-mode-git.el")
(load-config-file "misc.el")

(load-theme 'cozy-pink t)
