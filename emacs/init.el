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
  :hook (magit-status . (lambda () (which-function-mode 0))))

(use-package git-modes)

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history 5)
  :diminish 'undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package eglot
  :ensure t
  :hook ((c-mode c++-mode haskell-mode) . eglot-ensure)
  :custom
  (eglot-extend-to-xref t)
  :config
  (setq eglot-server-programs
        (cl-remove-if (lambda (entry)
                        (eq 'haskell-mode (car entry)))
                      eglot-server-programs))
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs
               '((haskell-mode) . ("my-hls-wrapper")))
  (setq-default eglot-workspace-configuration
                '((haskell (formattingProvider . "fourmolu")
                           (plugin (fourmolu (config (external . t)))))))
  (setq xref-backend-functions '(eglot-xref-backend xref-etags-backend))
  (setq tags-revert-without-query t
        xref-etags-mode t
        large-file-warning-threshold nil
        eldoc-idle-delay 0.5)
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c a" . eglot-code-actions)
              ("M-."   . xref-find-definitions)
              ("M-,"   . xref-pop-marker-stack)))

(use-package fish-mode
  :mode (("\\.fish$" . fish-mode)))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary nil)
  :init
  (global-corfu-mode))

(use-package php-mode
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode (("\\.ya?ml$" . yaml-mode)
         ("/stack\\.yaml\\.lock\\'" . yaml-mode)))

;; some time I may use haskell from repo again but for now I will be compiling
;;
;; (use-package haskell-mode
;;   :mode (("\\.hs$" . haskell-mode))
;;   :hook (;; (haskell-mode-hook . #'haskell-collapse-mode)
;;          ;; (haskell-mode-hook . #'haskell-doc-mode)
;;          (haskell-mode-hook . #'haskell-indent-mode)
;;          (haskell-mode-hook . #'interactive-haskell-mode))
;;   :config
;;   '((haskell-tags-on-save t)
;;     (hindent-reformat-buffer-on-save t)
;;     (haskell-process-show-debug-tips)
;;     (haskell-doc-prettify-types t))
;;   :diminish 'haskell-doc-mode)

(define-inline treemacs-hide-tags (file _)
  ""
  (declare (side-effect-free t) (pure t))
  (inline-letevals (file)
    (inline-quote
     (string= (file-name-nondirectory ,file) "TAGS"))))

(use-package treemacs
  :init
  (setq treemacs-no-png-images t)
  :config
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-hide-tags)
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

(use-package hindent)

(use-package tuareg
  :mode (("\\.ml$" . tuareg-mode)))

(defun load-config-file (filename)
  "load file name in this config"
  (let ((filepath (concat (expand-file-name (file-name-directory user-init-file)) filename)))
    (load filepath)))

(load-config-file "extra-enabled/haskell-mode-git.el")
(load-config-file "misc.el")

(load-theme 'cozy-pink t)
