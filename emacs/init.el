;;; package --- Summary: init file
;;; Commentary:
;;; Code:

;; set packages
(setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))

(require 'package)

(setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package no-littering
  :demand t)

(use-package diminish
  :config
  (diminish 'auto-revert-mode)
  (diminish 'rainbow-mode)
  (diminish 'eldoc-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package magit
  :commands (magit-status magit-blame magit-log))

(use-package git-modes)

(use-package undo-tree
  :diminish 'undo-tree-mode
  :hook ((prog-mode . undo-tree-mode)
         (text-mode . undo-tree-mode))
  :custom
  (undo-tree-auto-save-history t))

(defun eglot-unmanage-buffer ()
  "Force Eglot to stop managing this buffer if it's on the denylist."
  (when (and buffer-file-name
             (not (string= "cabal.project.local" (file-name-nondirectory buffer-file-name)))
             (string-match-p "/cabal\\.project\\..*\\'" buffer-file-name)
             (eglot-managed-p))
    (message "[Eglot] Disabling eglot-managed-mode for: %s" buffer-file-name)
    (eglot--managed-mode-off)))

(defun my-markup-formatter (orig-fun &rest args)
  "Enable math rendering and replace carriage returns with newlines"
  (let ((markdown-enable-math t))
    (let* ((raw (apply orig-fun args))
           (normalized (string-replace "\r\n" "\n" raw))
           (final (string-replace "\r" "\n" normalized)))
      final)))

(use-package eglot
  :hook (((c-mode c++-mode haskell-mode haskell-cabal-mode) . eglot-ensure)
         (eglot-managed-mode . eglot-unmanage-buffer))
  :custom
  (eglot-events-buffer-config '(:format lisp))
  (eglot-autoshutdown t)
  (eglot-autoreconnect t)
  (eglot-extend-to-xref t)
  (xref-backend-functions '(eglot-xref-backend xref-etags-backend))
  (tags-revert-without-query t)
  (large-file-warning-threshold nil)
  (eldoc-idle-delay 0.5)
  :config
  (let ((my-eglot-server-programs
         '(((c-mode c++-mode) . ("clangd"))
           ((haskell-mode haskell-cabal-mode) . ("my-hls-wrapper"))
           ((yaml-mode) . ("yaml-language-server" "--stdio")))))
    (dolist (new my-eglot-server-programs)
      (let* ((new-modes (if (listp (car new)) (car new) (list (car new)))))
        (setq eglot-server-programs
              (cl-remove-if
               (lambda (existing)
                 (let ((existing-modes (if (listp (car existing)) (car existing) (list (car existing)))))
                   (cl-some (lambda (mode) (memq mode existing-modes)) new-modes)))
               eglot-server-programs))
        (push new eglot-server-programs))))
  (advice-add 'eglot--format-markup :around #'my-markup-formatter)
  (setq-default eglot-workspace-configuration
                '((haskell (formattingProvider . "fourmolu")
                           (plugin (fourmolu (config (external . t)))))
                  (yaml
                   (schemas . ((https://www.schemastore.org/clang-format.json . "/.clang-format")))
                   (completion . t)
                   (hover . t))))
  :bind (:map eglot-mode-map
              ("C-c a" . eglot-code-actions)
              ("M-."   . xref-find-definitions)
              ("M-,"   . xref-pop-marker-stack)))

(use-package corfu
  :hook ((prog-mode . corfu-mode)
         (text-mode . corfu-mode))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary nil))

(define-inline treemacs-hide-boring-files (file _)
  ""
  (declare (side-effect-free t) (pure t))
  (inline-letevals (file)
    (inline-quote
     (let ((filename (file-name-nondirectory ,file)))
       (cl-some (lambda (ignored-file) (string= filename ignored-file)) '("TAGS" "dist-newstyle" ".stack-work"))))))

(use-package treemacs
  :custom
  (treemacs-no-png-images t)
  :config
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-hide-boring-files)
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
  :commands projectile-command-map projectile-project-root
  :bind ("C-c C-p" . projectile-command-map)
  :custom
  (projectile-use-git-grep t))

(use-package rainbow-mode
  :hook prog-mode
  :custom
  (rainbow-x-colors nil))

(use-package rainbow-delimiters
  :hook ((haskell-mode lisp-data-mode) . rainbow-delimiters-mode)
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#FF6F61")))) ; Coral red
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#6B5B95")))) ; Purple
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#88B04B")))) ; Olive green
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#F7CAC9")))) ; Light pink
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#92A8D1")))) ; Soft blue
   '(rainbow-delimiters-depth-6-face ((t (:foreground "#955251")))) ; Dark rose
   '(rainbow-delimiters-depth-7-face ((t (:foreground "#B565A7")))) ; Magenta
   '(rainbow-delimiters-depth-8-face ((t (:foreground "#009B77")))) ; Teal green
   '(rainbow-delimiters-depth-9-face ((t (:foreground "#DD4124")))) ; Red-orange
   ))

(use-package ido
  :defer nil
  :custom
  (ido-enable-flex-matching t
   ido-everywhere t
   ido-ignore-files '("\\`\\.nfs" "\\`#.*" "\\`.*~"))
  :config
  (ido-mode 1))

;; languages

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

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode))

(use-package php-mode)

(defun my-yaml-eglot-setup ()
  (when (executable-find "yaml-language-server")
    (eglot-ensure)))

(use-package yaml-mode
  :hook (yaml-mode . my-yaml-eglot-setup)
  :mode (("\\.ya?ml\\'" . yaml-mode)
         ("/\\.clang-format\\'" . yaml-mode)
         ("/stack\\.yaml\\.lock\\'" . yaml-mode)))

(use-package vimrc-mode)

(use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode))

;; load my other files
(defun load-config-file (filename)
  "load file name in this config"
  (let ((filepath (concat (expand-file-name (file-name-directory user-init-file)) filename)))
    (load filepath)))

(load-config-file "extra-enabled/haskell-mode-git.el")
(load-config-file "misc.el")

(load-theme 'cozy-pink t)
