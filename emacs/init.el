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

(defun magit-dont-force-push-to-wrong (orig-fun &rest args)
  (let ((cmd (car args)))
    (if (not (equal cmd "push"))
        (apply orig-fun args)
      (let ((is-force
             (seq-some (lambda (x)
                         (and (listp x)
                              (or (member "--force-with-lease" x)
                                  (member "--force" x))))
                       args))
            (push-spec nil))
        (cl-loop for x in args
                 when (and (stringp x)
                           (string-match "\\`\\([^:]+\\):\\([^:]+\\)\\'" x))
                 do (progn (setq push-spec x)
                           (cl-return push-spec)))
        (if (and is-force push-spec)
            (let* ((src (match-string 1 push-spec))
                   (dst (match-string 2 push-spec))
                   (normalize (lambda (branch)
                                (replace-regexp-in-string
                                 "\\`refs/heads/" "" branch)))
                   (src-norm (funcall normalize src))
                   (dst-norm (funcall normalize dst)))
              (if (not (string= src-norm dst-norm))
                  (message "Refusing to force-push from %s to different remote branch %s"
                           src dst)
                (apply orig-fun args)))
          (apply orig-fun args))))))

(use-package magit
  :commands (magit-status magit-blame magit-log)
  :custom
  (magit-completing-read-function #'ido-completing-read)
  :config
  (advice-add 'magit-run-git-async :around #'magit-dont-force-push-to-wrong)
  :custom-face
  (magit-section-heading ((t (:foreground "#ff55ff"))))
  (magit-log-author ((t (:foreground "#ff55ff")))))


(use-package magit-todos
  :after magit
  :hook (haskell-mode . magit-todos-mode))

(use-package treemacs-magit
  :after treemacs magit)

(use-package git-modes)

(use-package undo-tree
  :diminish 'undo-tree-mode
  :hook ((prog-mode . undo-tree-mode)
         (text-mode . undo-tree-mode))
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-lazy-drawing t)
  (undo-tree-auto-save-history t)
  (undo-tree-save-history t))

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
         (sh-mode . my-eglot-ensure-if-supported)
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
           ((haskell-mode haskell-cabal-mode) . ("my_hls_wrapper"))
           (fish-mode . ("fish-lsp" "start")))))
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

(use-package realgud
  :config
  (defun my-realgud-gdb-wrapper ()
    (interactive)
    (realgud:gdb "gdb"))
  :hook ((c-mode c++-mode) . (lambda () (require 'realgud)))
  :bind ( (:map c-mode-map
                ("C-c g" . #'my-realgud-gdb-wrapper))
          (:map c++-mode-map
                ("C-c g" . #'my-realgud-gdb-wrapper))))

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
  :custom-face
  (treemacs-root-face ((t (:foreground "#ffaaff"))))
  (treemacs-directory-face ((t (:foreground "#ff55ff"))))
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
  :hook (prog-mode . rainbow-mode)
  :custom
  (rainbow-x-colors nil))

(use-package rainbow-delimiters
  :hook ((haskell-mode lisp-data-mode) . rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "#FF6F61")))) ; Coral red
  (rainbow-delimiters-depth-2-face ((t (:foreground "#6B5B95")))) ; Purple
  (rainbow-delimiters-depth-3-face ((t (:foreground "#88B04B")))) ; Olive green
  (rainbow-delimiters-depth-4-face ((t (:foreground "#F7CAC9")))) ; Light pink
  (rainbow-delimiters-depth-5-face ((t (:foreground "#92A8D1")))) ; Soft blue
  (rainbow-delimiters-depth-6-face ((t (:foreground "#955251")))) ; Dark rose
  (rainbow-delimiters-depth-7-face ((t (:foreground "#B565A7")))) ; Magenta
  (rainbow-delimiters-depth-8-face ((t (:foreground "#009B77")))) ; Teal green
  (rainbow-delimiters-depth-9-face ((t (:foreground "#DD4124")))) ; Red-orange
  )

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

(defun my-eglot-ensure-if-supported ()
  "Start eglot if current major mode is supported and the language server executable is found."
  (message "[eglot-debug] Running in mode: %s" major-mode)
  (unless (featurep 'eglot)
    (require 'eglot))
  (when-let ((server-entry
              (cl-find-if
               (lambda (entry)
                 (let ((modes (if (listp (car entry)) (car entry) (list (car entry)))))
                   (memq major-mode modes)))
               eglot-server-programs)))
    (let* ((server-cmd (cdr server-entry))
           (cmd (if (functionp server-cmd)
                    (funcall server-cmd nil)
                  server-cmd))
           (executable
            (if (and (listp cmd)
                     (stringp (car cmd)))
                (car cmd))))
      (if (and executable (executable-find executable))
          (progn
            (message "[eglot-debug] Found executable '%s', starting eglot" executable)
            (eglot-ensure))
        (message "[eglot-debug] Executable '%s' not found, not starting eglot" executable)))))


(use-package markdown-mode
  :hook (markdown-mode . my-eglot-ensure-if-supported)
  :mode ("\\.md\\'" . markdown-mode)
  :custom-face
  (markdown-metadata-key-face ((t (:foreground "#ff55ff"))))
  (markdown-metadata-value-face ((t (:foreground "#ffaaff")))))

(use-package fish-mode
  :hook (fish-mode . my-eglot-ensure-if-supported)
  :mode ("\\.fish\\'" . fish-mode))

(use-package php-mode)

(use-package yaml-mode
  :hook (yaml-mode . my-eglot-ensure-if-supported)
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
