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

(setq-default use-package-always-ensure t)
(setq-default use-package-always-defer t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package no-littering
  :demand t)

(use-package diminish
  :hook ((subword-mode . (lambda () (diminish 'subword-mode)))
         (auto-revert-mode . (lambda () (diminish 'auto-revert-mode))))
  :config
  (diminish 'visual-line-mode)
  (diminish 'eldoc-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(defun magit-dont-force-push-to-wrong (orig-fun &rest args)
  "Advice around Magit’s git command execution to prevent dangerous force pushes.

ORIG-FUN is the original Magit function being advised.
ARGS are the arguments passed to the git command.

Specifically:
- For any command other than `push`, this just delegates to ORIG-FUN.
- For `git push`, it inspects whether the call includes a force flag
  (`--force` or `--force-with-lease`).
- If a force push is requested *and* an explicit refspec is given
  (e.g. SRC:DEST), it checks whether SRC and DEST are the same branch
  (ignoring the `refs/heads/` prefix).
- If they differ, the push is rejected and a warning is shown,
  preventing accidental force-pushes to the wrong branch.
- Otherwise, the original push is allowed."
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
  :hook (prog-mode . magit-todos-mode)
  :config
  (add-to-list 'magit-todos-exclude-globs "nob.h"))

(use-package treemacs-magit
  :after treemacs magit)

(use-package git-modes)

(use-package undo-tree
  :diminish 'undo-tree-mode
  :hook ((prog-mode . undo-tree-mode)
         (text-mode . undo-tree-mode))
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-enable-undo-in-region t)
  (undo-tree-visualizer-lazy-drawing t)
  (undo-tree-auto-save-history t)
  :bind (:map undo-tree-visualizer-mode-map
              ("C-x 0" . #'undo-tree-visualizer-quit))
  :config
  (let ((undo-tree-hist (expand-file-name "var/undo-tree-history" user-emacs-directory)))
    (setq-default undo-tree-history-directory-alist
                  `(("." . ,undo-tree-hist)))
    (unless (file-directory-p undo-tree-hist)
      (make-directory undo-tree-hist t))))

(defun eglot-unmanage-buffer ()
  "Force Eglot to stop managing this buffer if it's on the denylist."
  (progn
    (when (and buffer-file-name
               (not (string= "cabal.project.local" (file-name-nondirectory buffer-file-name)))
               (string-match-p "/cabal\\.project\\..*\\'" buffer-file-name)
               (functionp 'eglot--managed-mode-off)
               (functionp 'eglot-managed-p)
               (eglot-managed-p))
      (message "[Eglot] Disabling eglot-managed-mode for: %s" buffer-file-name)
      (eglot--managed-mode-off))
         (editorconfig-apply)))


(defun my-markup-formatter (orig-fun &rest args)
  "Advice to normalize markup output and enable math rendering.

ORIG-FUN is the original formatter function being advised.
ARGS are the arguments passed to ORIG-FUN.

This wrapper does two things:
1. Ensures `markdown-enable-math` is non-nil so that LaTeX-style math
   rendering works in Markdown buffers.
2. Normalizes line endings by converting Windows-style CRLF (\"\\r\\n\")
   and lone carriage returns (\"\\r\") into Unix-style newlines (\"\\n\"),
   which avoids rendering issues in mixed-platform text."
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
                   (schemas . ((https://www.schemastore.org/clang-format.json . "/.clang-format")
                               (https://www.schemastore.org/clangd.json . "/clangd.yaml")))
                   (completion . t)
                   (hover . t))))
  :bind (:map eglot-mode-map
              ("C-c a" . eglot-code-actions)
              ("C-c f" . eglot-format-buffer)
              ("M-."   . xref-find-definitions)
              ("M-,"   . xref-go-back)))

(use-package realgud
  :config
  (defun my-realgud-gdb-wrapper ()
    (interactive)
    (realgud:gdb "gdb"))
  :hook ((c-mode c++-mode) . (lambda () (require 'realgud)))
  :bind ( :map c-mode-map
          ("C-c g" . #'my-realgud-gdb-wrapper)
          :map c++-mode-map
          ("C-c g" . #'my-realgud-gdb-wrapper)))

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
  "Check FILE is included in boring file list."
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
  (treemacs-directory-face ((t (:foreground "#4477dd"))))
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag)
   :map treemacs-mode-map
   ("M-<down>" . nil)
   ("M-<up>" . nil)))

(use-package projectile
  :diminish 'projectile-mode
  :commands projectile-command-map projectile-project-root
  :bind ("C-c C-p" . projectile-command-map)
  :custom
  (projectile-use-git-grep t))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook ((prog-mode . rainbow-mode)
         (conf-mode . rainbow-mode))
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
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-ignore-files '("\\`\\.nfs" "\\`#.*" "\\`.*~"))
  :config
  (ido-mode 1))

(use-package ido-grid-mode
  :defer nil
  :after ido
  :custom
  (ido-grid-mode-jump t)
  (ido-grid-mode-padding " | ")
  :config
  (ido-grid-mode 1))

(use-package flymake
  :config
  (add-to-list 'trusted-content (file-truename user-emacs-directory))
  :hook ((emacs-lisp-mode . flymake-mode)))

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)))

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
  "Start eglot if current major mode is supported and the executable is found."
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

(defun calc-clear-calculations ()
  (interactive)
  (when (and (equal major-mode 'calc-mode)
             (not (equal (calc-stack-size) 0)))
    (calc-pop (calc-stack-size))))

(use-package calc
  :bind (("C-c c" . #'calc)
         :map calc-mode-map
         ("C-c k" . #'calc-clear-calculations)
         ("C-x 0" . #'calc-quit)))

(defun my-subword ()
  (subword-mode 1)
  (setq-local show-trailing-whitespace t))

(use-package markdown-mode
  :hook ((markdown-mode . (lambda () (my-jbeam-try-load-mode 'jbfl-mode)))
         (markdown-mode . my-subword)
         (markdown-mode . my-eglot-ensure-if-supported))
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  :config
  :custom-face
  (markdown-metadata-key-face ((t (:foreground "#ff55ff"))))
  (markdown-metadata-value-face ((t (:foreground "#ffaaff")))))

(use-package fish-mode
  :hook (fish-mode . my-eglot-ensure-if-supported)
  :mode ("\\.fish\\'" . fish-mode))

(use-package dockerfile-mode
  :custom
  (dockerfile-enable-auto-indent t))

(use-package docker
  :ensure t
  :bind (("C-c C-d" . docker)
         ("C-c M-d" . docker-compose)))

(use-package apache-mode
  :mode (("/\\.htaccess\\'" . apache-mode)))

(use-package php-mode
  :hook ((php-mode . my-subword)))

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
  "Load FILENAME in this config."
  (let ((filepath (concat (expand-file-name (file-name-directory user-init-file)) filename)))
    (load filepath)))

(load-config-file "extra-enabled/haskell-mode-git.el")
(load-config-file "misc.el")

(load-theme 'cozy-pink t)

(provide 'init)
;;; init.el ends here
