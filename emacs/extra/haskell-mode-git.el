(add-to-list 'load-path "~/.config/emacs/opt/haskell-mode")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.config/emacs/opt/haskell-mode")

(defun haskell-run-hlint ()
  "Run  hlint over the current project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "hlint .")))

(defun compile-with-haskell-process-type (fun)
  "Takes a function and applies it in the context of the proper process type and XTerm color."
  (let ((stack-or-cabal (haskell-process-type))
        (compilation-environment '("TERM=xterm-256color")))
    (if (eq stack-or-cabal 'ghci)
        (message "no project directory found")
      (compile (funcall fun stack-or-cabal)))))

(defun haskell-run-tests ()
  "Run tests in compilation mode with nice colors."
  (interactive)
  (compile-with-haskell-process-type
   (lambda (process-type)
     (if (eq process-type 'stack-ghci)
         "stack test --color always"
       "cabal test"))))

(defun my-haskell-mode-setup ()
  (haskell-collapse-mode 1)
  (haskell-doc-mode 1)
  (haskell-indent-mode 1)
  (haskell-decl-scan-mode -1)
  (interactive-haskell-mode 1)

  (with-eval-after-load 'diminish
    (diminish 'haskell-collapse-mode)
    (diminish 'haskell-doc-mode)
    (diminish 'haskell-indent-mode)
    (diminish 'interactive-haskell-mode))

  (hindent-mode 1)

  (define-key haskell-mode-map (kbd "C-c l") #'haskell-run-hlint)
  (define-key haskell-mode-map (kbd "C-c t") #'haskell-run-tests)
  (define-key haskell-mode-map (kbd "C-c a") #'haskell-command-insert-language-pragma)
  (add-hook 'before-save-hook #'hindent-reformat-buffer nil t))

(add-hook 'haskell-mode-hook #'my-haskell-mode-setup)

(setq haskell-process-args-stack-ghci '("--ghc-options" "-w"))
(setq haskell-tags-on-save t)
(setq hindent-reformat-buffer-on-save t)
(setq haskell-process-show-debug-tips t)
(setq haskell-doc-prettify-types t)

(setq haskell-indentation-electric-flag t)
