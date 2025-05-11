(add-to-list 'load-path "~/.config/emacs/opt/haskell-mode")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.config/emacs/opt/haskell-mode")

(defun run-hlint ()
  "Run  hlint over the current project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "hlint .")))

(add-hook 'haskell-mode-hook (lambda () (progn (haskell-collapse-mode 1)
                                               (haskell-doc-mode 1)
                                               (haskell-indent-mode 1)
                                               (haskell-decl-scan-mode -1)
                                               (interactive-haskell-mode 1)
                                               (diminish haskell-collapse-mode)
                                               (diminish  haskell-doc-mode)
                                               (diminish haskell-indent-mode)
                                               (diminish interactive-haskell-mode)
					       (define-key haskell-mode-map (kbd "C-c l") 'run-hlint)
					       (define-key haskell-mode-map (kbd "C-c a") 'haskell-command-insert-language-pragma))))

(setq haskell-process-args-stack-ghci '("--ghc-options" "-w"))
(setq haskell-tags-on-save t)
(setq hindent-reformat-buffer-on-save t)
(setq haskell-process-show-debug-tips t)
(setq haskell-doc-prettify-types t)

