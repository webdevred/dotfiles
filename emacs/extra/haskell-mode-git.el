(add-to-list 'load-path "~/.config/emacs/opt/haskell-mode")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.config/emacs/opt/haskell-mode")

(defun haskell-run-hlint ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "hlint .")))

(defun compile-with-haskell-process-type (fun)
  (let ((stack-or-cabal (haskell-process-type))
        (compilation-environment '("TERM=xterm-256color"))
        (default-directory (projectile-project-root)))
    (if (eq stack-or-cabal 'ghci)
        (message "no project directory found")
      (compile (funcall fun stack-or-cabal)))))

(defun haskell-run-tests ()
  (interactive)
  (compile-with-haskell-process-type
   (lambda (process-type)
     (if (eq process-type 'stack-ghci)
         "stack test --color always"
       "cabal test --test-show-details=direct"))))

(defun my-haskell-mode-setup ()
  (haskell-collapse-mode 1)
  (haskell-doc-mode 1)
  (haskell-indent-mode 1)
  (haskell-decl-scan-mode -1)
  (interactive-haskell-mode 1))

(add-hook 'haskell-mode-hook #'my-haskell-mode-setup)

(defvar my-haskell-dir-locals-last-dir nil
  "Holds the last directory for which Haskell dir-locals were applied for the current buffer.")

(defun my-haskell-apply-dir-locals ()
  "Apply .dir-locals.el automatically for Haskell buffers when switching buffers."
  (when (and buffer-file-name
             (string-match-p "\\.hs\\'" buffer-file-name)
             (not (minibufferp)))
    (let ((current-dir (file-name-directory buffer-file-name)))
      (unless (equal current-dir my-haskell-dir-locals-last-dir)
        (unless (bound-and-true-p my-haskell-applying-dir-locals)
          (let ((my-haskell-applying-dir-locals t))
            (require 'haskell-mode)
            (unless (derived-mode-p 'haskell-mode)
              (haskell-mode))
            (hack-local-variables t)
            (when (and (bound-and-true-p my-haskell-dir-locals-last-dir) (haskell-session-maybe) (fboundp 'haskell-process-restart))
              (haskell-process-restart))
            (setq my-haskell-dir-locals-last-dir current-dir)))))))

(add-hook 'buffer-list-update-hook #'my-haskell-apply-dir-locals)

(add-to-list 'safe-local-variable-values '(haskell-process-type . cabal-repl))
(defun cabal-args-are-safe (val)
  (and (listp val)
       (= (length val) 3)
       (let ((func (car val))
             (variable-name (cadr val))
             (variable-val (caddr val)))
         (and (eq func 'setq-local)
              (eq variable-name 'haskell-process-args-cabal-repl)
              (and (listp variable-val)
                   (= (length variable-val) 3))
              (let ((append-func (car variable-val))
                    (new-args (cadr variable-val))
                    (old-args (caddr variable-val)))
                (and (eq append-func 'append)
                     (let ((args (if (and (listp new-args)
                                          (= 2 (length new-args))
                                          (equal (car new-args) 'quote))
                                     (cadr new-args)
                                   new-args)))
                       (and (listp args)
                            (cl-every
                             (lambda (s)
                               (and (stringp s)
                                    (not (string-match-p "[;&|`$<>]" s))))
                             args)))
                     (eq old-args 'haskell-process-args-cabal-repl)))))))

(put 'eval 'safe-local-variable #'cabal-args-are-safe)

(with-eval-after-load 'diminish
  (dolist (mode '(haskell-collapse-mode haskell-doc-mode haskell-indent-mode interactive-haskell-mode))
    (diminish mode)))

(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c l") #'haskell-run-hlint)
  (define-key haskell-mode-map (kbd "C-c t") #'haskell-run-tests)
  (define-key haskell-mode-map (kbd "C-c C-a") #'haskell-command-insert-language-pragma))

(setq haskell-process-args-stack-ghci '("--ghc-options" "-w")
      haskell-process-show-debug-tips t
      haskell-doc-prettify-types t
      haskell-indentation-electric-flag t)
