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


;; ----------------
;; parse the cabal file to set cabal repl args in case .dir-locals.el wasnt found
;; ----------------
(defun cabal--parse-components (cabal-file)
  "Return an alist of (COMPONENT . SOURCE-DIRS) from CABAL-FILE.
COMPONENT is a string like 'library' or 'executable foo'."
  (with-temp-buffer
    (insert-file-contents cabal-file)
    (goto-char (point-min))
    (let (result current-comp)
      (while (not (eobp))
        (when (looking-at "^\\(library\\|executable\\|test-suite\\)\\(?:\\s-+\\([^ \n]+\\)\\)?")
          (setq current-comp
                (if (match-string 2)
                    (cons (match-string 1) (match-string 2))
                  (match-string 1))))
        (when (and current-comp
                   (looking-at "^\\s-+hs-source-dirs:\\s-*\\(.*\\)"))
          (let ((dirs (split-string (match-string 1) "[ \t]+" t)))
            (push (cons current-comp dirs) result)))
        (forward-line 1))
      (nreverse result))))

(defun cabal--get-project-name ()
  "Return the `name` field from the `.cabal` file of the current project."
  (let* ((project-root (projectile-project-root))
         (cabal-file (car (directory-files project-root t "\\.cabal\\'"))))
    (when cabal-file
      (with-temp-buffer
        (insert-file-contents cabal-file)
        (goto-char (point-min))
        (when (re-search-forward "^name:\\s-*\\(.*\\)$" nil t)
          (string-trim (match-string 1)))))))

(defun get-cabal-target-for-buffer ()
  "Return the Cabal component of the current buffer (library, executable:foo, etc.)."
  (let* ((project-root (projectile-project-root))
         (current-file-name (file-relative-name (buffer-file-name) project-root))
         (cabal-file (car (directory-files project-root t "\\`[^.#][A-Za-z0-9_.-]*\.cabal\\'")))
         (project-name (cabal--get-project-name)))
    (when cabal-file
      (let ((components (cabal--parse-components cabal-file))
            (result nil))
        (dolist (comp components)
          (let* ((name (car comp))
                 (dirs (cdr comp)))
            (when (cl-some (lambda (dir)
                             (string-prefix-p dir current-file-name))
                           dirs)
              (setq result
                    (cond
                     ((string-match-p "\\`library" (car name))
                      (concat "lib:" project-name))
                     ((string-match-p "\\`executable" (car name))
                      (concat project-name ":exe:" (cdr name)))
                     ((string-match-p "\\`test-suite" (car name))
                      (concat project-name ":test:" (cdr name)))
                     (t nil))))))
        result))))

(defun my-haskell-add-cabal-target-arg (orig-fun session hptype)
  "Advice around `haskell-process-compute-process-log-and-command' to add
an extra argument to `haskell-process-args-cabal-repl' dynamically."
  (if (eq hptype 'cabal-repl)
      (let* ((target (get-cabal-target-for-buffer))
             (haskell-process-args-cabal-repl
              (if target
                  (cons target haskell-process-args-cabal-repl)
                haskell-process-args-cabal-repl)))
        (funcall orig-fun session hptype))
    (funcall orig-fun session hptype)))

(advice-add 'haskell-process-compute-process-log-and-command :around
            #'my-haskell-add-cabal-target-arg)
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
