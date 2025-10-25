;;; package --- Summary: haskell mode configuration
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.config/emacs/opt/haskell-mode")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.config/emacs/opt/haskell-mode")

;; ----------------
;; some settings
;; ----------------
(defun my-haskell-mode-setup ()
  (haskell-collapse-mode 1)
  (haskell-doc-mode 1)
  (haskell-indent-mode 1)
  (haskell-decl-scan-mode 1)
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
                (if (and (match-string 2) (not (string-suffix-p ":" (match-string 2))))
                    (cons (match-string 1) (match-string 2))
                  (cons (match-string 1) nil))))
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
                     ((and (string-match-p "\\`library" (car name)) (eq (cdr name) nil))
                      (concat "lib:" project-name))
                     ((string-match-p "\\`library" (car name))
                      (concat project-name ":lib:" (cdr name)))
                     ((string-match-p "\\`executable" (car name))
                      (concat project-name ":exe:" (cdr name)))
                     ((string-match-p "\\`test-suite" (car name))
                      (concat project-name ":test:" (cdr name)))
                     (t nil))))))
        result))))

;; ----------------
;; apply my .dir-locals.el after switching to a haskell buffer
;; ----------------
(defvar my-haskell-dir-locals-last-dir nil
  "Holds the last directory for which Haskell dir-locals were applied for the current buffer.")

(defun filter-and-insert-target (target strings)
  "Filter STRINGS matching pattern [a-z]+:.
If exactly one match equals TARGET, return STRINGS unchanged.
Otherwise, remove all matching strings and prepend TARGET if it's a string."
  (let* ((pattern-pred (lambda (s) (and (stringp s) (string-match-p "^[a-z]+:" s))))
         (matches (cl-remove-if-not pattern-pred strings))
         (filtered (cl-remove-if pattern-pred strings)))
    (cond
     ((and (= (length matches) 1)
           (string= (car matches) target))
      strings)
     ((stringp target)
      (cons target filtered))
     (t
      filtered))))

(defun my-haskell-apply-dir-locals ()
  "Apply .dir-locals.el automatically for Haskell buffers when switching buffers."
  (when (and buffer-file-name
             (string-match-p "\\.hs\\'" buffer-file-name)
             (not (minibufferp)))
    (let ((current-dir (file-name-directory buffer-file-name)))
      (unless (equal current-dir my-haskell-dir-locals-last-dir)
        (unless (bound-and-true-p my-haskell-applying-dir-locals)
          (let ((my-haskell-applying-dir-locals t))
            (unless (derived-mode-p 'haskell-mode)
              (haskell-mode))
            (if (locate-dominating-file
                 default-directory
                 (lambda (d)
                   (cl-find-if
                    (lambda (f) (string-match-p "\\.dir-locals\\.el\\'" f))
                    (directory-files d))))
                (hack-local-variables t)
              (let ((target (get-cabal-target-for-buffer)))
                (setq-local haskell-process-args-cabal-repl (filter-and-insert-target target haskell-process-args-cabal-repl))))
            (message "cabal repl args: %S" haskell-process-args-cabal-repl)
            (when (and
                   (bound-and-true-p my-haskell-dir-locals-last-dir)
                   (or (haskell-session-maybe)
                       (haskell-session-from-buffer))
                   (fboundp 'haskell-process-restart))
              (haskell-process-restart))
            (setq my-haskell-dir-locals-last-dir current-dir)))))))

(add-hook 'buffer-list-update-hook #'my-haskell-apply-dir-locals)

;; ----------------
;; validate that the cabal args are safe
;; ----------------
(add-to-list 'safe-local-variable-values '(haskell-process-type . cabal-repl))
(defun cabal-args-are-safe (val)
  "Validate that VAL is validate cabal args list."
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

;; ----------------
;; run hlint and tests in compilation buffer
;; ----------------
(defun haskell-run-hlint ()
  "Run hlint."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "hlint -s .")))

(defun compile-with-haskell-process-type (fun)
  "Call (FUN stack-or-cabal) and put result in comp buffer."
  (let ((stack-or-cabal (haskell-process-type))
        (compilation-environment '("TERM=xterm-256color"))
        (default-directory (projectile-project-root)))
    (if (eq stack-or-cabal 'ghci)
        (message "no project directory found")
      (compile (funcall fun stack-or-cabal)))))

(defun haskell-run-tests ()
  "Run test-suite."
  (interactive)
  (compile-with-haskell-process-type
   (lambda (process-type)
     (if (eq process-type 'stack-ghci)
         "stack test --color always"
       "cabal test --test-show-details=direct"))))


(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c l") #'haskell-run-hlint)
  (define-key haskell-mode-map (kbd "C-c t") #'haskell-run-tests)
  (define-key haskell-mode-map (kbd "C-c C-a") #'haskell-command-insert-language-pragma)
  ;; annoying minor mode modelines
  (diminish 'haskell-collapse-mode)
  (diminish 'haskell-doc-mode)
  (diminish 'haskell-indent-mode)
  (diminish 'interactive-haskell-mode)
  )

(setq haskell-process-args-stack-ghci '("--ghc-options" "-w")
      haskell-process-show-debug-tips t
      haskell-doc-prettify-types t
      haskell-indentation-electric-flag t
      haskell-process-suggest-language-pragmas nil
      haskell-process-suggest-restart nil
      haskell-process-prompt-restart nil)

;;; haskell-mode-git.el ends here
