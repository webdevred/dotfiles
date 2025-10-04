;;; package --- Summary: misc file
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; set garbaǵe collection high to increase speed
 '(gc-cons-threshold 100000000)
 '(fill-column 80)
 '(fit-window-to-buffer-horizontally t)
 '(menu-bar-select-buffer-function 'switch-to-buffer)
 '(qmouse-avoidance-mode 'banish)
 '(window-resize-pixelwise t)
 ;; disable splash screen
 '(inhibit-splash-screen t)
 '(inhibit-startup-screen t)
 '(inhibit-startup-message t)
 '(indent-tabs-mode nil)
 ;; disable scratch message
 '(initial-scratch-message "")
 '(split-width-threshold 0)
 '(split-height-threshold nil)
 ;; Removes whitespace at end of lines
 '(truncate-lines t)
 '(ring-bell-function #'ignore)
 '(warning-minimum-level :error)
 '(require-final-newline t)
 '(js-indent-level 2))

(defun my/display-buffer-right-or-reuse (buffer _alist)
  "Show BUFFER on the right side."
  (let* ((windows
          (cl-remove-if
           (lambda (win)
             (with-current-buffer (window-buffer win)
               (eq major-mode 'treemacs-mode)))
           (window-list)))
         (right-win (window-in-direction 'right)))
    (cond
     ((= (length windows) 2)
      (let ((other (car (delq (selected-window) windows))))
        (when other
          (set-window-buffer other buffer)
          other)))

     ((and right-win
           (not (with-current-buffer (window-buffer right-win)
                  (eq major-mode 'treemacs-mode))))
      (set-window-buffer right-win buffer)
      right-win)

     (t
      (let ((new-win (split-window (selected-window) nil 'right)))
        (set-window-buffer new-win buffer)
        new-win)))))

(setq display-buffer-alist
      '(("\\*\\(Warnings\\|Completions\\|Buffer List\\)\\*" (display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (window-height . shrink-window-if-larger-than-buffer))
        ("^\\(\\*undo-tree\\|\s\\*Treemacs\\|\s\\*transient\\*\\)" nil)
        ("^COMMIT_EDITMSG$"
         (my/display-buffer-right-or-reuse)
         (body-function . select-window))
        ("^\\(\\*[^\\*]+\\*\\)\\|\\(magit.*: .*\\)$"
         (display-buffer-reuse-window my/display-buffer-right-or-reuse)
         (body-function . select-window))
        (".*"
         (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-use-some-window)
         (some-window 'lru)
         (body-function . select-window))))

(setq-default user-config-dir
              (expand-file-name (file-name-directory user-init-file)))

(setq-default backup-directory-alist
              `((".*" . ,(concat user-config-dir "var/backups/"))))

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-config-dir "var/auto-saves/") t)))

(dolist (dir (list (concat user-config-dir "var/backups/")
                   (concat user-config-dir "var/auto-saves/")))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; stuff for writing elisp-koans
(defun get-quoted-form-at-point ()
  "Return the quoted form from the current point."
  (save-excursion
    (when (re-search-backward "^(" nil t)
      (form-at-point))))

(defun get-test-name (test-def)
  "Get the name of test from TEST-DEF."
  (cadr test-def))

(defun run-elisp-koan-test ()
  "Run elisp koan test at point."
  (interactive)
  (if (and (bound-and-true-p projectile-mode)
           (s-ends-with-p "elisp-koans/" (projectile-project-root)))
      (let ((test-def (get-quoted-form-at-point)))
        (if (not test-def)
            (message "not valid sexp")
          (load-file (concat (projectile-project-root) "elisp-koans.el"))
          (eval test-def)
          (when (functionp 'elisp-koans/run-test)
            (elisp-koans/run-test (get-test-name test-def)))
          (princ "You are not in the elisp-koans repo")))))

(defun set-mode-for-backupish-files ()
  "Set mode for file are similar backup files."
  (when (and buffer-file-name
             (string-match "\\(.*\\)~[^/]*\\'" buffer-file-name))
    (let ((base (match-string 1 buffer-file-name)))
      (let ((mode (assoc-default base auto-mode-alist 'string-match)))
        (when mode
          (funcall mode))))))
(add-hook 'find-file-hook 'set-mode-for-backupish-files)

;; Major mode hooks
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Use spaces, not tabs.
            (progn
              (setq indent-tabs-mode nil)
              (define-key emacs-lisp-mode-map (kbd "C-c C-r") #'run-elisp-koan-test))
            ))
(add-hook 'js-json-mode-hook
          (lambda () (if (equal (buffer-name) "audiosink.json") (json-pretty-print-buffer))))

(add-hook 'text-mode-hook #'visual-line-mode)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Colorize ANSI escape sequences in the compilation buffer."
  (let ((inhibit-read-only t)
        (compilation-environment '("LANG=C")))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; set up column numbers
(column-number-mode 1)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

;; deb key bindings
(global-set-key
 (kbd "C-c d")
 (lambda ()
   (interactive)
   (save-excursion
     (goto-char (point-min))
     ;; Remove all comment lines
     (delete-matching-lines
      (concat "^[ \t]*"
              (regexp-quote (string-trim-right comment-start))))
     ;; Collapse multiple blank lines into one
     (goto-char (point-min))
     (while (re-search-forward "\n\\{3,\\}" nil t)
       (replace-match "\n\n"))
     ;; Remove leading/trailing blank lines
     (goto-char (point-min))
     (when (looking-at-p "\n")
       (delete-blank-lines))
     (goto-char (point-max))
     (when (looking-back "\n" nil)
       (delete-blank-lines))
     (indent-region (point-min) (point-max)))))
(global-set-key (kbd "C-c b") (lambda () (interactive) (projectile-switch-to-buffer)))
(global-set-key (kbd "C-c r") (lambda () (interactive) (load user-init-file) ) )
(global-set-key (kbd "C-c u") (lambda () (interactive) (package-upgrade-all nil) ))

(defun my-jbeam-try-load-mode (mode)
  "Load MODE jbeam modes if we are in jbeam-edit project."
  (let* ((project-root (ignore-errors (projectile-project-root)))
         (project-name (and project-root (projectile-project-name)))
         (elisp-dir (and project-root (concat project-root "/editors"))))
    (if (and (string= project-name "jbeam-edit")
             elisp-dir
             (file-directory-p elisp-dir))
        (progn
          (add-to-list 'load-path elisp-dir)
          (require mode)
          (funcall mode))
      (fundamental-mode))))

(add-to-list 'auto-mode-alist
             '("\\.jbfl\\'" . (lambda () (my-jbeam-try-load-mode 'jbfl-mode))))
(add-to-list 'auto-mode-alist
             '("\\.jbeam\\'" . (lambda () (my-jbeam-try-load-mode 'jbeam-mode))))

;; remove ugly bars
(menu-bar-mode -1)

(if window-system
    (progn (tool-bar-mode -1)
           (scroll-bar-mode -1))
  )

;; y/n is better than yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; allow me to go other windows
(windmove-default-keybindings)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (haskell . t)))

(defvar scratchpad-buffer "*scratch*"
  "Name of the scratchpad buffer.")

(defun get-window-class ()
  "Return the WM_CLASS of the focused window."
  (shell-command-to-string "xdotool getwindowfocus getwindowclassname"))

(defun is-scratchpad ()
  "Check if the focused window is the scratchpad."
  (string-match-p "emacs-scratch" (get-window-class)))

(defun switch-to-buffer-check-scratchpad (orig-fun buffer-or-name &rest args)
  "Advice function to switch buffer, allowing only *scratch* buffer if the scratchpad is focused."
  (if (and (is-scratchpad) (not (equal (buffer-name buffer-or-name) scratchpad-buffer)))
      (message "Cannot switch buffer when scratchpad is focused.")
    (apply orig-fun buffer-or-name args)))

(advice-add 'switch-to-buffer :around #'switch-to-buffer-check-scratchpad)
(advice-add 'switch-to-buffer-other-window :around #'switch-to-buffer-check-scratchpad)
(advice-add 'switch-to-buffer-other-frame :around #'switch-to-buffer-check-scratchpad)

(define-generic-mode 'bnf-mode
  nil
  nil
  '(("<[^>]+>" . font-lock-function-name-face)
    ("::=" . font-lock-keyword-face)
    ("\"[^\"]*\"" . font-lock-string-face)
    ("'[^']*'" . font-lock-string-face))
  '("\\.bnf\\'")
  nil
  "Simple BNF mode for syntax highlighting.")

;; Register 'bnf' language with org-babel for highlighting
(add-to-list 'org-src-lang-modes '("bnf" . bnf))

(provide 'misc)
;;; misc.el ends here
