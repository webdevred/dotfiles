;; -*- lexical-binding: t; -*-
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
 '(warning-minimum-level :error))

(setq user-config-dir
  (expand-file-name (file-name-directory user-init-file)))

(setq backup-directory-alist
      `((".*" . ,(concat user-config-dir "backups/"))))

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-config-dir "auto-saves/") t)))

(dolist (dir (list (concat user-config-dir "backups/")
                   (concat user-config-dir "auto-saves/")))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; stuff for writing elisp-koans
(defun get-quoted-form-at-point ()
  "Return the quoted form from the current point."
  (save-excursion
    (when (re-search-backward "^(" nil t)
          (form-at-point))))

(defun get-test-name (test-def)
    (cadr test-def))

(defun run-elisp-koan-test ()
  "run test"
  (interactive)
  (if (and (bound-and-true-p projectile-mode) (s-ends-with-p "elisp-koans/" (projectile-project-root)))
      (let ((test-def (get-quoted-form-at-point)))
        (if (not test-def)
            (message "not valid sexp")
          (load-file (concat (projectile-project-root) "elisp-koans.el"))
          (eval test-def)
          (elisp-koans/run-test (get-test-name test-def))))
    (princ "You are not in the elisp-koans repo")))

(define-derived-mode jbeam-mode js-mode "Jbeam"
  "Major mode for Jbeam files.")

(add-to-list 'auto-mode-alist '("\\.jbeam\\'" . jbeam-mode))

;; Major mode hooks
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    ;; Use spaces, not tabs.
    (progn
      (setq indent-tabs-mode nil)
      (define-key emacs-lisp-mode-map (kbd "C-c C-r") #'run-elisp-koan-test))
    ))
(add-hook 'js-json-mode-hook
          (lambda () (if (((equal (buffer-name) "audiosink.json") . (json-pretty-print-buffer))))))

(defun do-calc-clear-calculations ()
  (when (not (equal (calc-stack-size) 0))
    (calc-pop (calc-stack-size))))

(defun calc-clear-calculations ()
  (interactive)
  (when (equal major-mode 'calc-mode)
    (do-calc-clear-calculations)))

(add-hook 'calc-mode-hook
          (lambda () (define-key calc-mode-map (kbd "C-c k") #'calc-clear-calculations)))

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

; deb key bindings
(global-set-key (kbd "C-c b") (lambda () (interactive) (projectile-switch-to-buffer)))
(global-set-key (kbd "C-c d") (lambda () (interactive) (delete-matching-lines)))
(global-set-key (kbd "C-c r") (lambda () (interactive) (load user-init-file) ) )
(global-set-key (kbd "C-c u") (lambda () (interactive) (package-upgrade-all nil) ))


(defun user-installed-p (package)
  ""
  (let* ((pkg-desc (cadr (assq package package-alist)))
         (dir (package-desc-dir pkg-desc)))
    (file-in-directory-p dir package-user-dir)))

; remove ugly bars
(menu-bar-mode -1)

(if window-system
    (progn (tool-bar-mode -1)
           (scroll-bar-mode -1))
  )

; y/n is better than yes/no
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
