;;; package --- Summary: theme file
;;; Commentary:
;;; Code:

(deftheme cozy-pink
  "A simple theme for Emacs.")

(setq-default nlinum-format " %d ")

(let ((class '((class color) (min-colors 256)))
      (black "#000000")
      (gray "#666666")
      (pink "#ffaaff")
      (purple "#ff55ff")
      )
  (custom-theme-set-faces
   'cozy-pink

   `(default ((,class (:foreground ,pink :background ,black))))

   `(minibuffer-prompt ((,class (:foreground ,purple))))
   `(hl-line ((,class (:foreground "#0000ff" :background ,purple :weight bold))))
   `(mode-line ((,class (:foreground ,black :background ,pink))))
   `(mode-line-inactive ((,class (:foreground ,black :background ,purple))))

   `(font-lock-type-face ((,class (:foreground ,purple))))
   `(font-lock-variable-name-face ((,class (:foreground ,purple))))

   `(flymake-error-echo ((,class (:foreground "#8B0000"))))
   `(flymake-warning-echo ((,class (:foreground "#32CD32"))))
   `(line-number ((,class (:foreground ,pink))))
   `(match ((,class (:background ,gray))))
   `(isearch ((,class (:foreground ,purple :background ,gray))))

   `(cursor ((,class (:foreground ,pink :background ,gray))))
   `(region ((,class (:background ,gray))))
   )
  )

(provide-theme 'cozy-pink)
;;; cozy-pink-theme.el ends here
