;; -*- lexical-binding: t; -*-
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

   `(mode-line ((,class (:foreground ,black :background ,pink))))
   `(mode-line-inactive ((,class (:foreground ,black :background ,purple))))

   `(line-number ((,class (:foreground ,pink))))
   `(match ((,class (:background ,gray))))
   `(isearch ((,class (:foreground ,purple :background ,gray))))

   `(cursor ((,class (:foreground ,pink :background ,gray))))
   `(region ((,class (:background ,gray))))
   )

  (if (facep 'treemacs-root-face)
    (custom-theme-set-faces
     'cozy-pink
     `(treemacs-root-face ((,class (:foreground ,pink)))))
    )
  (if (facep 'which-func)
    (custom-theme-set-faces
     'cozy-pink
     `(which-func ((,class (:foreground ,black)))))
    )
  )

(provide-theme 'cozy-pink)
(provide 'cozy-pink-theme)
