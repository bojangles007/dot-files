(package-initialize)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(unless (package-installed-p 'use-package)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(let ((dotfiles-dir (file-name-directory (or (buffer-file-name)
                                             load-file-name))))
  (mapc #'org-babel-load-file
        (directory-files dotfiles-dir t "[A-Z]\\w+\\.org$")))
