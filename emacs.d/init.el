(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(setq package-archive-priorities
      '(("org" . 30)
        ("melpa-stable" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))

(package-initialize)

(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(add-to-list 'package-selected-packages 'use-package)

(let ((dotfiles-dir (file-name-directory (or (buffer-file-name)
                                             load-file-name))))
  (mapc #'org-babel-load-file
        (directory-files dotfiles-dir t "[A-Z]\\w+\\.org$")))
