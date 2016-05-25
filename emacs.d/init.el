;;; init.el --- Where all the magic begins

;;; Commentary:

;; This file loads Org-mode and then loads the rest of our Emacs
;; initialization from Emacs Lisp embedded in literate Org-mode files.

;; I copied this from an example found at
;; http://orgmode.org/worg/org-contrib/babel/intro.html#literate-programming

;;; Code:

;; Initialize packages early, so we load latest org from ELPA later in this
;; file.
(package-initialize)

;; Don't call package-initialize again after we're done with init.el
(setq package-enable-at-startup nil)

;; Load (and tangle) all literate org-mode files in this directory
(let ((dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name))))
  (mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$")))

;;; init.el ends here
