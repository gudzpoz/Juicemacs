;; -*- lexical-binding: t; -*-

(require 'ox-html)
(require 'ox-publish)

(require 'package)
(setq package-load-list '(all))
(package-initialize)
(eval '(use-package ox-html-stable-ids :ensure
         :vc (:url "https://codeberg.org/jkreeftmeijer/ox-html-stable-ids.el.git")
         :config (org-html-stable-ids-add)
         :custom (org-html-stable-ids t)))
(eval '(use-package htmlize :ensure
         :vc (:url "https://github.com/emacsorphanage/htmlize.git")))

(setq org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "
<link rel=\"stylesheet\" href=\"/tufte.css\">
<link rel=\"stylesheet\" href=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/css/htmlize.css\">
"
      org-html-htmlize-output-type 'css)

(setq org-export-use-babel t
      org-confirm-babel-evaluate nil)
(add-to-list 'org-babel-default-header-args '(:eval . "no-export"))
(add-to-list 'org-babel-default-header-args '(:exports . "both"))
(add-to-list 'org-babel-default-header-args:elisp '(:eval . "no-export"))
(add-to-list 'org-babel-default-header-args:elisp '(:exports . "both"))

(defvar gh-pages-directory
  (file-name-parent-directory load-file-name))
(defvar project-directory
  (file-name-parent-directory gh-pages-directory))
(setq org-publish-project-alist
      `(("Juicemacs docs"
         :recursive t
         :base-directory ,gh-pages-directory
         :publishing-directory ,gh-pages-directory
         :publishing-function ,#'org-html-publish-to-html
         :with-author nil
         :with-creator t
         :with-toc t
         :section-numbers t
         :time-stamp-file nil)))

(defconst org-files
  '("README.org"
    "CONTRIBUTING.org"
    "docs/*.org"
    "elisp/README.org")
  "Org files to publish.")

(dolist (pattern org-files)
  (let ((pattern (file-name-concat project-directory pattern)))
    (dolist (file (file-expand-wildcards pattern))
      (let* ((rel (file-relative-name file project-directory))
             (dest (file-name-concat gh-pages-directory rel)))
        (mkdir (file-name-parent-directory dest) t)
        (copy-file file dest t)))))

(org-publish-all t)
