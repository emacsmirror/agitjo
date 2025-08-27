(use-modules (gnu packages emacs-xyz)
             (gnu packages version-control)
             (guix build-system emacs)
             (guix download)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages))

(define-public emacs-agitjo
  (package
    (name "emacs-agitjo")
    (version "0.0.0-dev")
    (source
     (let ((root (dirname (current-filename))))
       (local-file root
                   #:recursive? #t
                   #:select? (git-predicate root))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-magit emacs-markdown-mode emacs-transient))
    (home-page "https://codeberg.org/halvin/agitjo")
    (synopsis "Manage Forgejo PRs with AGit-Flow in Emacs")
    (description "")
    (license license:gpl3+)))

emacs-agitjo
