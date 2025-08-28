;;; agitjo.el --- Manage Forgejo PRs with AGit-Flow  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Alvin Hsu

;; Author: Alvin Hsu <aurtzy@gmail.com>
;; Homepage: https://github.com/halvin/agitjo
;; Keywords: convenience, vc, tools

;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (magit "4.3.8") (transient "0.9.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; AGitjo provides a transient interface for managing forge pull requests using
;; AGit-Flow, specifically for Forgejo-based (e.g. Codeberg) repositories.  This
;; package aims to implement facilities that make the AGit workflow more
;; convenient for users.

;; Some integration with Magit is also included.  It does not make use of the
;; Forgejo API.

;;; Code:

;;; Initial Code.

(require 'eieio)
(require 'magit)
(require 'markdown-mode)
(require 'pcase)
(require 'transient)

(keymap-set magit-status-mode-map "#" #'agitjo-push)

(transient-append-suffix 'magit-dispatch "!"
  '("#" "AGit-Flow Push" agitjo-push))

;;; Options.

(defvar agitjo--topic-current nil
  "The current session topic.

If nil, the PR's source branch will be used by default.")


;;; Classes.

;;;; `agitjo-push-pullreq-suffix'.

(defclass agitjo-push-pullreq-suffix (transient-suffix)
  ((source :initarg :source
           :initform nil
           :type (or function null)
           :documentation "\
Thunk that returns source reference.  If nil, read from user instead.")
   (target :initarg :target
           :initform nil
           :type (or function null)
           :documentation "\
Thunk that returns target branch.  If nil, read from user instead.")))

(cl-defmethod agitjo-pullreq-source ((obj agitjo-push-pullreq-suffix))
  "Return pull request source for OBJ, as a string.  Prompt if needed."
  (let ((value (oref obj source)))
    (cond
     ((functionp value) (funcall value))
     (t (magit-read-local-branch "Source local branch: ")))))

(cl-defmethod agitjo-pullreq-target ((obj agitjo-push-pullreq-suffix))
  "Return pull request target for OBJ, as a string.  Prompt if needed."
  (let ((value (oref obj target)))
    (cond
     ((functionp value) (funcall value))
     (t (magit-read-remote-branch "Target remote branch: ")))))

;;;; `agitjo--topic-variable-infix'

(defclass agitjo--topic-variable-infix (transient-variable)
  ((reader :initform #'agitjo--topic-reader)
   (prompt :initform "Topic (empty to use PR source branch): ")))

(cl-defmethod transient-infix-set ((_obj agitjo--topic-variable-infix) value)
  "Set `agitjo--topic-current' to VALUE."
  (setq agitjo--topic-current value))

(cl-defmethod transient-format-value ((_obj agitjo--topic-variable-infix))
  "Return `agitjo--topic-current' as a formatted string for display."
  (concat
   "("
   (or (and agitjo--topic-current
            (propertize agitjo--topic-current 'face 'transient-value))
       (propertize "<use PR source branch>" 'face 'transient-inactive-value))
   ")"))

(defun agitjo--topic-reader (prompt initial-input history)
  "Read and return the session identifier to use.

PROMPT, INITIAL-INPUT, and HISTORY are as defined in `read-string'."
  (read-string prompt initial-input history))


;;; Modes.

;;;; Auxiliary.

(defvar agitjo-post--buffer-name "*AGitjo-post*")

(defvar-local agitjo-post--pullreq-args nil
  "Buffer-local storage for arguments to pass to `agitjo--push-pullreq'.")

;; TODO: Back the buffer by a file, so it can have a chance of recovery in case
;; of some interruption.
(defun agitjo-post--setup-buffer (args)
  "Set up buffer for editing pull request posts.

ARGS is a list of arguments to be passed to `agitjo--push-pullreq'."
  (let* ((buffer (get-buffer-create agitjo-post--buffer-name)))
    (with-current-buffer buffer
      (agitjo-post-mode)
      (setq agitjo-post--pullreq-args args)
      (insert "\
<!-- WARNING: Recovery facilities for this buffer have not yet been implemented.
This buffer is NOT backed by a file, nor any history, so it is recommended to
prepare descriptions elsewhere to copy here if a description is going to contain
a significant amount of content.
(this comment can be deleted.) -->"))
    (select-window (display-buffer buffer))))

;;;; Definitions.

(defvar-keymap agitjo-post-mode-map
  "C-c C-c" #'agitjo-post-confirm
  "C-c C-k" #'agitjo-post-cancel
  "<remap> <kill-buffer>" #'agitjo-post-cancel)

(define-derived-mode agitjo-post-mode gfm-mode "AGitjo-Post"
  "Major mode for editing pull request post contents.")


;;; Commands.

;;;; Interactive functions.

;;;;; Definitions.

(defun agitjo-post-cancel ()
  "Cancel pull request post."
  (interactive)
  (with-current-buffer agitjo-post--buffer-name
    (quit-window :kill (get-buffer-window))
    (message "Canceled post creation.")))

(defun agitjo-post-confirm ()
  "Confirm pull request post."
  (interactive)
  (unless (equal agitjo-post--buffer-name (buffer-name))
    (user-error "Function called outside AGitjo post buffer"))
  (with-current-buffer agitjo-post--buffer-name
    (apply #'agitjo--push-pullreq
           `(,@agitjo-post--pullreq-args
             ,(concat "--push-option=description=" (agitjo--sanitize-description
                                                    (buffer-string)))))
    (quit-window :kill (get-buffer-window))))

;;;; Transient suffixes.

;;;;; Auxiliary.

(defun agitjo-get-target-branch (branch)
  "Return the target pull request branch from BRANCH.

If BRANCH is a local branch, assume that the target remote is
`magit-primary-remote' and return the associated branch for that remote
if it exists.  If there is no associated remote branch, return nil.
Otherwise, BRANCH is already a remote branch, and return it as-is."
  (if (magit-local-branch-p branch)
      (let ((remote-branch (propertize (concat (magit-primary-remote) "/" branch)
                                       'face 'magit-branch-remote)))
        (if (magit-remote-branch-p remote-branch) remote-branch nil))
    branch))

(defun agitjo-pullreq-refspec (type source target)
  "Construct and return a pull request refspec from arguments.

TYPE should be one of \"for|draft|for-review\", where \"for\" is a
normal pull request.  This should always be \"for\", as this feature is
not yet implemented in Forgejo.

SOURCE must a local branch.  TARGET must be a remote branch.

If `agitjo--topic-current' is non-nil, use that value as the session.
Otherwise, the source branch name will be used."
  (let ((valid-types '("for" "draft" "for-review")))
    (unless (member type valid-types)
      (error "Pull request type is not one of %S" valid-types)))
  (unless (magit-local-branch-p source)
    (error "Source branch is not a local branch: %S" source))
  (let ((target-branch (agitjo--remote-branch-name target))
        ;; TODO: How do we handle local references?  We can currently default to
        ;; source since it can only be a local branch, but git also allows local
        ;; references; if we permit using local references as sources, could
        ;; this default-to-source cause issues in the refspec?
        (session (or agitjo--topic-current source)))
    (format "%s:refs/%s/%s/%s" source type target-branch session)))

(defun agitjo--push-pullreq (type source target &rest args)
  "Push a pull request of TYPE, from SOURCE ref to TARGET branch.

TYPE, SOURCE, and TARGET will be passed to
`agitjo-pullreq-refspec' to construct a pull request refspec; see
for documentation.

ARGS is a list of additional arguments to pass to \"git push\"."
  (pcase-exhaustive (magit-split-branch-name target)
    (`(,remote . ,_target-branch)
     (let ((refspec (agitjo-pullreq-refspec type source target)))
       (magit-run-git-async "push" "-v" remote refspec args)))))

(defun agitjo--remote-branch-name (branch)
  "Return the name part of remote branch BRANCH."
  (unless (magit-remote-branch-p branch) (error "Not a remote branch: %S" branch))
  (pcase-exhaustive (magit-split-branch-name branch)
    (`(,_remote . ,name) name)))

(defun agitjo--remote-branch-remote (branch)
  "Return the remote part of remote branch BRANCH."
  (unless (magit-remote-branch-p branch) (error "Not a remote branch: %S" branch))
  (pcase-exhaustive (magit-split-branch-name branch)
    (`(,remote . ,_name) remote)))

(defun agitjo--sanitize-description (string)
  "Remove or convert problematic characters from description STRING."
  ;; New lines cause a git error "fatal: push options must not have new line
  ;; characters", but carriage returns seem to work fine, and render fine on
  ;; Codeberg as well.
  (string-replace "\n" "\r" string))

;;;;; Definitions.

(transient-define-suffix agitjo-push-pullreq (args)
  "Push with AGit-Flow to create or edit a pull request.

This is meant to be modified with keywords in suffix specifications.
See `agitjo-push-pullreq-suffix' for information on slots.

ARGS is a list of transient arguments to be passed to \"git push\"."
  :class 'agitjo-push-pullreq-suffix
  (interactive (list (transient-args 'agitjo-push)))
  (let* ((obj (transient-suffix-object))
         (force-push? (transient-arg-value "--push-option=force-push=true" args))
         (source (agitjo-pullreq-source obj))
         (target (agitjo-pullreq-target obj))
         ;; TODO: Implement using pull request type from transient state.
         ;; Hard-code as "for" for now.
         (pullreq-args (list "for" source target args)))
    (if force-push?
        (apply #'agitjo--push-pullreq pullreq-args)
      (agitjo-post--setup-buffer pullreq-args))))

(transient-define-suffix agitjo-push-pullreq-current-to-upstream ()
  :class 'agitjo-push-pullreq-suffix
  :source #'magit-get-current-branch
  :target (lambda ()
            (if-let* ((current-branch (magit-get-current-branch))
                      (branch (magit-get-upstream-branch))
                      (target-branch (agitjo-get-target-branch branch)))
                target-branch
              (let ((new-upstream (magit-read-remote-branch
                                   (format "Set upstream of %s and push PR: "
                                           current-branch))))
                (magit-set-upstream-branch current-branch new-upstream)
                new-upstream)))
  :inapt-if-not (lambda () (magit-get-current-branch))
  :description (lambda ()
                 (if-let* ((branch (magit-get-upstream-branch)))
                     (if-let* ((target-branch (agitjo-get-target-branch branch)))
                         target-branch
                       (format "@{upstream}, overwriting it (%s not in %s)"
                               branch (propertize (magit-primary-remote)
                                                  'face 'magit-branch-remote)))
                   "@{upstream}, setting it"))
  (interactive)
  (call-interactively #'agitjo-push-pullreq))

;;;; Transient infixes.

;;;;; Definitions.

(transient-define-infix agitjo-force-push-switch ()
  "Force-push to an existing pull request.

If this is not passed, a new pull request will be created with the topic
identifier, even if a pull request with the same ID exists."
  :class 'transient-switch
  :argument "--push-option=force-push=true")

(transient-define-infix agitjo-title-option ()
  "Title of pull request.

Leave empty to use the first line of the first new Git commit."
  :class 'transient-option
  :argument "--push-option=title=")

(transient-define-infix agitjo-topic-variable ()
  "Topic of the pull request.

This is an identifier string that controls which pull request is being
interacted with.  If not specified, the pull request's source branch
will be used as the topic."
  :class 'agitjo--topic-variable-infix)

;;;; Transient prefixes.

;;;;; Definitions.

(transient-define-prefix agitjo-push ()
  "Push to a Forgejo-based repository, using AGit-Flow."
  ["Arguments"
   ("-f" "Force-push existing PR" agitjo-force-push-switch)
   ("-t" "Title" agitjo-title-option)
   ("-s" "Session/topic" agitjo-topic-variable)]
  [ :inapt-if-not magit-get-current-branch
    :description (lambda ()
                   (if-let* ((branch (magit-get-current-branch)))
                       (format (propertize "Push pull request from %s to"
                                           'face 'transient-heading)
                               (propertize branch 'face 'magit-branch-local))
                     "Push pull request from <no current branch> to"))
    ("u" agitjo-push-pullreq-current-to-upstream)]
  ["Configure"
   ("C" "Set variables..." magit-branch-configure)])

(transient-augment-suffix agitjo-push
  :inapt-if-not #'magit-get-current-branch)

;;; Provide library.
(provide 'agitjo)
;;; agitjo.el ends here
