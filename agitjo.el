;;; agitjo.el --- Manage Forgejo PRs with AGit-Flow  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Alvin Hsu

;; Author: Alvin Hsu <aurtzy@gmail.com>
;; Homepage: https://codeberg.org/halvin/agitjo
;; Keywords: convenience, vc, tools

;; Package-Version: 0.3.0
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

;; AGitjo extends Magit with a new menu for AGit-Flow operations, to make them
;; more convenient for users.  The AGit workflow enables users to create and
;; edit pull requests using just the "git push" command.  This package is
;; intended specifically for use with Forgejo-based (e.g. Codeberg)
;; repositories.

;;; Code:

;;; Initial Code.

(require 'eieio)
(require 'magit)
(require 'markdown-mode)
(require 'pcase)
(require 'project)
(require 'transient)

(keymap-set magit-status-mode-map "#" #'agitjo-push)

(transient-append-suffix 'magit-dispatch "!"
  '("#" "AGit-Flow push" agitjo-push))


;;; Classes.

;;;; `agitjo-push-pullreq-suffix'.

(defclass agitjo-push-pullreq-suffix (transient-suffix)
  ((source :initarg :source
           :type function
           :documentation "Thunk that returns local source branch or reference.")
   (target :initarg :target
           :type function
           :documentation "Thunk that returns remote target branch.")))

(cl-defmethod agitjo-pullreq-source ((obj agitjo-push-pullreq-suffix))
  "Return pull request source for OBJ from calling the source slot's function."
  (funcall (oref obj source)))

(cl-defmethod agitjo-pullreq-target ((obj agitjo-push-pullreq-suffix))
  "Return pull request target for OBJ from calling the target slot's function."
  (funcall (oref obj target)))

;;;; `agitjo--pullreq-configuration'

(defvar agitjo--push-pullreq-debug? nil)

(defclass agitjo--pullreq-configuration ()
  ((type :initarg :type
         :type (satisfies agitjo--valid-pullreq-type?)
         :documentation "Pull request type.

This should always be \"for\", as Forgejo does not currently implement
this feature.")
   (source :initarg :source
           :type (satisfies magit-ref-p)
           :documentation "Source reference to use for pull request.")
   (target :initarg :target
           :type (satisfies agitjo-get-target-branch)
           :documentation "Target remote branch for pull request.")
   (args :initarg :args
         :type (list-of string)
         :documentation "Additional arguments to pass to \"git push\"."))
  "Class for storing push information about an AGit pull request.

The `type', `source', and `target' slots are passed to
`agitjo-pullreq-refspec' to construct a pull request refspec; see for
documentation.")

(cl-defmethod agitjo--pullreq-refspec ((config agitjo--pullreq-configuration))
  "Construct and return a pull request refspec from CONFIG.

SOURCE must a local branch.  TARGET must be a remote branch.

If the current topic for this project is non-nil, use that value as the
session.  Otherwise, the source branch name will be used."
  ;; Everything after "refs/{for|...}/{target}/" is matched as the entire
  ;; session string.  This permits "/", so it is okay if `session' is a full
  ;; refname.
  (let* ((type (oref config type))
         (source (oref config source))
         (target-branch (agitjo--pullreq-target-name config))
         (session (or (agitjo--get-current-topic) source)))
    (format "%s:refs/%s/%s/%s" source type target-branch session)))

(cl-defmethod agitjo--push-pullreq ((config agitjo--pullreq-configuration)
                                    &optional synchronously?)
  "Push an AGit pull request with CONFIG configuration.

By default, run asynchronously with an unspecified return value.  If
SYNCHRONOUSLY? is non-nil, wait for \"git push\" to finish before
returning, and return the exit code."
  (let ((refspec (agitjo--pullreq-refspec config))
        (remote (agitjo--pullreq-target-remote config))
        (args (oref config args)))
    (cond
     (agitjo--push-pullreq-debug?
      (message "debug: (remote; refspec; args): %s; %s; %S"
               remote refspec (seq-map #'substring-no-properties
                                       (flatten-list args)))
      0)
     (synchronously?
      (magit-run-git "push" "-v" remote refspec args))
     (t
      (magit-run-git-async "push" "-v" remote refspec args)))))

(cl-defmethod agitjo--pullreq-target-name ((config agitjo--pullreq-configuration))
  "Return the pull request target's name from CONFIG."
  (pcase-exhaustive (magit-split-branch-name (oref config target))
    (`(,_remote . ,name) name)))

(cl-defmethod agitjo--pullreq-target-remote ((config agitjo--pullreq-configuration))
  "Return the pull request target's remote from CONFIG."
  (pcase-exhaustive (magit-split-branch-name (oref config target))
    (`(,remote . ,_name) remote)))

(defun agitjo--valid-pullreq-type? (value)
  "Return non-nil if VALUE is a valid pull request type."
  (member value '("for" "draft" "for-review")))

;;;; `agitjo--topic-variable-infix'

(defvar agitjo--current-topics nil
  "Alist of project root to \"current topic\" session identifier.

Access and set this variable with `agitjo--get-current-topic' and
`agitjo--set-current-topic', respectively.

The session identifier is persistent per project, per Emacs session.  If
there is no associated session identifier in this variable or it is nil,
the pull request's source branch will be used by default.

Practically, the only projects that will ever have entries in this
variable will be Git projects.")

(defclass agitjo--topic-variable-infix (transient-variable)
  ((reader :initform #'agitjo--topic-reader)
   (prompt :initform "Topic (empty to use PR source branch): ")))

(cl-defmethod transient-infix-set ((_obj agitjo--topic-variable-infix) value)
  "Set the current topic for this project to VALUE."
  (agitjo--set-current-topic value))

(cl-defmethod transient-format-value ((_obj agitjo--topic-variable-infix))
  "Return the current topic as a formatted string for display."
  (concat
   "("
   (let ((topic (agitjo--get-current-topic)))
     (or (and topic (propertize topic 'face 'transient-value))
         (propertize "<use PR source branch>" 'face 'transient-inactive-value)))
   ")"))

(defun agitjo--get-current-topic ()
  "Return the current session identifier associated with the current project.

May be nil."
  (if-let* ((project (project-current))
            (root (project-root project)))
      (alist-get root agitjo--current-topics nil nil #'equal)))

(defun agitjo--set-current-topic (new-topic)
  "Set the current session identifier for the current project to NEW-TOPIC."
  (if-let* ((project (project-current))
            (root (project-root project)))
      (setf (alist-get root agitjo--current-topics nil nil #'equal) new-topic)))

(defun agitjo--topic-reader (prompt initial-input history)
  "Read and return the session identifier to use.

PROMPT, INITIAL-INPUT, and HISTORY are as defined in `read-string'."
  (read-string prompt initial-input history))


;;; Modes.

;;;; Auxiliary.

(defvar agitjo-post--draft-file-name "agitjo/pullreq-draft"
  "The relative file name for AGitjo PR draft, from the repository's gitdir.")

(defvar-local agitjo-post--pullreq-config nil
  "Buffer-local storage for configuration to pass to `agitjo--push-pullreq'.")

;; Follow orderings and file names for templates as documented here:
;; <https://forgejo.org/docs/latest/user/issue-pull-request-templates/>

(defvar agitjo-post--pullreq-template-directories '(".forgejo" ".gitea" ".github")
  "Directories where pull request templates are expected.")

(defvar agitjo-post--pullreq-template-files '("PULL_REQUEST_TEMPLATE.md"
                                              "pull_request_template.md")
  "Supported pull request template file names.")

(defun agitjo-post--setup-buffer (config)
  "Set up buffer for editing pull request posts.

CONFIG is the pull request configuration that will be passed to
`agitjo--push-pullreq'."
  (let* ((buffer (agitjo-post--buffer)))
    (with-current-buffer buffer
      (agitjo-post-mode)
      (setq agitjo-post--pullreq-config config
            header-line-format "C-c C-c to confirm and send; C-c C-k to cancel.")
      (select-window (display-buffer buffer))
      (if (= (buffer-size) 0)
          (agitjo-post--erase-and-insert-pullreq-template)
        (magit-read-char-case "A previous draft exists: " nil
          (?r "[r]esume editing this draft")
          (?d "[d]iscard and start over?"
              (agitjo-post--erase-and-insert-pullreq-template)))))))

(defun agitjo-post--buffer ()
  "Find the post draft file for this repository and return its buffer."
  (let* ((gitdir (or (magit-gitdir)
                     (error "No gitdir associated with this directory")))
         (file (expand-file-name agitjo-post--draft-file-name gitdir))
         (_ (make-directory (file-name-directory file) t)))
    (find-file-noselect file)))

(defun agitjo-post--erase-and-insert-pullreq-template ()
  "Erase the current buffer and insert PR template."
  (erase-buffer)
  ;; TODO: Support YAML templates.
  (if-let* ((file (agitjo-post--find-pullreq-template-file)))
      (insert-file-contents file)))

(defun agitjo-post--find-pullreq-template-file ()
  "Find and return the preferred pull request template file from repository."
  (seq-some (lambda (dir)
              (seq-some (lambda (file)
                          (magit-with-toplevel
                            (let ((filename (expand-file-name file dir)))
                              (if (file-exists-p filename)
                                  filename))))
                        agitjo-post--pullreq-template-files))
            agitjo-post--pullreq-template-directories))

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
  (with-current-buffer (agitjo-post--buffer)
    (save-buffer)
    (quit-window :kill (get-buffer-window)))
  (message "Canceled post creation."))

(defun agitjo-post-confirm ()
  "Confirm pull request post."
  (interactive)
  (let ((post-buffer (agitjo-post--buffer)))
    (unless (equal (buffer-name post-buffer) (buffer-name))
      (user-error "Function called outside AGitjo post buffer"))
    (with-current-buffer post-buffer
      (oset agitjo-post--pullreq-config args
            (cons (concat "--push-option=description="
                          (agitjo--sanitize-description (buffer-string)))
                  (oref agitjo-post--pullreq-config args)))
      (message "Pushing to PR...")
      ;; Don't kill the buffer when git push fails; let the user try submitting
      ;; again or at least have a chance to save contents elsewhere.
      ;; TODO: Make this not block.  This requires more than just using an
      ;; async function, as we do not want to e.g. kill a post buffer when
      ;; git push fails.
      (when (= 0 (agitjo--push-pullreq agitjo-post--pullreq-config :synchronously))
        ;; Since PR was successfully pushed, we don't need to keep this draft
        ;; anymore.  Erase so that we don't prompt to discard/keep next time.
        (erase-buffer)
        (save-buffer)
        (quit-window :kill (get-buffer-window))
        (message "Push successful.")))))

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

(defun agitjo--sanitize-description (string)
  "Remove or convert problematic characters from description STRING."
  ;; New lines cause a git error "fatal: push options must not have new line
  ;; characters", but carriage returns seem to work fine, and render fine on
  ;; Codeberg as well.
  (string-replace "\n" "\r" string))

;;;;; Definitions.

(transient-define-suffix agitjo-push-pullreq (args)
  "Push with AGit-Flow to create or edit a pull request.

ARGS is a list of transient arguments to be passed to \"git push\".

This implements `agitjo-push-pullreq-suffix', and acts as a template
that other suffixes can use.  The `source' and `target' slots are not
set before calling this command.  See class documentation for
information on slots."
  :class 'agitjo-push-pullreq-suffix
  (interactive (list (transient-args 'agitjo-push)))
  (let* ((obj (transient-suffix-object))
         (force-push? (transient-arg-value "--push-option=force-push=true" args))
         (source (agitjo-pullreq-source obj))
         (target (agitjo-pullreq-target obj))
         (pullreq-config (agitjo--pullreq-configuration
                          ;; TODO: Implement using pull request type from
                          ;; transient state.  Hard-code as "for" for now.
                          :type "for"
                          :source source
                          :target target
                          :args args)))
    (if force-push?
        (agitjo--push-pullreq pullreq-config)
      (agitjo-post--setup-buffer pullreq-config))))

(transient-define-suffix agitjo-push-pullreq-current-to-upstream ()
  :class 'agitjo-push-pullreq-suffix
  :source #'magit-get-current-branch
  :target (lambda ()
            (if-let* ((current-branch (magit-get-current-branch))
                      (branch (magit-get-upstream-branch))
                      (target-branch (agitjo-get-target-branch branch)))
                target-branch
              (let ((new-upstream (magit-read-remote-branch
                                   (format "Target (and set as %s upstream): "
                                           current-branch))))
                (magit-set-upstream-branch current-branch new-upstream)
                new-upstream)))
  :inapt-if-not #'magit-get-current-branch
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

(transient-define-suffix agitjo-push-pullreq-current ()
  :class 'agitjo-push-pullreq-suffix
  :source #'magit-get-current-branch
  :target (lambda () (magit-read-remote-branch "Target: "))
  :inapt-if-not #'magit-get-current-branch
  :description "elsewhere"
  (interactive)
  (call-interactively #'agitjo-push-pullreq))

(transient-define-suffix agitjo-push-pullreq-local-branch ()
  "Push AGit-Flow PR from some local branch to some remote branch."
  :class 'agitjo-push-pullreq-suffix
  :source (lambda () (magit-read-local-branch "Source: "))
  :target (lambda () (magit-read-remote-branch "Target: "))
  :description "local branch"
  (interactive)
  (call-interactively #'agitjo-push-pullreq))

(transient-define-suffix agitjo-push-pullreq-local-branch-or-ref ()
  :class 'agitjo-push-pullreq-suffix
  :source (lambda () (magit-read-local-branch-or-ref "Source: "))
  :target (lambda () (magit-read-remote-branch "Target: "))
  :description "local branch or ref"
  (interactive)
  (call-interactively #'agitjo-push-pullreq))

;;;; Transient infixes.

;;;;; Definitions.

(transient-define-infix agitjo-force-push-switch ()
  "Force-push a pull request, if it exists (or create a new PR anyways).

If this is not passed, a new pull request will be created with the topic
identifier, even if a pull request with the same ID exists."
  :class 'transient-switch
  :argument "--push-option=force-push=true"
  :description "Force-push")

(transient-define-infix agitjo-title-option ()
  "Title of pull request.

Leave empty to use the first line of the first new Git commit."
  :class 'transient-option
  :argument "--push-option=title="
  :description "Title")

(transient-define-infix agitjo-topic-variable ()
  "Topic of the pull request.

This is an identifier string that controls which pull request is being
interacted with.  If not specified, the pull request's source branch
will be used as the topic."
  :class 'agitjo--topic-variable-infix
  :description "Session/topic")

;;;; Transient prefixes.

;;;;; Auxiliary.

(defun agitjo-push--pullreq-current-description ()
  "Return description for group of commands that make PRs from current branch."
  (if-let* ((branch (magit-get-current-branch)))
      (format (propertize "Push PR from %s to" 'face 'transient-heading)
              (propertize branch 'face 'magit-branch-local))
    "Push PR from <no current branch> to"))

;;;;; Definitions.

(transient-define-prefix agitjo-push ()
  "Push to a Forgejo-based repository, using AGit-Flow."
  ["Arguments"
   ("-f" agitjo-force-push-switch)
   ("-t" agitjo-title-option)
   ("-s" agitjo-topic-variable)]
  [ :inapt-if-not magit-get-current-branch
    :description agitjo-push--pullreq-current-description
    ("u" agitjo-push-pullreq-current-to-upstream)
    ("e" agitjo-push-pullreq-current)]
  ["Push PR from"
   ("l" agitjo-push-pullreq-local-branch)
   ("r" agitjo-push-pullreq-local-branch-or-ref)]
  ["Configure"
   ("C" "Set variables..." magit-branch-configure)])

;;; Provide library.
(provide 'agitjo)
;;; agitjo.el ends here
