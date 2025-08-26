;;; agitjo.el --- Manage Forgejo PRs with AGit-Flow  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Alvin Hsu

;; Author: Alvin Hsu <aurtzy@gmail.com>
;; Homepage: https://github.com/halvin/agitjo
;; Keywords: convenience, vc, tools

;; Package-Version: 0.0.0
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
;; AGit-Flow, specifically for Forgejo-based repositories like Codeberg.  This
;; package aims to implement facilities that make the AGit workflow more
;; convenient for users.

;; Some integration with Magit is also included.  It does not make use of the
;; Forgejo API.

;;; Code:

;;; Initial Code.

(require 'magit)
(require 'markdown-mode)
(require 'transient)

(keymap-set magit-status-mode-map "#" #'agitjo-push)

(transient-append-suffix 'magit-dispatch "!"
  '("#" "AGit-Flow Push" agitjo-push))


;;; Modes.

;;;; Auxiliary.

(defvar agitjo-post--buffer-name "*AGitjo-post*")

(defvar-local agitjo-post--on-confirm-function nil
  "Function to call when `agitjo-post-confirm' is invoked.")
(defvar-local agitjo-post--push-args nil
  "Buffer-local storage for transient arguments to eventually pass to \"git push\".")

;; TODO: Back the buffer by a file, so it can have a chance of recovery in case
;; of some interruption.
(defun agitjo-post--setup-buffer (on-confirm-fun args)
  "Set up buffer for editing pull request posts.

ON-CONFIRM-FUN is the function that will be called when
`agitjo-post-confirm' is invoked.

ARGS is a list of transient arguments."
  (let* ((buffer (get-buffer-create agitjo-post--buffer-name)))
    (with-current-buffer buffer
      (agitjo-post-mode)
      (setq agitjo-post--on-confirm-function on-confirm-fun
            agitjo-post--push-args args))
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
  (funcall agitjo-post--on-confirm-function))

;;;; Transient suffixes.

;;;;; Auxiliary.

(defun agitjo--confirm-push-current-for-upstream ()
  "Confirm the action to push current branch for a pull request.

Use the buffer's contents as a description for the PR."
  (with-current-buffer agitjo-post--buffer-name
    (agitjo--push-current-for-upstream
     `(,(concat "--push-option=description=" (buffer-string))
       ,agitjo-post--push-args))
    (quit-window :kill (get-buffer-window))))

(defun agitjo--push-current-for-upstream (args)
  "Create the pull request upstream.

ARGS specifies the transient arguments to be passed to \"git push\"."
  (let* ((remote (magit-get-upstream-remote))
         (target-branch (magit-get-local-upstream-branch))
         (refspec (format "HEAD:refs/for/%s" target-branch)))
    (magit-run-git-async "push" "-v" remote refspec args)))

;;;;; Definitions.

(transient-define-suffix agitjo-push-current-for-upstream (args)
  :inapt-if-not (lambda () (and (magit-get-current-branch)
                                (magit-get-upstream-branch)))
  :description (lambda ()
                 (if-let* ((branch (magit-get-upstream-branch)))
                     branch
                   "(no upstream set)"))
  (interactive (list (transient-args 'agitjo-push)))
  (let ((force-push? (transient-arg-value "--push-option=force-push=true" args)))
    (if force-push?
        (agitjo--push-current-for-upstream args)
      (agitjo-post--setup-buffer
       #'agitjo--confirm-push-current-for-upstream args))))

;;;; Transient infixes.

;;;;; Auxiliary.

(defun agitjo--read-topic-string (&rest _args)
  "Read and return the session identifier to use."
  (let ((branch (magit-get-current-branch)))
    (read-string
     (concat "Topic (default: " branch  "): ")
     nil 'agitjo--topic-history branch)))

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
  :argument "--push-option=title="
  :unsavable t)

(transient-define-infix agitjo-topic-option ()
  "The topic of this pull request.

This is an identifier string that controls which pull request is being
interacted with."
  :class 'transient-option
  :argument "--push-option=topic="
  :reader #'agitjo--read-topic-string
  :init-value (lambda (obj) (oset obj value (transient-scope)))
  :always-read t
  :unsavable t)

;;;; Transient prefixes.

;;;;; Definitions.

(transient-define-prefix agitjo-push ()
  "Push to a Forgejo-based repository, using AGit-Flow.

Initially prompt for a topic.  This string identifies the pull request
that will be created or pushed to."
  :value '("--push-option=force-push=true")
  ["Arguments"
   ("-f" "Force-push existing PR" agitjo-force-push-switch)
   ("-t" "Title" agitjo-title-option)
   ("-T" "Topic (session)" agitjo-topic-option)]
  [ :inapt-if-not magit-get-current-branch
    :description (lambda ()
                   (if-let* ((branch (magit-get-current-branch)))
                       (format (propertize "Push %s pull request for"
                                           'face 'transient-heading)
                               (propertize branch 'face 'magit-branch-local))
                     "Push <no branch set> ..."))
    ("u" agitjo-push-current-for-upstream)]
  ["Configure"
   ("C" "Set variables..." magit-branch-configure)]
  (interactive)
  (transient-setup
   'agitjo-push nil nil
   ;; The scope (at least for now) is the session identifier.
   :scope (agitjo--read-topic-string)))

(transient-augment-suffix agitjo-push
  :inapt-if-not #'magit-get-current-branch)

;;; Provide library.
(provide 'agitjo)
;;; agitjo.el ends here
